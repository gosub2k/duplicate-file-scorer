module DuplicateScorer.Scoring
    ( ScoreWithReason(..)
    , FileWithReason(..)
    , ScoringFunction
    , selectBestFile
    , pathLengthScore
    , modificationTimeScore
    , pathDepthScore
    , createGoldenScore
    , createTrashScore
    ) where

import Control.Exception (catch, SomeException)
import Foreign.C.Types (CTime(..))
import System.Posix.Files (getFileStatus, modificationTime)
import Text.Regex.TDFA ((=~))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

data ScoreWithReason = ScoreWithReason 
    { score :: Integer
    , reason :: String
    , scoreData :: String
    } deriving (Show, Eq)

data FileWithReason = FileWithReason
    { reasonFilePath :: FilePath
    , whyChosen :: String
    } deriving (Show, Eq)

type ScoringFunction = FilePath -> IO ScoreWithReason

type FileWithReasons = (FilePath, [String])

selectBestFile :: [ScoringFunction] -> [FilePath] -> IO (FileWithReason, [FileWithReason])
selectBestFile scoreFns files = do
    let filesWithReasons = map (\f -> (f, [])) files
    (winner, losers) <- selectBestFile' scoreFns filesWithReasons
    let winnerFinal = FileWithReason (fst winner) (combineReasons (snd winner))
    let losersFinal = map (\(f, rs) -> FileWithReason f (combineReasons rs)) losers
    return (winnerFinal, losersFinal)
  where
    combineReasons [] = "default selection (tie-breaker)"
    combineReasons reasons = unwords (intersperse "/" (reverse reasons))
    intersperse :: a -> [a] -> [a]
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs

selectBestFile' :: [ScoringFunction] -> [FileWithReasons] -> IO (FileWithReasons, [FileWithReasons])
selectBestFile' [] [] = error "selectBestFile called with empty file list"
selectBestFile' [] (file:rest) = return (file, rest)
selectBestFile' _ [] = error "selectBestFile called with empty file list"
selectBestFile' (scoreFn:restFns) filesWithReasons = do
    scoredFiles <- mapM (\(file, reasons) -> do
        scoreWithReason <- scoreFn file
        let newReason = formatReason scoreWithReason
        return (file, reasons, scoreWithReason, newReason)) filesWithReasons

    let maxScore = maximum $ map (\(_, _, swr, _) -> score swr) scoredFiles
    let bestFiles = filter (\(_, _, swr, _) -> score swr == maxScore) scoredFiles
    let worstFiles = filter (\(_, _, swr, _) -> score swr < maxScore) scoredFiles

    -- Add winning/losing reason to each file
    let bestWithNewReason = map (\(f, rs, _, newR) -> (f, newR : rs)) bestFiles
    let worstWithNewReason = map (\(f, rs, _, newR) -> (f, ("lost: " ++ newR) : rs)) worstFiles

    case bestWithNewReason of
        [singleBest] -> return (singleBest, worstWithNewReason)
        multiple -> do
            (winner, tiedLosers) <- selectBestFile' restFns multiple
            return (winner, tiedLosers ++ worstWithNewReason)
  where
    formatReason scoreWithReason = reason scoreWithReason ++ " " ++ scoreData scoreWithReason

--
-- Scoring functions
pathLengthScore :: ScoringFunction
pathLengthScore path = do
    let pathLen = length path
    let scoreVal = fromIntegral $ negate pathLen
    return $ ScoreWithReason scoreVal "shorter path" (show pathLen ++ " chars")

modificationTimeScore :: ScoringFunction
modificationTimeScore path = do
    result <- catch (do
        status <- getFileStatus path
        let (CTime time) = modificationTime status
        let utcTime = posixSecondsToUTCTime (fromIntegral time)
        let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utcTime
        return (fromIntegral time, formattedTime)) 
        (const (return (0, "unknown")) :: SomeException -> IO (Integer, String))
    return $ ScoreWithReason (fst result) "file modification time" (snd result)

pathDepthScore :: ScoringFunction
pathDepthScore path = do
    let depth = length $ filter (== '/') path
    let scoreVal = fromIntegral $ negate depth
    return $ ScoreWithReason scoreVal (show depth ++ " levels") " path components" 

createGoldenScore :: T.Text -> ScoringFunction
createGoldenScore pattern path =
    if T.pack path =~ pattern
        then return $ ScoreWithReason 1000 "in golden directory" (T.unpack pattern)
        else return $ ScoreWithReason 0 "not in golden directory" (T.unpack pattern)

createTrashScore :: T.Text -> ScoringFunction
createTrashScore pattern path = 
    if T.pack path =~ pattern
        then return $ ScoreWithReason (-1000) "in trash directory" (T.unpack pattern)
        else return $ ScoreWithReason 0 "not in trash directory" (T.unpack pattern)
