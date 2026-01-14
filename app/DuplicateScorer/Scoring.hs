module DuplicateScorer.Scoring
    ( WithReason(..)
    , ScoringFunction
    , selectBestFile
    , pathLengthScore
    , modificationTimeScore
    , pathDepthScore
    , createGoldenScore
    , createTrashScore
    ) where

import Control.Exception (catch, SomeException)
import Data.List (intersperse, partition)
import Foreign.C.Types (CTime(..))
import System.Posix.Files (getFileStatus, modificationTime)
import Text.Regex.TDFA ((=~))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- Generic type for any value with explanatory reasons
data WithReason a = WithReason
    { value :: a
    , reasons :: [String]
    } deriving (Show, Eq)

type ScoringFunction = FilePath -> IO (Integer, String)

selectBestFile :: [ScoringFunction] -> [FilePath] -> IO (WithReason FilePath, [WithReason FilePath])
selectBestFile scoreFns files = do
    let initial = map (`WithReason` []) files
    (winner, losers) <- selectBest scoreFns initial
    let finalize (WithReason f rs) = WithReason f [combineReasons rs]
    return (finalize winner, map finalize losers)
  where
    combineReasons [] = "default selection (tie-breaker)"
    combineReasons rs = unwords (intersperse "/" (reverse rs))

selectBest :: [ScoringFunction] -> [WithReason FilePath] -> IO (WithReason FilePath, [WithReason FilePath])
selectBest [] [] = error "selectBest called with empty file list"
selectBest [] (file:rest) = return (file, rest)
selectBest _ [] = error "selectBest called with empty file list"
selectBest (scoreFn:restFns) candidates = do
    scored <- mapM scoreCandidate candidates

    let maxScore = maximum $ map (\(s, _, _) -> s) scored
        (winners, losers) = partition (\(s, _, _) -> s == maxScore) scored
        addReason isWinner (_, reason, candidate) =
            let prefix = if isWinner then "" else "lost: "
            in candidate { reasons = (prefix ++ reason) : reasons candidate }

    case map (addReason True) winners of
        [singleWinner] -> return (singleWinner, map (addReason False) losers)
        multiple -> do
            (winner, tiedLosers) <- selectBest restFns multiple
            return (winner, tiedLosers ++ map (addReason False) losers)
  where
    scoreCandidate candidate = do
        (scoreVal, reason) <- scoreFn (value candidate)
        return (scoreVal, reason, candidate)

--
-- Scoring functions
pathLengthScore :: ScoringFunction
pathLengthScore path = do
    let pathLen = length path
        scoreVal = fromIntegral $ negate pathLen
    return (scoreVal, "shorter path " ++ show pathLen ++ " chars")

modificationTimeScore :: ScoringFunction
modificationTimeScore path = do
    result <- catch (do
        status <- getFileStatus path
        let (CTime time) = modificationTime status
        let utcTime = posixSecondsToUTCTime (fromIntegral time)
        let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utcTime
        return (fromIntegral time, formattedTime))
        (const (return (0, "unknown")) :: SomeException -> IO (Integer, String))
    let (scoreVal, timeStr) = result
    return (scoreVal, "file modification time " ++ timeStr)

pathDepthScore :: ScoringFunction
pathDepthScore path = do
    let depth = length $ filter (== '/') path
        scoreVal = fromIntegral $ negate depth
    return (scoreVal, show depth ++ " levels path components")

createGoldenScore :: T.Text -> ScoringFunction
createGoldenScore pattern path =
    if T.pack path =~ pattern
        then return (1000, "in golden directory " ++ T.unpack pattern)
        else return (0, "not in golden directory " ++ T.unpack pattern)

createTrashScore :: T.Text -> ScoringFunction
createTrashScore pattern path =
    if T.pack path =~ pattern
        then return (-1000, "in trash directory " ++ T.unpack pattern)
        else return (0, "not in trash directory " ++ T.unpack pattern)
