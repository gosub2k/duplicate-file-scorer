import qualified Options.Applicative as OA
import DuplicateChecker.Analyser (AnalyserConfig(..), findDuplicates)
import DuplicateChecker.FsType   (MountInfo, loadMountInfo, isBlacklistedFs)
import DuplicateChecker.Stats    (Stats, newStats, recordStat, dumpStats)
import System.Posix.Files        (getFileStatus, getSymbolicLinkStatus, fileSize,
                                  isDirectory, isSymbolicLink, isRegularFile, FileStatus)
import Control.Exception         (try, SomeException)

data Options = Options
    { jobs :: Int
    } deriving (Show, Eq)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.option OA.auto
        ( OA.long "jobs"
       <> OA.short 'j'
       <> OA.metavar "INT"
       <> OA.value 8
       <> OA.showDefault
       <> OA.help "Number of parallel workers")

optsParser :: OA.ParserInfo Options
optsParser = OA.info (parseOptions OA.<**> OA.helper)
    ( OA.fullDesc
   <> OA.progDesc "Check a list of file paths for duplicates"
   <> OA.header "duplicate-file-spot-checker - spot check files for duplicates")

processPath :: Stats -> MountInfo -> FilePath -> IO (Maybe (FilePath, Integer))
processPath stats mountInfo path = do
    linkResult <- try (getSymbolicLinkStatus path) :: IO (Either SomeException FileStatus)
    case linkResult of
        Left _ -> do
            recordStat stats "stat-error"
            return Nothing
        Right linkStatus ->
            if isSymbolicLink linkStatus
                then do
                    recordStat stats "skipped-symlink"
                    return Nothing
                else do
                    result <- try (getFileStatus path) :: IO (Either SomeException FileStatus)
                    case result of
                        Left _ -> do
                            recordStat stats "stat-error"
                            return Nothing
                        Right status -> do
                            if isDirectory status
                                then do
                                    recordStat stats "skipped-directory"
                                    return Nothing
                                else if not (isRegularFile status)
                                    then do
                                        recordStat stats "skipped-special"
                                        return Nothing
                                    else do
                                        let size = fromIntegral (fileSize status) :: Integer
                                        if size == 0
                                            then do
                                                recordStat stats "skipped-zero-size"
                                                return Nothing
                                            else if isBlacklistedFs mountInfo path
                                                then do
                                                    recordStat stats "skipped-blacklisted-fs"
                                                    return Nothing
                                                else return (Just (path, size))

printDuplicateGroup :: [FilePath] -> IO ()
printDuplicateGroup paths = do
    mapM_ putStrLn paths
    putStrLn ""

main :: IO ()
main = do
    opts <- OA.execParser optsParser

    stats <- newStats
    mountInfo <- loadMountInfo

    content <- getContents
    let allLines = lines content

    let analyserConfig = AnalyserConfig { numWorkers = jobs opts }

    results <- mapM (\ln ->
        if null ln
            then do
                recordStat stats "blank-lines"
                return Nothing
            else processPath stats mountInfo ln
        ) allLines

    let pairs = [p | Just p <- results]

    duplicateGroups <- findDuplicates analyserConfig stats pairs

    mapM_ printDuplicateGroup duplicateGroups

    dumpStats stats
