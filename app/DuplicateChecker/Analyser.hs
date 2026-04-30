module DuplicateChecker.Analyser
  ( AnalyserConfig(..)
  , findDuplicates
  ) where

import Control.Concurrent.Async (forConcurrently)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)

import DuplicateChecker.Hasher (hashFile, hashFileSparse)
import DuplicateChecker.Stats (Stats, recordStat)

data AnalyserConfig = AnalyserConfig
  { numWorkers :: Int
  } deriving (Show)

sparseThreshold :: Integer
sparseThreshold = 65536

groupBySize :: [(FilePath, Integer)] -> Map.Map Integer [FilePath]
groupBySize = foldr (\(fp, sz) m -> Map.insertWith (++) sz [fp] m) Map.empty

groupByHash :: [(FilePath, Word64)] -> Map.Map Word64 [FilePath]
groupByHash = foldr (\(fp, h) m -> Map.insertWith (++) h [fp] m) Map.empty

hashGroup :: Stats -> [FilePath] -> Integer -> IO [(FilePath, Word64)]
hashGroup stats paths size = do
  results <- mapM hashOne paths
  return [r | Just r <- results]
  where
    hashOne fp = do
      result <- if size > sparseThreshold
                  then hashFileSparse fp size
                  else hashFile fp
      case result of
        Left _  -> recordStat stats "hash-error" >> return Nothing
        Right h -> return (Just (fp, h))

findDuplicates :: AnalyserConfig -> Stats -> [(FilePath, Integer)] -> IO [[FilePath]]
findDuplicates _ stats pairs = do
  let sizeMap = groupBySize pairs
      (singletons, candidates) = Map.partition (\fps -> length fps < 2) sizeMap
  mapM_ (\_ -> recordStat stats "skipped-unique-size") (Map.toList singletons)
  groups <- forConcurrently (Map.toList candidates) $ \(size, paths) -> do
    hashed <- hashGroup stats paths size
    let hashMap = groupByHash hashed
    return $ filter (\fps -> length fps >= 2) (Map.elems hashMap)
  return (concat groups)
