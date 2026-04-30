module DuplicateChecker.Stats
  ( Stats
  , newStats
  , recordStat
  , dumpStats
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')
import qualified Data.Map.Strict as Map
import System.IO (stderr, hPutStrLn)

newtype Stats = Stats (TVar (Map.Map String Int))

newStats :: IO Stats
newStats = Stats <$> newTVarIO Map.empty

recordStat :: Stats -> String -> IO ()
recordStat (Stats tvar) key =
  atomically $ modifyTVar' tvar (Map.insertWith (+) key 1)

dumpStats :: Stats -> IO ()
dumpStats (Stats tvar) = do
  m <- readTVarIO tvar
  mapM_ (\(k, v) -> hPutStrLn stderr (k ++ ": " ++ show v)) (Map.toAscList m)
