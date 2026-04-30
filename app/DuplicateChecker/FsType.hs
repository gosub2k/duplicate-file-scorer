module DuplicateChecker.FsType
  ( MountInfo
  , loadMountInfo
  , isBlacklistedFs
  ) where

import Data.List (isPrefixOf, maximumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import System.IO.Error (catchIOError)

type MountInfo = Map FilePath String

blacklisted :: [String]
blacklisted =
  [ "tmpfs", "sysfs", "efivarfs", "devfs", "tracefs", "proc"
  , "cgroup", "cgroup2", "debugfs", "securityfs", "pstore"
  , "bpf", "autofs", "hugetlbfs", "mqueue", "fusectl", "configfs"
  ]

parseLine :: String -> Maybe (FilePath, String)
parseLine line =
  case words line of
    (_:mountPoint:fsType:_) -> Just (mountPoint, fsType)
    _                       -> Nothing

loadMountInfo :: IO MountInfo
loadMountInfo = catchIOError load (\_ -> return Map.empty)
  where
    load = do
      contents <- readFile "/proc/mounts"
      let entries = concatMap (maybe [] (:[]) . parseLine) (lines contents)
      return $ Map.fromList entries

isBlacklistedFs :: MountInfo -> FilePath -> Bool
isBlacklistedFs mountInfo filePath =
  let matches = filter (\mp -> mp `isPrefixOf` filePath) (Map.keys mountInfo)
  in case matches of
       [] -> False
       ms ->
         let best = maximumBy (comparing length) ms
         in case Map.lookup best mountInfo of
              Just fsType -> fsType `elem` blacklisted
              Nothing     -> False
