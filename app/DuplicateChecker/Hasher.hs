module DuplicateChecker.Hasher
  ( hashFile
  , hashFileSparse
  ) where

import Control.Exception (try, SomeException)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import System.IO

blocksize :: Int
blocksize = 8192

skipBlocks :: Int
skipBlocks = 1000

offsetBasis :: Word64
offsetBasis = 14695981039346656037

prime :: Word64
prime = 1099511628211

fnv1aStep :: Word64 -> Word64 -> Word64
fnv1aStep h b = (h `xor` b) * prime

fnv1aBytes :: Word64 -> ByteString -> Word64
fnv1aBytes = BS.foldl' (\h b -> fnv1aStep h (fromIntegral b))

hashFile :: FilePath -> IO (Either String Word64)
hashFile path = do
  result <- try go :: IO (Either SomeException Word64)
  case result of
    Left ex -> return $ Left (show ex)
    Right h  -> return $ Right h
  where
    go = withFile path ReadMode $ \hdl -> do
      let loop h = do
            chunk <- BS.hGet hdl blocksize
            if BS.null chunk
              then return h
              else loop (fnv1aBytes h chunk)
      loop offsetBasis

hashFileSparse :: FilePath -> Integer -> IO (Either String Word64)
hashFileSparse path _ = do
  result <- try go :: IO (Either SomeException Word64)
  case result of
    Left ex -> return $ Left (show ex)
    Right h  -> return $ Right h
  where
    go = withFile path ReadMode $ \hdl -> do
      let skipDist = fromIntegral (skipBlocks * blocksize)
          loop h = do
            chunk <- BS.hGet hdl blocksize
            if BS.null chunk
              then return h
              else do
                hSeek hdl RelativeSeek skipDist
                loop (fnv1aBytes h chunk)
      loop offsetBasis
