module Main (main) where

import DuplicateWeb.Server (runServer)

main :: IO ()
main = runServer 8080
