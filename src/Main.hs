{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Shelly
import Prelude hiding (putStrLn)
import Data.Text.IO

main = do
    listing <- shelly $ silently $ do
        run_ "echo" ["hello", "world"]
        listing <- run "ls" []
        return listing
    putStrLn listing
