{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Shelly
import Prelude hiding (putStrLn)
import Data.Text.IO

-- TODO windows shell commands? detect os and run the appropriate commands?
-- TODO cross-comp to windows exe?

main = do
    listing <- shelly $ silently $ do
        run_ "echo" ["hello", "world"]
        listing <- run "ls" []
        return listing
    putStrLn listing
