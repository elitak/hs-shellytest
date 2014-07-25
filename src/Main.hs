{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Prelude hiding (putStrLn)
import Data.Text.Lazy
import Data.Text.Lazy.IO
import qualified Data.Text.Internal as DTI
import Text.Shakespeare.Text

-- prepend sudo to all commands here
import Shelly hiding (run, command)
import qualified Shelly (run, command)
run cmd args = Shelly.run "sudo" (cmd:args)
command cmd args = Shelly.command "sudo" (cmd:args)

bswap16 :: Text -> Text -> Sh Text
bswap16 srcDevPath destDevName = do
    let dmsetup = command "dmsetup" ["create", toStrict destDevName]
    devSize <- run "blockdev" ["--getsize", toStrict srcDevPath]
    let mapSpec = toStrict [lt|0 #{devSize} bswap16 #{srcDevPath}|]
    setStdin mapSpec
    dmsetup [] >>= (return . fromStrict)


main = do
    listing <- shelly $ verbosely $ do
        listing <- bswap16 "/dev/sda" "testdev"
        return listing
    putStrLn listing
