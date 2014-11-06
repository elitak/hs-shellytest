{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Prelude hiding (putStrLn, readFile)
import Data.Text
import Data.Text.Lazy.Builder (fromText)
import Text.Shakespeare.Text
import Control.Monad
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Data.ByteString.Lazy (readFile)
import Data.Maybe
import Shelly hiding (fromText)
import Text.EditDistance

-- TODO: guess and add to PATH
runBatch commands = do
    let commands' = commands ++ ["quit"]
    run_ "C:\\Games\\SteamCmd\\steamcmd.exe" [append "+" cmd | cmd <- commands']

-- TODO: store AppIds, logins, apps-to-update in a json files
data AppId = Dota2
           | ProjectZomboid
           | DivinityOriginalSin
           | Gauntlet
             deriving (Show, Eq, Bounded, Generic)

-- TODO: use template haskell and singular mapping to generate these?
instance Enum AppId where
    toEnum 570 = Dota2
    toEnum 108600 = ProjectZomboid
    toEnum 230230 = DivinityOriginalSin
    toEnum 258970 = Gauntlet
    fromEnum Gauntlet = 258970
    fromEnum Dota2 = 570
    fromEnum ProjectZomboid = 108600
    fromEnum DivinityOriginalSin = 230230

instance ToText AppId where
    toText = fromText . pack . show . fromEnum

data GameAccount = GameAccount {
    game :: Text
  , username :: Text
  , password :: Text
} deriving (Eq, Show, Generic)

instance FromJSON AppId
instance FromJSON GameAccount

main = do
    -- use something like the following samples to get nearest-match of accounts.json entries to lookup appid in appids.json
    print $ levenshteinDistance defaultEditCosts (unpack $ toLower "Age of wonders 3") (unpack $ toLower "AgeOfWondersIII")
    print $ levenshteinDistance defaultEditCosts (unpack $ toLower "aow3") (unpack $ toLower "AgeOfWondersIII")
    print $ levenshteinDistance defaultEditCosts (unpack $ toLower "AOW3") (unpack $ toLower "AgeOfWondersIII")
    -- I could easily add 1to1 rules for special chars like hypens through the
    -- edit-distance api, but its harder for mappings like III->3 roman
    -- numerals and easier to just sanitize the input by stripping most
    -- non-ascii chars
    parsed <- decode <$> readFile "accounts.json"
    guard $ isJust parsed
    shelly $ verbosely $ forM_ (fromJust parsed) $ \GameAccount{..} ->
        runBatch [ [st|login #{username} #{password}|]
                 , [st|app_update #{game}|]
                 ]
