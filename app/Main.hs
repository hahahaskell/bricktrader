module Main where

import UI (runTui)
import qualified Data.ByteString.Lazy as BS
import Lib (BrickTraderConfig(..))
import Data.Aeson (decode)
import System.Posix.User

main :: IO ()
main = do
    username <- getEffectiveUserName
    entry <- getUserEntryForName username
    let configPath = homeDirectory entry ++ "/.bricktrader.settings.json"

    jsonConfig <- BS.readFile configPath
    let config = decode jsonConfig :: Maybe BrickTraderConfig
    let myconfig = case config of
            Just a -> a
            Nothing -> error $ "File could not be parsed. Location: '" ++ configPath ++ "'"

    runTui myconfig
