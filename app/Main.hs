module Main where

import qualified App

main :: IO ()
main = do
  App.appMain
  return ()

  -- user <- getUserEntryForName =<< getEffectiveUserName

  -- let app = App
  --   {
  --   }

--     let configPath = homeDirectory entry ++ "/.bricktrader.settings.json"

--     jsonConfig <- BS.readFile configPath
--     let config = decode jsonConfig :: Maybe BrickTraderConfig
--     let myconfig = case config of
--             Just a -> a
--             Nothing -> error $ "File could not be parsed. Location: '" ++ configPath ++ "'"

--     runTui myconfig
