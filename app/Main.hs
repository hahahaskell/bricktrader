module Main where

import qualified Run

main :: IO ()
main = do
  Run.main
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
