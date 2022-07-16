module Service.BookKeeper where

import Control.Concurrent (takeMVar)
import Control.Concurrent.MVar (putMVar)
import Network.Wreq.Session ( Session )
import Data.Text (Text)
import Data.List (intercalate)

-- runBookKeeper

-- recordBookerPrice :: BinanceSessionState -> [Text]
--         -> IO (Either ([Book], WeightCount) FailureMessage)
-- recordBookerPrice bSess symbols = do

--     -- maybe not worth the retry logic as it's time sensitive
--     result <- binanceRequest bSess booker

--     case result of
--       Left (b, w) -> do
--             appendFile path $ show (filter match b) ++ "\n"
--             return $ Left (b, w)
--       Right s -> return $ Right $ intercalate "\n" s

--     where
--         match p = bookSymbol p `elem` symbols
--         path = "pricebook.json"
