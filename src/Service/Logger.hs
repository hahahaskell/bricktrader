module Service.Logger where
import Data.Text (Text)
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime, UTCTime (UTCTime))
import qualified Data.Text as Text

data Verbosity =
  Information
  | Warning
  | Error
  deriving (Enum, Bounded)

data Options = Options
  { verbosity :: Verbosity
  }

data Hooks = Hooks
  { logger :: Text -> IO ()
  }

data Service = Service
  { logInfo :: Text -> IO ()
  , logWarning :: Text -> IO ()
  , logError :: Text -> IO ()
  }

createService :: Options -> Hooks -> IO Service
createService options hooks = do

  return $ Service
    { logInfo = log_ Information options hooks
    , logWarning = log_ Warning options hooks
    , logError = log_ Error options hooks
    }

log_ :: Verbosity -> Options -> Hooks -> Text -> IO ()
log_ setVerbosity options hooks txt = do

  now <- getCurrentTime

  let toLog =
        case (options.verbosity, setVerbosity) of
          (Error, Error) -> True
          (Error, _) -> False
          (Warning, Error) -> True
          (Warning, Warning) -> True
          (Warning, _) -> False
          (Information, _) -> True

  if toLog
  then hooks.logger $ fmtDateTime now <> txt
  else pure ()

  where
    fmtDateTime :: UTCTime  -> Text
    fmtDateTime d = Text.pack $ formatTime defaultTimeLocale "%b %e %T > " d
