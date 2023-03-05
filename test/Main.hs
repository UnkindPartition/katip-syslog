module Main where

import Katip
import Katip.Scribes.Syslog
import Control.Monad
import Data.String

main :: IO ()
main = do
  meta_scribe <- mkSyslogScribe "katip-syslog-test" InfoS V3
  base_log_env <- initLogEnv mempty "test"
  meta_log_env <- registerScribe "meta" meta_scribe defaultScribeSettings base_log_env

  forM_ [ DebugS .. EmergencyS ] $ \severity_threshold -> do
    runKatipContextT meta_log_env () "meta" $ logFM InfoS $ "Setting severity threshold to " <> showLS severity_threshold

    s_scribe <- mkSyslogScribe "katip-syslog-test" severity_threshold V3
    log_env <- registerScribe "s" s_scribe defaultScribeSettings base_log_env
    runKatipContextT log_env () (fromString $ "threshold=" ++ show severity_threshold) $ do
      forM_ [ DebugS .. EmergencyS ] $ \sev ->
        logFM sev (showLS sev)
    closeScribes log_env

  closeScribes meta_log_env

  return ()
