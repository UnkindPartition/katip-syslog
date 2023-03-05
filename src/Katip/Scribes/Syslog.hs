{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Katip.Scribes.Syslog
    ( mkSyslogScribe
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.Builder as LTB
import           Katip
import           System.Posix.Syslog
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Storable
import           Foreign.Marshal

--------------------------------------------------------------------------------
-- | A syslog `Scribe` which respects the main Katip's guidelines.
-- Returns a tuple containing the `Scribe` and a finaliser.
mkSyslogScribe :: Namespace -> Severity -> Verbosity -> IO Scribe
mkSyslogScribe ns sev verb = do
  identifier :: ForeignPtr CChar <- toCString $ T.encodeUtf8 $ T.intercalate "." (unNamespace ns)
  -- make a copy of identifier that won't be garbage-collected

  let options = [LogPID, Console]

  withForeignPtr identifier $ \ptr -> openlog ptr options User

  let
    push i@Item{..} = do
      let bs = formatItem verb i
      BSU.unsafeUseAsCStringLen bs $ \cslen ->
        syslog Nothing (toSyslogPriority _itemSeverity) cslen
    scribe = Scribe push (closelog >> finalizeForeignPtr identifier) (permitItem sev)
  return scribe

-- | Create a NULL-terminated 'CString' (wrapped in a 'ForeignPtr') from a
-- given bytestring.
toCString :: BS.ByteString -> IO (ForeignPtr CChar)
toCString bs = BSU.unsafeUseAsCStringLen bs $ \(ptr0, len) -> do
  fptr <- mallocForeignPtrBytes (len + 1)
  withForeignPtr fptr $ \ptr -> do
    copyBytes
      {- destination -} ptr
      {- source -}      ptr0
      {- size -}        len
    pokeByteOff ptr len (0::CChar)
  return fptr

formatItem :: LogItem a => Verbosity -> Item a -> BS.ByteString
formatItem verb = LBS.toStrict . LT.encodeUtf8 . LTB.toLazyText . bracketFormat
  {- colorize? -} False
  {- verbosity -} verb

--------------------------------------------------------------------------------
toSyslogPriority :: Severity -> Priority
toSyslogPriority DebugS      =  Debug
toSyslogPriority InfoS       =  Info
toSyslogPriority NoticeS     =  Notice
toSyslogPriority WarningS    =  Warning
toSyslogPriority ErrorS      =  Error
toSyslogPriority CriticalS   =  Critical
toSyslogPriority AlertS      =  Alert
toSyslogPriority EmergencyS  =  Emergency
