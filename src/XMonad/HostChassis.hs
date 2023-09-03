{-# LANGUAGE StrictData #-}
module XMonad.HostChassis where

import           Data.Convertible
import           Data.Text               (Text)
import qualified Data.Text.IO            as T
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Util.Run

-- | 動いているコンピュータの種類。
data HostChassis
  = HostChassisDesktop
  | HostChassisLaptop
  | HostChassisOther Text
  deriving (Eq, Ord, Read, Show)

-- | systemdを利用してコンピュータの種類を取得します。
getHostChassis :: MonadIO m => m HostChassis
getHostChassis = do
  r <- runProcessWithInput "hostnamectl" ["chassis"] ""
  case trim r of
    "desktop" -> return HostChassisDesktop
    "laptop"  -> return HostChassisLaptop
    other     -> do
      liftIO $ T.hPutStrLn stderr $ "`getHostChassis` could not be determined: other: " <> convert other
      return $ HostChassisOther $ convert other
