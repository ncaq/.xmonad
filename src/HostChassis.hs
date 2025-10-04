{-# LANGUAGE StrictData #-}

module HostChassis where

import Control.Monad.IO.Class
import Data.Char
import Data.Convertible
import Data.Text (Text)
import Data.Text qualified as T
import System.Process
import XMonad.Util.Run

-- | 動いているコンピュータの種類。
data HostChassis
  = HostChassisDesktop
  | HostChassisLaptop
  | HostChassisOther Text
  deriving (Eq, Ord, Read, Show)

-- | systemdを利用してコンピュータの種類を取得します。
-- XMonad向けの関数であり、他のコンテキストから呼び出すとプロセスが残り続けるので注意してください。
getHostChassisXMonad :: (MonadIO m) => m HostChassis
getHostChassisXMonad = textToHostChassis . convert <$> runProcessWithInput "hostnamectl" ["chassis"] ""

-- | systemdを利用してコンピュータの種類を取得します。
-- XMonadから呼び出すと、特にStartupの文脈では`waitForProcess: does not exist`のエラーになります。
getHostChassisNormal :: (MonadIO m) => m HostChassis
getHostChassisNormal = textToHostChassis . convert <$> liftIO (readProcess "hostnamectl" ["chassis"] "")

-- | 文字列から`HostChassis`を推定します。
textToHostChassis :: Text -> HostChassis
textToHostChassis x = f $ trim x
 where
  f "desktop" = HostChassisDesktop
  f "laptop" = HostChassisLaptop
  f other = HostChassisOther other

-- | 文字列の両端の空白を削除します。
trim :: Text -> Text
trim = f . f
 where
  f = T.reverse . T.dropWhile isSpace
