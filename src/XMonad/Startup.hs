module XMonad.Startup (myStartupHook) where

import ByDpi
import HostChassis
import System.Environment
import XMonad
import XMonad.TouchPad

myStartupHook :: X ()
myStartupHook = do
  liftIO $ setEnv "_JAVA_AWT_WM_NONREPARENTING" "1"
  -- 各デバイス向け設定。
  hostChassis <- getHostChassisXMonad
  case hostChassis of
    HostChassisLaptop -> myStartupHookLaptop
    _ -> return ()
  barHeight <- liftIO getBarHeight
  -- SNI(StatusNotifierItem)をXEmbedに変換してtrayerで表示できるようにする。
  -- trayscaleなどのSNIアプリケーション向け。
  spawn "snixembed --fork"
  spawn $
    unwords
      [ "trayer"
      , "--edge top"
      , "--align right"
      , "--widthtype percent"
      , "--width 10"
      , "--heighttype pixel"
      , "--height " <> show barHeight
      , "--monitor primary"
      , "--transparent true"
      , "--alpha 255"
      ]
  spawn "copyq"
  spawn "nm-applet"

-- | ラップトップ環境での初期設定。
myStartupHookLaptop :: X ()
myStartupHookLaptop = disableTouchPad
