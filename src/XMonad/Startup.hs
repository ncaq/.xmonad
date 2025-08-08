module XMonad.Startup (myStartupHook) where

import           ByDpi
import           HostChassis
import           System.Environment
import           XMonad
import           XMonad.Layout.IndependentScreens
import           XMonad.TouchPad

myStartupHook :: X ()
myStartupHook = do
  liftIO $ setEnv "_JAVA_AWT_WM_NONREPARENTING" "1"
  -- 各デバイス向け設定。
  hostChassis <- getHostChassisXMonad
  case hostChassis of
    HostChassisDesktop -> myStartupHookDesktop
    HostChassisLaptop  -> myStartupHookLaptop
    _                  -> return ()
  barHeight <- liftIO getBarHeight
  spawn $ unwords
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

-- | デスクトップ環境での初期設定。
myStartupHookDesktop :: X ()
myStartupHookDesktop = do
  -- Thunderbirdの未読メール通知アイコンを表示するプログラムなのですが、
  -- やたらとCPUとメモリを食います。
  -- CPUコアなんて基本的に余っているので、
  -- デスクトップPCでは別に無害なのですが、
  -- ラップトップPCではバッテリーを気にする必要があるので、
  -- デスクトップPCでのみ有効にすることにします。
  spawn "birdtray"

-- | ラップトップ環境での初期設定。
myStartupHookLaptop :: X ()
myStartupHookLaptop = disableTouchPad
