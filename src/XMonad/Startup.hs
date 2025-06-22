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
  setDpms
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
    ]
  spawn "copyq"
  spawn "nm-applet"

-- | デスクトップ環境での初期設定。
myStartupHookDesktop :: X ()
myStartupHookDesktop = do
  screensAmount <- countScreens :: MonadIO m => m Int
  case screensAmount of
    -- Switchなどが中央画面奪った時のモニタ環境。
    3 -> spawn "xrandr-desktop-3"
    -- 通常のフルで使えるモニタ環境。
    4 -> spawn "xrandr-desktop-4"
    -- フォールバック。(何もしない)
    _ -> return ()
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

-- | [Display Power Management Signaling - ArchWiki](https://wiki.archlinux.jp/index.php/Display_Power_Management_Signaling)
-- をコンピュータのクラスに基づいて設定します。
setDpms :: MonadIO m => m ()
setDpms = do
  hostChassis <- getHostChassisXMonad
  case hostChassis of
    -- デスクトップは画面消灯を無効にする。
    HostChassisDesktop -> spawn "dpms-off"
    -- デスクトップ以外では30分で消灯する。
    _                  -> spawn "dpms-30m"
