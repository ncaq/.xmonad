module XMonad.Startup (myStartupHook) where

import           ByDpi
import           HostChassis
import           Network.HostName
import           System.Directory
import           System.Environment
import           XMonad
import           XMonad.Layout.IndependentScreens
import           XMonad.Prelude
import           XMonad.TouchPad

myStartupHook :: X ()
myStartupHook = do
  liftIO $ do
    -- xmonadを使っている時に使いたいスクリプトたちにPATHを通します。
    originalPath <- getEnv "PATH"
    home <- getHomeDirectory
    let xmonadBin = home <> "/.xmonad/bin"
    setEnv "PATH" $ xmonadBin <> ":" <> originalPath
    setEnv "GLFW_IM_MODULE" "ibus"
    setEnv "GTK_IM_MODULE" "ibus"
    setEnv "QT_IM_MODULE" "ibus"
    setEnv "XMODIFIERS" "@im=ibus"
    setEnv "_JAVA_AWT_WM_NONREPARENTING" "1"
  loadXresources
  -- 各デバイス向け設定。
  hostChassis <- getHostChassisXMonad
  case hostChassis of
    HostChassisDesktop -> myStartupHookDesktop
    HostChassisLaptop  -> myStartupHookLaptop
    _                  -> return ()
  hostName <- liftIO getHostName
  case hostName of
    "indigo" -> myStartupHookIndigo
    _        -> return ()
  setDpms
  barHeight <- liftIO getBarHeight
  spawn $ unwords
    [ "trayer-srg"
    , "--edge top"
    , "--align right"
    , "--widthtype percent"
    , "--width 10"
    , "--heighttype pixel"
    , "--height " <> show barHeight
    , "--monitor primary"
    ]
  spawn "copyq"
  spawn "ibus-daemon --xim --replace"
  spawn "nm-applet"
  spawn "systemctl --user restart xkeysnail"

-- | 必要なコマンドとファイルが揃っている場合、
-- `xrdb ~/.Xresources`を実行します。
-- 主にDPIの設定に使われます。
loadXresources :: X ()
loadXresources = do
  xrdbExecutable <- liftIO findXrdbExecutable
  xresourcesExist <- liftIO doesXresourcesExist
  when (xrdbExecutable && xresourcesExist) $
    spawn "xrdb ~/.Xresources"
  where findXrdbExecutable = isJust <$> findExecutable "xrdb"
        doesXresourcesExist = getHomeDirectory >>= \home -> doesFileExist $ home <> "/.Xresources"

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
myStartupHookLaptop = do
  disableTouchPad
  screensAmount <- countScreens :: MonadIO m => m Int
  case screensAmount of
    2 -> spawn "xrandr-laptop-2"
    3 -> spawn "xrandr-laptop-3"
    _ -> return ()

-- | indigo特有の設定。
myStartupHookIndigo :: X ()
myStartupHookIndigo = do
  -- xkbsetをウィンドウマネージャのセットアップ前に動かすと効かないようなので
  -- 対処療法として`sleep`で待機させます
  -- どのタイミングで設定可能になるのかわからないのでEvent見るわけにもいかないのでsleep
  -- 起動直後に有効になっている必要性はないので待ってもそこまで問題ではない
  -- xkbsetの後にxkeysnailを再起動しないと一部のキーシーケンスがうまく動かない
  spawn "sleep 10 && systemctl --user restart xkbset-bouncekeys"

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
