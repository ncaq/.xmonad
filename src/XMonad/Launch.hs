{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module XMonad.Launch (appMain) where

import           Control.Concurrent
import           Data.Convertible
import           Data.List                        (find, isPrefixOf)
import qualified Data.Map.Strict                  as M
import           Data.Text                        (Text)
import qualified Data.Text.IO                     as T
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified GI.GLib.Constants                as G
import qualified GI.GLib.Functions                as G
import qualified GI.Gtk                           as Gtk
import qualified GI.Gtk.Objects.RecentManager     as G
import           Network.HostName                 (getHostName)
import           System.Directory                 (getHomeDirectory)
import           System.Environment               (setEnv)
import           System.IO
import           Text.Regex.TDFA                  ((=~))
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers       hiding ((~?))
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.Spiral
import           XMonad.StackSet
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

-- | 使ってるコンピュータの種類。
data HostChassis
  = HostChassisDesktop
  | HostChassisLaptop
  | HostChassisOther Text
  deriving (Eq, Ord, Show, Read)

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

appMain :: IO ()
appMain = do
  myConfig <- mkMyConfig
  bar <- statusBar "xmobar" myPP (\XConfig{modMask} -> (modMask, xK_u)) myConfig
  directories <- getDirectories
  launch bar directories

mkMyConfig :: MonadIO m => m (XConfig (Choose Full (Choose (Mirror Tall) SpiralWithDir)))
mkMyConfig = do
  myManageHook <- mkMyManageHook
  hostChassis <- getHostChassis
  return $ ewmhFullscreen $ docks $ def
    { terminal = "kitty"
    , layoutHook = Full ||| Mirror (Tall 0 (3 / 100) 1) ||| spiral (4 / 3)
    , manageHook = myManageHook
    , modMask = mod4Mask
    , XMonad.keys = myKeys hostChassis
    , borderWidth = 0
    , startupHook = myStartupHook
    , focusFollowsMouse = False
    }

myPP :: PP
myPP = xmobarPP{ppTitle = id}

mkMyManageHook :: MonadIO m => m ManageHook
mkMyManageHook = do
  screensAmount <- countScreens :: MonadIO m => m Int
  return $ if screensAmount < 3
    then mySingleMonitorManageHook
    else myMultiMonitorManageHook

-- | モニタ数が2つ以下の場合に使われる `ManageHook` です。
-- マルチモニタでない部分に複数ワークスペース生成が行われた場合、
-- ワークスペース3にいるときにもワークスペース1にダイアログが表示されたりして不便なため、
-- なるべく一つしかワークスペースを使わないようにします。
mySingleMonitorManageHook :: ManageHook
mySingleMonitorManageHook = composeAll
  [ isDialog --> doFullFloat
  ]

-- | モニタ数が3つ以上の場合に使われる `ManageHook` です。
myMultiMonitorManageHook :: ManageHook
myMultiMonitorManageHook = composeAll
  [ isDialog                   --> doFullFloat
  , className =? "Mikutter.rb" --> doShift "3"
  , className =? "kitty"       --> doShift "2"
  , return True                --> doShift "1"
  ]

myKeys :: HostChassis -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys hostChassis conf@XConfig{modMask} = mkKeymap conf
  [ ("M-q", kill)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
  -- move focus up or down the window stack
  , ("M-<Tab>", windows focusDown)
  , ("M-S-<Tab>", windows focusUp)
  -- modifying the window order
  , ("M-<Return>", windows swapMaster)
  , ("M-j", windows swapDown)
  , ("M-k", windows swapUp)
  -- resizing the master/slave ratio
  , ("M-a", sendMessage Shrink)
  , ("M-e", sendMessage Expand)
  -- increase or decrease number of windows in the master area
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M-.", sendMessage (IncMasterN (-1)))
  -- floating layer support
  , ("M-;", withFocused $ windows . sink)
  -- audio
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -1%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1%")
  -- misc
  , ("<Print>", takeScreenshot)
  -- move to application
  , ("M-o",   runOrRaiseNext "libreoffice"             (className ~? "libreoffice"))
  , ("M-S-o", runOrRaiseNext "obs"                     (className ~? "obs"))
  , ("M-i",   runOrRaiseNext "inkscape"                (className =? "Inkscape"))
  , ("M-S-i", runOrRaiseNext "idea-community"          (className ~? "jetbrains-idea"))

  , ("M-p",   runOrRaiseNext "pavucontrol"             (className ~? "Pavucontrol"))
  , ("M-S-p", runOrRaiseNext "skypeforlinux"           (className =? "Skype"))
  , ("M-y",   runOrRaiseNext "rhythmbox"               (className =? "Rhythmbox"))

  , ("M-x",   runOrRaiseNext "steam"                   (className =? "Steam"))

  , ("M-d",   runOrRaiseNext "discord-fix-pulse"       (className =? "discord"))
  , ("M-S-d", runOrRaiseNext "jd.sh"                   (className =? "Jdim"))
  , ("M-h",   runOrRaiseNext "firefox"                 (className =? "firefox"))
  , ("M-t",   runOrRaiseNext "kitty"                   (className =? "kitty"))
  , ("M-n",   runOrRaiseNext "emacs"                   (className =? "Emacs"))
  , ("M-s",   runOrRaiseNext "mikutter"                (className =? "Mikutter.rb"))
  , ("M--",   runOrRaiseNext "slack"                   (className =? "Slack"))

  , ("M-f",   runOrRaiseNext "nautilus"                (className =? "Org.gnome.Nautilus"))
  , ("M-g",   runOrRaiseNext "gimp"                    (className =? "Gimp"))
  , ("M-c",   runOrRaiseNext "chromium"                (className =? "Chromium-browser-chromium"))
  , ("M-r",   runOrRaiseNext "evince"                  (className =? "Evince"))

  , ("M-b",   runOrRaiseNext "keepassxc"               (className =? "KeePassXC"))
  , ("M-S-b", runOrRaiseNext "virtualbox"              (className =? "VirtualBox Machine" <||> className =? "VirtualBox Manager"))
  , ("M-m",   runOrRaiseNext "thunderbird"             (className =? "thunderbird"))
  , ("M-S-m", runOrRaiseNext "smplayer"                (className =? "smplayer"))
  , ("M-w",   runOrRaiseNext "eog"                     (className =? "Eog"))
  , ("M-v",   runOrRaiseNext "copyq-show"              (className =? "copyq"))
  , ("M-S-v", runOrRaiseNext "vlc"                     (className =? "vlc"))
  , ("M-z",   runOrRaiseNext "zoom-fix-v4l"            (className =? "zoom"))
  ]
  <>
  M.fromList
  [ ((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(greedyView, shiftMask), (shift, 0)]
  ]
  <>
  laptopKeys
  where laptopKeys = mkKeymap conf $
          if hostChassis == HostChassisLaptop
          then [ ("<Pause>", toggleTouchPad)   -- 本当はT-Padキーに割り当てたかったのですがxmodmapを使っても認識できなかった。
               , ("M-l", spawn "dm-tool lock") -- M-lにロックを割り当てておかないとロックファンクションキーも動かなくなる。
               ]
          else []

-- | 正規表現でクラスネームをマッチさせます
-- 主にパターンが多いLibreOffice用に必要になります
(~?) :: Query String -> String -> Query Bool
a ~? b = fmap (=~ b) a

-- | スクリーンショットを取得します
takeScreenshot :: X ()
takeScreenshot = do
  home <- liftIO getHomeDirectory
  time <- liftIO $ formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> getZonedTime
  let path = concat [home, "/Pictures/", "screenshot-", time, ".png"]
  _ <- runProcessWithInput "import" [path] ""
  _ <- liftIO $ recentAddItem path
  _ <- runProcessWithInput "oxipng" ["--strip", "safe", path] ""
  return ()

-- | GTKの最近使ったファイルリストにファイルを追加します
recentAddItem :: FilePath -> IO ThreadId
recentAddItem filePath = forkIO $ do -- `forkIO`しないとxmonad自体が終了してしまいます。
  _ <- Gtk.init Nothing              -- Gtk.initしないとアプリケーション名が存在しないと言う警告が出ます
  recentManager <- G.recentManagerGetDefault
  _ <- G.recentManagerAddItem recentManager $ convert $ "file://" <> filePath
  _ <- G.idleAdd G.PRIORITY_DEFAULT_IDLE $ do
    Gtk.mainQuit
    return True
  Gtk.main

-- | 使用しているタッチパッドの名前
-- 現在使っているラップトップがAlienware m17しかないので決め打ちになってます
touchPadName :: String
touchPadName = "SynPS/2 Synaptics TouchPad"

-- | タッチパッドが有効なのか無効なのか判定します
-- 存在しない場合も意味的には無効扱いします
getTouchPadEnable :: X Bool
getTouchPadEnable = do
  mDeviceEnabledLine <-
    find ("\tDevice Enabled" `isPrefixOf`) . lines <$>
    runProcessWithInput "xinput" ["list-props", touchPadName] ""
  return $ case mDeviceEnabledLine of
    Nothing                -> False
    Just deviceEnabledLine -> last deviceEnabledLine == '1'

-- | タッチパッドの有効無効をトグルします
toggleTouchPad :: X ()
toggleTouchPad = do
  touchPadEnable <- getTouchPadEnable
  if touchPadEnable
    then disableTouchPad
    else enableTouchPad

disableTouchPad :: X ()
disableTouchPad = safeSpawn "xinput" ["disable", touchPadName]

enableTouchPad :: X ()
enableTouchPad = safeSpawn "xinput" ["enable", touchPadName]

myStartupHook :: X ()
myStartupHook = do
  liftIO $ do
    setEnv "GLFW_IM_MODULE" "ibus"
    setEnv "GTK_IM_MODULE" "ibus"
    setEnv "QT_IM_MODULE" "ibus"
    setEnv "XMODIFIERS" "@im=ibus"
    setEnv "_JAVA_AWT_WM_NONREPARENTING" "1"
  -- DPI設定。
  spawn "xrdb ~/.Xresources"
  -- 各デバイス向け設定。
  hostChassis <- getHostChassis
  case hostChassis of
    HostChassisDesktop -> myStartupHookDesktop
    HostChassisLaptop  -> myStartupHookLaptop
    _                  -> return ()
  hostName <- liftIO getHostName
  case hostName of
    "indigo" -> myStartupHookIndigo
    _        -> return ()
  setDpms
  let trayerHeight = "32"
  spawn $
    "trayer-srg --edge top --align right --widthtype percent --width 10 --heighttype pixel --height " <>
    trayerHeight <>
    " --monitor primary"
  spawn "copyq"
  spawn "ibus-daemon --xim --replace"
  spawn "nm-applet"
  spawn "birdtray"
  spawn "systemctl --user restart xkeysnail"

-- | デスクトップ環境での初期設定。
myStartupHookDesktop :: X ()
myStartupHookDesktop = do
  screensAmount <- countScreens :: MonadIO m => m Int
  case screensAmount of
    -- Switchなどが中央画面奪った時のモニタ環境。
    3 ->
      spawn "xrandr --output DP-4 --primary --output HDMI-0 --right-of DP-4 --output DP-2 --right-of HDMI-0"
    -- 通常のフルで使えるモニタ環境。
    4 ->
      spawn "xrandr --output DP-0 --primary --output DP-2 --right-of DP-0 --output DP-4 --left-of DP-0 --output HDMI-0 --above DP-0"
    -- フォールバック。(何もしない)
    _ -> return ()

-- | ラップトップ環境での初期設定。
myStartupHookLaptop :: X ()
myStartupHookLaptop = do
  disableTouchPad
  screensAmount <- countScreens :: MonadIO m => m Int
  case screensAmount of
    2 -> spawn "xrandr --output eDP-1-1 --primary --output DP-1-1 --left-of eDP-1-1"
    3 -> spawn "xrandr --output eDP-1-1 --primary --output DP-1-1 --left-of eDP-1-1 --output DP-0 --right-of eDP-1-1"
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
  hostChassis <- getHostChassis
  case hostChassis of
    -- デスクトップは画面消灯を無効にする。
    HostChassisDesktop -> spawn "xset s off -dpms"
    -- デスクトップ以外では30分で消灯するようにしておく。
    _                  -> spawn "xset dpms 1800 1800 1800"
