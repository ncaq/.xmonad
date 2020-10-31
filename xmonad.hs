{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict                  as M
import           Data.Time.Format
import           Data.Time.LocalTime
import           Network.HostName                 (getHostName)
import           System.Directory                 (getHomeDirectory)
import           System.Environment               (setEnv)
import           System.Exit
import           Text.Regex.TDFA                  ((=~))
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.Spiral
import           XMonad.StackSet
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

main :: IO ()
main = statusBar "xmobar" myPP (\XConfig{modMask} -> (modMask, xK_u)) myConfig >>= launch

myConfig :: XConfig (Choose Full (Choose (Mirror Tall) SpiralWithDir))
myConfig = ewmh $ docks $ def
  { terminal = "lilyterm"
  , layoutHook = Full ||| Mirror (Tall 0 (3 / 100) 1) ||| spiral (4 / 3)
  , manageHook = myManageHook
  , handleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook
  , modMask = mod4Mask
  , XMonad.keys = myKeys
  , borderWidth = 0
  , startupHook = myStartupHook
  , focusFollowsMouse = False
  }

myPP :: PP
myPP = xmobarPP{ppTitle = id}

myManageHook :: ManageHook
myManageHook = composeAll
  [ isDialog                   --> doFullFloat
  , className =? "Mikutter.rb" --> doShift "2"
  , className =? "LilyTerm"    --> doShift "3"
  , return True                --> doShift "1"
  ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig{modMask} = mkKeymap conf
  [ ("M-q", kill)
  , ("M-S-q", io exitSuccess)
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
  , ("<Pause>", toggleTouchPad)   -- 本当はT-Padキーに割り当てたかったのですがxmodmapを使っても認識できなかった
  , ("M-l", spawn "dm-tool lock") -- M-lにロックを割り当てて置かないとファンクションキーも動かなくなる
  -- move to application
  , ("M-o",   runOrRaiseNext "libreoffice"             (className ~? "libreoffice"))
  , ("M-i",   runOrRaiseNext "inkscape"                (className =? "Inkscape"))
  , ("M-S-i", runOrRaiseNext "idea-community"          (className ~? "jetbrains-idea"))

  , ("M-p",   runOrRaiseNext "pavucontrol"             (className ~? "Pavucontrol"))
  , ("M-S-p", runOrRaiseNext "skypeforlinux"           (className =? "Skype"))
  , ("M-y",   runOrRaiseNext "rhythmbox"               (className =? "Rhythmbox"))

  , ("M-x",   runOrRaiseNext "steam"                   (className =? "Steam"))

  , ("M-d",   runOrRaiseNext "discord"                 (className =? "discord"))
  , ("M-S-d", runOrRaiseNext "jd.sh"                   (className =? "Jdim"))
  , ("M-h",   runOrRaiseNext "firefox"                 (className =? "Firefox"))
  , ("M-t",   runOrRaiseNext "lilyterm"                (className =? "LilyTerm"))
  , ("M-n",   runOrRaiseNext "emacs"                   (className =? "Emacs"))
  , ("M-s",   runOrRaiseNext "mikutter"                (className =? "Mikutter.rb"))
  , ("M--",   runOrRaiseNext "slack"                   (className =? "Slack"))

  , ("M-f",   runOrRaiseNext "nautilus"                (className =? "Org.gnome.Nautilus"))
  , ("M-g",   runOrRaiseNext "gimp"                    (className =? "Gimp"))
  , ("M-c",   runOrRaiseNext "chromium"                (className =? "Chromium-browser-chromium"))
  , ("M-r",   runOrRaiseNext "evince"                  (className =? "Evince"))

  , ("M-b",   runOrRaiseNext "keepassxc"               (className =? "KeePassXC"))
  , ("M-m",   runOrRaiseNext "thunderbird"             (className =? "Thunderbird"))
  , ("M-S-m", runOrRaiseNext "smplayer"                (className =? "smplayer"))
  , ("M-v",   runOrRaiseNext "virtualbox"              (className =? "VirtualBox Machine" <||> className =? "VirtualBox Manager"))
  , ("M-S-v", runOrRaiseNext "vlc"                     (className =? "vlc"))
  , ("M-w",   runOrRaiseNext "eog"                     (className =? "Eog"))
  , ("M-z",   runOrRaiseNext ".xmonad/copyq-show"      (className =? "copyq"))
  , ("M-S-z", runOrRaiseNext ".xmonad/zoom-workaround" (className =? "zoom"))
  ]
  <>
  M.fromList
  [ ((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(greedyView, shiftMask), (shift, 0)]
  ]

-- | 正規表現でクラスネームをマッチさせる
-- LibreOfficeはパターンが多いので
(~?) :: Query String -> String -> Query Bool
a ~? b = fmap (=~ b) a

-- | スクリーンショットを取得
takeScreenshot :: X ()
takeScreenshot = do
  home <- liftIO getHomeDirectory
  time <- liftIO $ formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> getZonedTime
  let path = concat [home, "/Pictures/", "screenshot-", time, ".png"]
  spawn $ concat
    ["import ", path, " && "
    , ".xmonad/recent-add-item.py ", path, " && "
    , "oxipng --strip safe ", path
    ]

-- | 使用しているタッチパッドの名前
-- 現在使っているラップトップがAlienware m17しかないので決め打ちになってます
touchPadName :: String
touchPadName = "SynPS/2 Synaptics TouchPad"

-- | タッチパッドが有効なのか無効なのか判定する
-- 存在しない場合も意味的には無効扱いする
getTouchPadEnable :: X Bool
getTouchPadEnable = do
  mDeviceEnabledLine <-
    find ("\tDevice Enabled" `isPrefixOf`) . lines <$>
    runProcessWithInput "xinput" ["list-props", touchPadName] ""
  return $ case mDeviceEnabledLine of
    Nothing                -> False
    Just deviceEnabledLine -> last deviceEnabledLine == '1'

-- | タッチパッドの有効無効をトグルする
toggleTouchPad :: X ()
toggleTouchPad = do
  touchPadEnable <- getTouchPadEnable
  let command = if touchPadEnable
        then "disable"
        else "enable"
  safeSpawn "xinput" [command, touchPadName]

myStartupHook :: X ()
myStartupHook = do
  liftIO $ do
    setEnv "GTK_IM_MODULE" "ibus"
    setEnv "QT_IM_MODULE" "ibus"
    setEnv "XMODIFIERS" "@im=ibus"
    setEnv "_JAVA_AWT_WM_NONREPARENTING" "1"
    setEnv "PULSE_LATENCY_MSEC" "90" -- Discordのノイズ対策
  hostName <- liftIO getHostName
  screensAmount <- countScreens
  -- DPI設定
  when (hostName == "strawberry" || hostName == "indigo") $
    spawn "xrdb ~/.Xresources"
  -- 各マルチディスプレイ設定
  when (hostName == "strawberry" && screensAmount == (3 :: Int)) $
    spawn "xrandr --output DP-0 --primary --output DP-2 --left-of DP-0 --output HDMI-0 --right-of DP-0"
  when (hostName == "indigo") $ do
    -- 場当たり対処ですがxkbsetをウィンドウマネージャのセットアップ前に動かすと効かないようなので
    -- 対処療法として`sleep`で待機させます
    -- どのタイミングで設定可能になるのかわからないのでEvent見るわけにもいかないのでsleep
    -- 起動直後に有効になっている必要性はないので待ってもそこまで問題ではない
    -- xkbsetの後にxkeysnailを再起動しないと一部のキーシーケンスがうまく動かない
    spawn "sleep 10 && systemctl --user restart xkbset-bouncekeys"
    case screensAmount of
      2 -> spawn "xrandr --output eDP-1-1 --primary --output DP-1-1 --left-of eDP-1-1"
      3 -> spawn "xrandr --output eDP-1-1 --primary --output DP-1-1 --left-of eDP-1-1 --output DP-0 --right-of eDP-1-1"
      _ -> return ()
  when (hostName == "karen" && screensAmount == (2 :: Int)) $
    spawn "xrandr --output eDP-1 --primary --output DP-1 --above eDP-1"
  let trayerHeight = case hostName of
        "strawberry" -> "31"
        "indigo"     -> "31"
        _            -> "22"
  spawn $
    "trayer-srg --edge top --align right --widthtype percent --width 10 --heighttype pixel --height " <>
    trayerHeight <>
    " --monitor primary"
  spawn "nm-applet"
  spawn "ibus-daemon --xim --replace"
  spawn "copyq"
  spawn "kdeconnect-indicator"
  spawn "systemctl --user restart xkeysnail"
