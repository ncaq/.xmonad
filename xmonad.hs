{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           ClassyPrelude
import           Data.Time.LocalTime
import           Graphics.X11.Xlib
import           Network.HostName
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.Regex.Posix
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.IndependentScreens
import           XMonad.StackSet
import           XMonad.Util.EZConfig

main :: IO ()
main = statusBar "xmobar" myPP (\XConfig{modMask} -> (modMask, xK_u)) myConfig >>= launch

myConfig :: XConfig (Choose Full (Choose Tall (Mirror Tall)))
myConfig = ewmh $ docks $ def
  { terminal = "lilyterm"
  , layoutHook = let tall = Tall 0 (3 / 100) (1 / 2) in Full ||| tall ||| Mirror tall
  , manageHook = myManageHook
  , handleEventHook = handleEventHook def <+> fullscreenEventHook
  , modMask = mod4Mask
  , XMonad.keys = myKeys
  , borderWidth = 0
  , startupHook = myStartupHook
  , focusFollowsMouse = False
  }

myPP :: PP
myPP = def
  { ppCurrent = wrap "[" "]"
  , ppVisible = wrap "(" ")"
  , ppSep = ":"
  , ppWsSep = ""
  , ppTitle = id
  }

myManageHook :: ManageHook
myManageHook = composeAll
  [ isDialog                   --> doFloat
  , className =? "Mikutter.rb" --> doShift "2"
  , return True                --> doShift "1"
  ]

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
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
  , ("M-l", spawn "dm-tool lock")
  -- toggle trackpad
  , ("<F1>", spawn "xinput --disable 'SynPS/2 Synaptics TouchPad'")
  , ("<F2>", spawn "xinput --enable 'SynPS/2 Synaptics TouchPad'")
  -- move to application
  , ("M--", runOrRaiseNext "slack"              (className =? "Slack"))
  , ("M-b", runOrRaiseNext "keepassxc"          (className =? "keepassxc"))
  , ("M-c", runOrRaiseNext "chromium-browser"   (className =? "Chromium-browser-chromium"))
  , ("M-d", runOrRaiseNext "discord"            (className =? "discord"))
  , ("M-f", runOrRaiseNext "nautilus"           (className =? "Nautilus"))
  , ("M-g", runOrRaiseNext "gimp"               (className =? "Gimp"))
  , ("M-h", runOrRaiseNext "firefox"            (className =? "Firefox"))
  , ("M-i", runOrRaiseNext "inkscape"           (className =? "Inkscape"))
  , ("M-m", runOrRaiseNext "thunderbird"        (className =? "Thunderbird"))
  , ("M-n", runOrRaiseNext "emacs"              (className =? "Emacs"))
  , ("M-o", runOrRaiseNext "libreoffice"        (className ~? "libreoffice"))
  , ("M-p", runOrRaiseNext "skypeforlinux"      (className =? "Skype"))
  , ("M-r", runOrRaiseNext "evince"             (className =? "Evince"))
  , ("M-s", runOrRaiseNext "mikutter"           (className =? "Mikutter.rb"))
  , ("M-t", runOrRaiseNext "lilyterm"           (className =? "LilyTerm"))
  , ("M-v", runOrRaiseNext "vlc"                (className =? "vlc"))
  , ("M-w", runOrRaiseNext "eog"                (className =? "Eog"))
  , ("M-x", runOrRaiseNext "steam"              (className =? "Steam"))
  , ("M-y", runOrRaiseNext "rhythmbox"          (className =? "Rhythmbox"))
  , ("M-z", runOrRaiseNext ".xmonad/copyq-show" (className =? "copyq"))
  ]
  <>
  mapFromList
  [ ((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(greedyView, shiftMask), (shift, 0)]
  ]

(~?) :: Query String -> String -> Query Bool
a ~? b = fmap (=~ b) a

takeScreenshot :: X ()
takeScreenshot = do
  home <- liftIO getHomeDirectory
  time <- liftIO $ formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> getZonedTime
  let path = concat [home, "/Pictures/", "screenshot-", time, ".png"]
  spawn $ concat ["import ", path, " && .xmonad/recent-add-item.py ", path]

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
  when (hostName == "strawberry"  && screensAmount == (3 :: Int)) $
    spawn $ "xrandr " <>
      "--output HDMI-1 --primary --pos 3840x0 " <>
      "--output DVI-D-0 --pos 7680x1080 " <>
      "--output HDMI-0 --pos 0x0 --scale 2x2"
  when (hostName == "indigo" && screensAmount == (2 :: Int)) $
    spawn "xrandr --output DP-1-1 --output eDP-1-1 --primary --below DP-1-1"
  when (hostName == "karen" && screensAmount == (2 :: Int)) $
    spawn "xrandr --output DP-1 --output eDP-1 --primary --below DP-1"
  let trayerHeight = case hostName of
        "strawberry" -> "31"
        "indigo"     -> "31"
        "karen"      -> "22"
        _            -> "22"
  spawn $
    "trayer-srg " <>
    "--edge top --align right --widthtype percent --width 10 " <>
    "--heighttype pixel --height " <>
    trayerHeight <>
    " --monitor primary"
  spawn "nm-applet"
  spawn "ibus-daemon --xim --replace"
  spawn "copyq"
  spawn "kdeconnect-indicator"
  spawn "systemctl --user restart xkeysnail"
