{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           ClassyPrelude
import           Graphics.X11.Xlib
import           System.Directory
import           System.Exit
import           Text.Regex.Posix
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.StackSet
import           XMonad.Util.EZConfig

main :: IO ()
main = statusBar "xmobar" myPP hideStatusBar myConfig >>= xmonad

myConfig :: XConfig (Choose Full (Choose Tall (Mirror Tall)))
myConfig = docks $ def
    { borderWidth       = 0
    , layoutHook        = myLayoutHook
    , modMask           = mod4Mask
    , XMonad.keys       = myKeys
    , manageHook        = myManageHook
    , focusFollowsMouse = False
    }

myPP :: PP
myPP = def { ppCurrent = wrap "[" "]"
           , ppVisible = wrap "(" ")"
           , ppSep     = ":"
           , ppWsSep   = ""
           }

hideStatusBar :: XConfig t -> (KeyMask, KeySym)
hideStatusBar XConfig{modMask} = (modMask, xK_u)

myLayoutHook :: Choose Full (Choose Tall (Mirror Tall)) a
myLayoutHook = Full ||| tiled ||| Mirror tiled
  where tiled = Tall 0 (1 / 2) (3 / 100)

myManageHook :: ManageHook
myManageHook = composeAll
               [ isDialog                   --> doFloat
               , className =? "Mikutter.rb" --> doShift "2"
               , return True                --> doShift "1"
               ]

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig{ modMask } = mkKeymap conf
    [ ("M-q", kill)
    , ("M-S-q", io exitSuccess)
    , ("M-S-r", xmonadRestart)
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
    -- floating layer support
    , ("M-;", withFocused $ windows . sink)
    -- increase or decrease number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    -- toggle trackpad
    , ("<KP_F1>", disableTrackPad)
    , ("<KP_F2>", enableTrackPad)
    -- misc
    , ("<Print>", takeScreenShot)
    , ("M-l", spawn "dm-tool lock")
    -- move to application
    , ("M-p", runOrRaiseNext "skypeforlinux"    (className =? "skypeforlinux"))
    , ("M-y", runOrRaiseNext "rhythmbox"        (className =? "Rhythmbox"))
    , ("M-f", runOrRaiseNext "libreoffice"      (className ~? "libreoffice"))
    , ("M-g", runOrRaiseNext "gimp"             (className =? "Gimp"))
    , ("M-c", runOrRaiseNext "chromium-browser" (className =? "Chromium-browser-chromium"))
    , ("M-r", runOrRaiseNext "evince"           (className =? "Evince"))
    , ("M-d", runOrRaiseNext "slack"            (className =? "Slack"))
    , ("M-h", runOrRaiseNext "firefox"          (className =? "Firefox"))
    , ("M-t", runOrRaiseNext "lilyterm"         (className =? "Lilyterm"))
    , ("M-n", runOrRaiseNext "emacs"            (className =? "Emacs"))
    , ("M-s", runOrRaiseNext "mikutter"         (className =? "Mikutter.rb"))
    , ("M-b", runOrRaiseNext "keepassx"         (className =? "Keepassx"))
    , ("M-m", runOrRaiseNext "thunderbird"      (className =? "Thunderbird"))
    , ("M-w", runOrRaiseNext "nautilus"         (className =? "Nautilus"))
    , ("M-v", runOrRaiseNext "vlc"              (className =? "Vlc"))
    , ("M-z", runOrRaiseNext "inkscape"         (className =? "Inkscape"))
    ]
    <>
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    mapFromList [ ((m .|. modMask, k), windows $ f i) |
                  (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
                  (f, m) <- [(greedyView, 0), (shift, shiftMask)] ]

(~?) :: Query String -> String -> Query Bool
a ~? b = fmap (=~ b) a

xmonadRestart :: X ()
xmonadRestart = spawn "stack exec -- xmonad --recompile && stack exec -- xmonad --restart"

enableTrackPad :: X ()
enableTrackPad = spawn "xinput --enable 'SynPS/2 Synaptics TouchPad'"

disableTrackPad :: X ()
disableTrackPad = spawn "xinput --disable 'SynPS/2 Synaptics TouchPad'"

takeScreenShot :: X ()
takeScreenShot = do
    home <- liftIO getHomeDirectory
    time <- liftIO $ formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> getCurrentTime
    let path = concat [home, "/Pictures/", "screenshot-", time, ".png"]
    spawn $ concat ["import", " " , path, " && eog ", path]
