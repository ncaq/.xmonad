{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           ClassyPrelude
import           Data.Time
import           Graphics.X11.Xlib
import           System.Directory
import           System.Exit
import           Text.Regex.Posix
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.LayoutModifier
import           XMonad.StackSet
import           XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad =<< statusBar "xmobar" myPP hideStatusBar myConfig

myConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Full (Choose Tall (Mirror Tall))))
myConfig = def
    { borderWidth       = 0
    , XMonad.workspaces = ["main", "mikutter"]
    , layoutHook        = myLayoutHook
    , terminal          = "lilyterm"
    , modMask           = mod4Mask
    , XMonad.keys       = myKeys
    , startupHook       = myStartUp
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

myLayoutHook :: ModifiedLayout AvoidStruts (Choose Full (Choose Tall (Mirror Tall))) a
myLayoutHook = avoidStruts $ Full ||| tiled ||| Mirror tiled
  where tiled = Tall 0 (1 / 2) (3 / 100)

myManageHook :: ManageHook
myManageHook = composeAll
               [ manageDocks
               , isDialog                   --> doFloat
               , className =? "Mikutter.rb" --> doShift "mikutter"
               , return True                --> doShift "main"
               ]

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig{modMask}) = mapFromList $
    [ ((modMask,               xK_q     ), kill)
    , ((modMask .|. shiftMask, xK_q     ), io $ exitWith ExitSuccess)
    , ((modMask .|. shiftMask, xK_r     ), xmonadRestart)
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows focusDown)
    , ((modMask .|. shiftMask, xK_Tab   ), windows focusUp  )
    , ((modMask,               xK_j     ), windows focusDown)
    , ((modMask,               xK_k     ), windows focusUp  )
    -- modifying the window order
    , ((modMask,               xK_Return), windows swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows swapUp    )
    -- resizing the master/slave ratio
    , ((modMask,               xK_a     ), sendMessage Shrink)
    , ((modMask,               xK_e     ), sendMessage Expand)
    -- floating layer support
    , ((modMask,               xK_p     ), withFocused $ windows . sink)
    -- increase or decrease number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))
    -- toggle trackpad
    , ((noModMask,             xK_F1    ), disableTrackPad)
    , ((noModMask,             xK_F2    ), enableTrackPad)
    -- misc
    , ((noModMask,             xK_Print ), takeScreenShot)
    , ((modMask,               xK_l     ), spawn "dm-tool lock")
    -- move to application
    , ((modMask, xK_f), runOrRaiseNext "libreoffice"      (className ~? "libreoffice"))
    , ((modMask, xK_g), runOrRaiseNext "gimp"             (className =? "Gimp"))
    , ((modMask, xK_c), runOrRaiseNext "chromium-browser" (className =? "Chromium-browser-chromium"))
    , ((modMask, xK_r), runOrRaiseNext "evince"           (className =? "Evince"))
    , ((modMask, xK_d), runOrRaiseNext "slack"            (className =? "Slack"))
    , ((modMask, xK_h), runOrRaiseNext "firefox"          (className =? "Firefox"))
    , ((modMask, xK_t), runOrRaiseNext "lilyterm"         (className =? "Lilyterm"))
    , ((modMask, xK_n), runOrRaiseNext "emacs"            (className =? "Emacs"))
    , ((modMask, xK_s), runOrRaiseNext "mikutter"         (className =? "Mikutter.rb"))
    , ((modMask, xK_b), runOrRaiseNext "keepassx"         (className =? "Keepassx"))
    , ((modMask, xK_m), runOrRaiseNext "thunderbird"      (className =? "Thunderbird"))
    , ((modMask, xK_v), runOrRaiseNext "vlc"              (className =? "Vlc"))
    ]
    <>
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(greedyView, 0), (shift, shiftMask)]]

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
    time <- liftIO $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%S")) <$>
        getCurrentTime
    spawn $ concat ["import", " ", home, "/Pictures/", "screenshot-", time, ".png"]

myStartUp :: X ()
myStartUp = do
    spawnOnce "trayer --edge top --align left --margin 1820 --widthtype pixel --width 100 --heighttype pixel --height 16"
    spawnOnce "nm-applet"
    spawnOnce "ibus-daemon --xim --replace"
