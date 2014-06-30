-- It original code copylight.
-- Copyright (c) 2007,2008 Don Stewart
-- Copyright (c) 2007,2008 Spencer Janssen
-- Copyright (c) The Xmonad Community

import Control.Monad
import Data.Monoid
import Data.Time
import Data.Time.LocalTime()
import Graphics.X11.Xlib
import Text.Regex.Posix
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.StackSet
import XMonad.Util.WorkspaceCompare
import qualified Data.Map as M

main :: IO ()
main = xmonad =<< statusBar "xmobar" myPP hideStatusBar myConfig

myConfig :: XConfig (Choose Full (Choose Tall (Mirror Tall)))
myConfig = XConfig
  { XMonad.normalBorderColor  = "#dddddd"
  , XMonad.focusedBorderColor = "#ff0000"
  , XMonad.terminal           = "lilyterm"
  , XMonad.layoutHook         = firstFullLayout
  , XMonad.manageHook         = windowMode
  , XMonad.handleEventHook    = const $ return (All True)
  , XMonad.workspaces         = ["main","mikutter"]
  , XMonad.modMask            = superKey
  , XMonad.keys               = keyBind
  , XMonad.mouseBindings      = mouseBindings defaultConfig
  , XMonad.borderWidth        = 0
  , XMonad.logHook            = return ()
  , XMonad.startupHook        = startUp
  , XMonad.focusFollowsMouse  = False
  , XMonad.clickJustFocuses   = True
  }

myPP :: PP
myPP = PP { ppCurrent         = wrap "[" "]"
          , ppVisible         = wrap "(" ")"
          , ppHidden          = id
          , ppHiddenNoWindows = id
          , ppUrgent          = id
          , ppSep             = ":"
          , ppWsSep           = ""
          , ppTitle           = id
          , ppTitleSanitize   = id
          , ppLayout          = id
          , ppOrder           = id
          , ppOutput          = putStr
          , ppSort            = getSortByIndex
          , ppExtras          = []
          }

hideStatusBar :: XConfig t -> (KeyMask, KeySym)
hideStatusBar _ = (superKey, xK_F11)

superKey :: KeyMask
superKey = mod4Mask

firstFullLayout :: Choose Full (Choose Tall (Mirror Tall)) a
firstFullLayout = Full ||| tiled ||| Mirror tiled
  where
     tiled   = Tall nmaster delta ratio -- default tiling algorithm partitions the screen into two panes
     nmaster = 0 -- The default number of windows in the master pane
     ratio   = 1/2 -- Default proportion of screen occupied by master pane
     delta   = 3/100 -- Percent of screen to increment by when resizing panes

windowMode :: Query (Endo WindowSet)
windowMode = composeAll
   [ className =? "Mikutter.rb" --> doShift "mikutter"
   , manageDocks
   ]

keyBind :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keyBind conf@(XConfig {XMonad.modMask = modKey}) = M.fromList $
    -- launching and killing programs
  [ ((modKey .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
  , ((modKey,               xK_q     ), kill) -- %! Close the focused window
  , ((modKey .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
  , ((modKey,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    -- move focus up or down the window stack
  , ((modKey,               xK_Tab   ), windows focusDown) -- %! Move focus to the next window
  , ((modKey .|. shiftMask, xK_Tab   ), windows focusUp  ) -- %! Move focus to the previous window
  , ((modKey,               xK_j     ), windows focusDown) -- %! Move focus to the next window
  , ((modKey,               xK_k     ), windows focusUp  ) -- %! Move focus to the previous window
    -- modifying the window order
  , ((modKey,               xK_Return), windows swapMaster) -- %! Swap the focused window and the master window
  , ((modKey .|. shiftMask, xK_j     ), windows swapDown  ) -- %! Swap the focused window with the next window
  , ((modKey .|. shiftMask, xK_k     ), windows swapUp    ) -- %! Swap the focused window with the previous window
    -- resizing the master/slave ratio
  , ((modKey .|. shiftMask, xK_h     ), sendMessage Shrink) -- %! Shrink the master area
  , ((modKey .|. shiftMask, xK_s     ), sendMessage Expand) -- %! Expand the master area
    -- floating layer support
  , ((modKey,               xK_l     ), withFocused $ windows . sink) -- %! Push window back into tiling
    -- increase or decrease number of windows in the master area
  , ((modKey,               xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
  , ((modKey,               xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    -- quit, or restart
  , ((modKey,               xK_r     ), xmonadRestart)

    -- screenShot
  , ((noModMask,            xK_Print ), takeScreenShot)

    -- move to application
  , ((modKey, xK_h), runOrRaise "firefox"          (className =? "Firefox"))
  , ((modKey, xK_t), runOrRaise "mikutter.rb"      (className =? "Mikutter.rb"))
  , ((modKey, xK_n), runOrRaise "lilyterm"         (className =? "Lilyterm"))
  , ((modKey, xK_s), runOrRaise "emacs"            (className =? "Emacs"))

  , ((modKey, xK_b), runOrRaise "keepassx"         (className =? "Keepassx"))
  , ((modKey, xK_c), runOrRaise "chromium-browser" (className =? "Chromium-browser"))
  , ((modKey, xK_d), runOrRaise "thunderbird"      (className =? "Thunderbird"))
  , ((modKey, xK_e), runOrRaise "evince"           (className =? "Evince"))
  , ((modKey, xK_g), runOrRaise "gimp"             (className =? "Gimp"))
  , ((modKey, xK_m), runOrRaise "rhythmbox"        (className =? "Rhythmbox"))
  , ((modKey, xK_o), runOrRaise "libreoffice"      (className ~? "libreoffice"))
  , ((modKey, xK_v), runOrRaise "inkscape"         (className =? "Inkscape"))
  , ((modKey, xK_w), runOrRaise "viewnior"         (className =? "Viewnior"))

  ]
    ++
  -- workspace
  [((m .|. modKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
        , (f, m) <- [(view, 0), (shift, shiftMask)]]

(~?)   :: Query String -> String -> Query Bool
a ~? b = fmap (=~ b) a

xmonadRestart :: X ()
xmonadRestart = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

takeScreenShot :: X ()
takeScreenShot = liftIO $ localDayTimeNumber >>= spawn . ("import -screen ~/Downloads/screenshot" ++) . (++ ".png")

localDayTimeNumber :: IO String
localDayTimeNumber = liftM ((\x -> show (localDay x) ++ "_" ++ map toSafeChar (show (localTimeOfDay x))) . zonedTimeToLocalTime) getZonedTime

toSafeChar :: Char -> Char
toSafeChar ':' = '-'
toSafeChar  x  = x

startUp :: X ()
startUp = spawn "trayer --edge top --align left --widthtype pixel --width 100 --heighttype pixel --height 16" >>
          spawn "ibus-daemon --replace --xim" >>
          spawn "dropbox"
