import           Control.Monad
import qualified Data.Map                     as M
import           Data.Monoid
import           Data.Time
import           Data.Time.LocalTime          ()
import           Graphics.X11.Xlib
import           System.Directory
import           Text.Regex.Posix
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.LayoutModifier
import           XMonad.StackSet
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WorkspaceCompare

main :: IO ()
main = xmonad =<< statusBar "xmobar" myPP hideStatusBar myConfig

myConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Full (Choose Tall (Mirror Tall))))
myConfig = XConfig
  { XMonad.normalBorderColor  = "#000000"
  , XMonad.focusedBorderColor = "#000000"
  , XMonad.terminal           = "lilyterm"
  , XMonad.layoutHook         = myLayoutHook
  , XMonad.manageHook         = myManageHook
  , XMonad.handleEventHook    = const $ return (All True)
  , XMonad.workspaces         = ["main","mikutter"]
  , XMonad.modMask            = hyModMask
  , XMonad.keys               = myKeys
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

hyModMask :: KeyMask
hyModMask = mod4Mask

hideStatusBar :: XConfig t -> (KeyMask, KeySym)
hideStatusBar _ = (hyModMask, xK_u)

myLayoutHook :: XMonad.Layout.LayoutModifier.ModifiedLayout AvoidStruts (Choose Full (Choose Tall (Mirror Tall))) a
myLayoutHook = avoidStruts $ Full ||| tiled ||| Mirror tiled
  where
     tiled   = Tall nmaster delta ratio -- default tiling algorithm partitions the screen into two panes
     nmaster = 0 -- The default number of windows in the master pane
     ratio   = 1 / 2 -- Default proportion of screen occupied by master pane
     delta   = 3 / 100 -- Percent of screen to increment by when resizing panes

-- apply from backward
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
   [
     -- Gimp bug
     className =? "Gimp" <&&> isDialog --> doFloat,

     return True --> (ask >>= doF . sink),

     className =? "Mikutter.rb" --> doShift "mikutter",
     return True                --> doShift "main"
   ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList
    -- launching and killing programs
  [ ((hyModMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
  , ((hyModMask,               xK_q     ), kill) -- %! Close the focused window
  , ((hyModMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
  , ((hyModMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    -- move focus up or down the window stack
  , ((hyModMask,               xK_Tab   ), windows focusDown) -- %! Move focus to the next window
  , ((hyModMask .|. shiftMask, xK_Tab   ), windows focusUp  ) -- %! Move focus to the previous window
    -- modifying the window order
  , ((hyModMask,               xK_Return), windows swapMaster) -- %! Swap the focused window and the master window
  , ((hyModMask,               xK_j     ), windows swapDown  ) -- %! Swap the focused window with the next window
  , ((hyModMask,               xK_k     ), windows swapUp    ) -- %! Swap the focused window with the previous window
    -- resizing the master/slave ratio
  , ((hyModMask .|. shiftMask, xK_j     ), sendMessage Expand) -- %! Expand the master area
  , ((hyModMask .|. shiftMask, xK_k     ), sendMessage Shrink) -- %! Shrink the master area
    -- floating layer support
  , ((hyModMask,               xK_f     ), withFocused $ windows . sink) -- %! Push window back into tiling
  , ((hyModMask .|. shiftMask, xK_f     ), withFocused   XMonad.float)   -- %! windows to float
    -- increase or decrease number of windows in the master area
  , ((hyModMask,               xK_comma ), sendMessage (IncMasterN 1))    -- %! Increment the number of windows in the master area
  , ((hyModMask,               xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    -- quit, or restart
  , ((hyModMask .|. shiftMask, xK_r     ), xmonadRestart)
    -- screenShot
  , ((noModMask,               xK_Print ), withFocused $ takeScreenShot . Just)
  , ((noModMask .|. shiftMask, xK_Print ), takeScreenShot Nothing)
    -- move to application
  , ((hyModMask, xK_b), runOrRaise "keepassx"         (className =? "Keepassx"))
  , ((hyModMask, xK_c), runOrRaise "chromium-browser" (className =? "Chromium-browser"))
  , ((hyModMask, xK_g), runOrRaise "gimp"             (className =? "Gimp"))
  , ((hyModMask, xK_h), runOrRaise "firefox"          (className =? "Firefox"))
  , ((hyModMask, xK_l), runOrRaise "libreoffice"      (className ~? "libreoffice"))
  , ((hyModMask, xK_m), runOrRaise "thunderbird"      (className =? "Thunderbird"))
  , ((hyModMask, xK_n), runOrRaise "lilyterm"         (className =? "Lilyterm"))
  , ((hyModMask, xK_r), runOrRaise "rhythmbox"        (className =? "Rhythmbox"))
  , ((hyModMask, xK_s), runOrRaise "emacs"            (className =? "Emacs"))
  , ((hyModMask, xK_t), runOrRaise "mikutter"         (className =? "Mikutter.rb"))
  , ((hyModMask, xK_v), runOrRaise "viewnior"         (className =? "Viewnior"))
  , ((hyModMask, xK_w), runOrRaise "inkscape"         (className =? "Inkscape"))
  , ((hyModMask, xK_z), runOrRaise "evince"           (className =? "Evince"))
  ]

(~?) :: Query String -> String -> Query Bool
a ~? b = fmap (=~ b) a

xmonadRestart :: X ()
xmonadRestart = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

takeScreenShot :: Maybe Window -> X ()
takeScreenShot mw = do
    home <- liftIO getHomeDirectory
    time <- liftIO localDayTimeNumber
    safeSpawn "import" $ maybe [] (\w -> ["-window", show w]) mw ++ [home ++ "/Downloads/screenshot" ++ time ++ ".png"]

localDayTimeNumber :: IO String
localDayTimeNumber = liftM ((\x -> show (localDay x) ++ "_" ++ map toSafeChar (show (localTimeOfDay x))) . zonedTimeToLocalTime) getZonedTime

toSafeChar :: Char -> Char
toSafeChar ':' = '-'
toSafeChar  x  = x

startUp :: X ()
startUp = spawnOnce     "trayer --edge top --align left --widthtype pixel --width 100 --heighttype pixel --height 16" >>
          safeSpawn     "ibus-daemon" ["--replace", "--xim"] >>
          safeSpawnProg "dropbox"
