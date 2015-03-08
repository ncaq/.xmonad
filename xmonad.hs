import           Control.Monad
import           Data.Bits                    ()
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
import           XMonad.Util.EZConfig
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
  , XMonad.modMask            = mod4Mask
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

hideStatusBar :: XConfig t -> (KeyMask, KeySym)
hideStatusBar conf = (modMask conf, xK_u)

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
               [ manageDocks
               , className =? "Gimp" <&&> isDialog --> doFloat

               , className =? "Mikutter.rb" --> doShift "mikutter"
               , return True                --> doShift "main"
               ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = mkKeymap conf
              [ ("M-<space>",   sendMessage NextLayout)             -- Rotate through the available layout algorithms
              , ("M-S-<space>", setLayout $ XMonad.layoutHook conf) -- Reset the layouts on the current workspace to default
              , ("M-<tab>",     windows focusDown)                  -- Move focus to the next window
              , ("M-S-<tab>",   windows focusUp)                    -- Move focus to the previous window
              , ("M-,",         sendMessage (IncMasterN 1))         -- Increment the number of windows in the master area
              , ("M-.",         sendMessage (IncMasterN (-1)))      -- Deincrement the number of windows in the master area
              , ("M-<return>",  windows swapMaster)                 -- Swap the focused window and the master window
              , ("M-j",         windows swapDown)                   -- Swap the focused window with the next window
              , ("M-k",         windows swapUp)                     -- Swap the focused window with the previous window
              , ("M-S-j",       sendMessage Expand)                 -- Expand the master area
              , ("M-S-k",       sendMessage Shrink)                 -- Shrink the master area
              , ("M-p",         withFocused $ windows . sink)       -- Push window back into tiling
              , ("M-S-p",       withFocused XMonad.float)           -- Window to float
              , ("M-S-r",       xmonadRestart)                      -- Apply setting
              , ("M-q",         kill)                               -- Close the focused window
              , ("<print>",     withFocused $ screenShot . Just)    -- ScreenShot wait
              , ("M-<print>",   screenShot Nothing)                 -- ScreenShot from focus window
                -- move to application
              , ("M-b", runOrRaise "keepassx"         (className =? "Keepassx"))
              , ("M-c", runOrRaise "chromium-browser" (className =? "Chromium-browser"))
              , ("M-f", runOrRaise "inkscape"         (className =? "Inkscape"))
              , ("M-g", runOrRaise "gimp"             (className =? "Gimp"))
              , ("M-h", runOrRaise "firefox"          (className =? "Firefox"))
              , ("M-l", runOrRaise "libreoffice"      (className ~? "Libreoffice"))
              , ("M-m", runOrRaise "thunderbird"      (className =? "Thunderbird"))
              , ("M-n", runOrRaise "emacs"            (className =? "Emacs"))
              , ("M-r", runOrRaise "rhythmbox"        (className =? "Rhythmbox"))
              , ("M-s", runOrRaise "mikutter"         (className =? "Mikutter.rb"))
              , ("M-t", runOrRaise "lilyterm"         (className =? "Lilyterm"))
              , ("M-v", runOrRaise "viewnior"         (className =? "Viewnior"))
              , ("M-z", runOrRaise "evince"           (className =? "Evince"))
              ]

(~?) :: Query String -> String -> Query Bool
a ~? b = fmap (=~ b) a

xmonadRestart :: X ()
xmonadRestart = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

screenShot :: Maybe Window -> X ()
screenShot mw = do
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
