import Data.Map as M
import Data.Monoid
import Graphics.X11.Xlib
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.StackSet

main :: IO ()
main = xmonad =<< statusBar "xmobar" defaultPP hideStatusBar myConfig

hideStatusBar :: XConfig t -> (KeyMask, KeySym)
hideStatusBar _ = (mod4Mask, xK_F11)

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig = XConfig
  { XMonad.normalBorderColor  = "#dddddd" ,
    XMonad.focusedBorderColor = "#ff0000" ,
    XMonad.terminal           = "lilyterm" ,
    XMonad.layoutHook         = fullFirstLayout,
    XMonad.manageHook         = composeAll [] ,
    XMonad.handleEventHook    = \_ -> return (All True) ,
    XMonad.workspaces         = ["main","mikutter"] ,
    XMonad.modMask            = superKey ,
    XMonad.keys               = keyBind ,
    XMonad.mouseBindings      = mouseBindings defaultConfig,
    XMonad.borderWidth        = 0 ,
    XMonad.logHook            = return () ,
    XMonad.startupHook        = return () ,
    XMonad.focusFollowsMouse  = False ,
    XMonad.clickJustFocuses   = True
  }

fullFirstLayout :: Choose Tall (Choose (Mirror Tall) Full) a
fullFirstLayout = tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio -- default tiling algorithm partitions the screen into two panes
     nmaster = 0 -- The default number of windows in the master pane
     ratio   = 1/2 -- Default proportion of screen occupied by master pane
     delta   = 3/100 -- Percent of screen to increment by when resizing panes

superKey :: KeyMask
superKey = mod4Mask

keyBind :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keyBind conf@(XConfig {XMonad.modMask = modKey}) = M.fromList $
    -- launching and killing programs
    [ ((modKey .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modKey              , xK_q     ), kill) -- %! Close the focused window
    , ((modKey .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
    , ((modKey,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size
    , ((modKey,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    -- move focus up or down the window stack
    , ((modKey,               xK_Tab   ), windows focusDown) -- %! Move focus to the next window
    , ((modKey .|. shiftMask, xK_Tab   ), windows focusUp  ) -- %! Move focus to the previous window
    , ((modKey,               xK_j     ), windows focusDown) -- %! Move focus to the next window
    , ((modKey,               xK_k     ), windows focusUp  ) -- %! Move focus to the previous window
    , ((modKey,               xK_m     ), windows focusMaster  ) -- %! Move focus to the master window
    -- modifying the window order
    , ((modKey,               xK_Return), windows swapMaster) -- %! Swap the focused window and the master window
    , ((modKey .|. shiftMask, xK_j     ), windows swapDown  ) -- %! Swap the focused window with the next window
    , ((modKey .|. shiftMask, xK_k     ), windows swapUp    ) -- %! Swap the focused window with the previous window
    -- resizing the master/slave ratio
    , ((modKey,               xK_a     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modKey,               xK_o     ), sendMessage Expand) -- %! Expand the master area
    -- floating layer support
    , ((modKey,               xK_l     ), withFocused $ windows . sink) -- %! Push window back into tiling
    -- increase or decrease number of windows in the master area
    , ((modKey              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modKey              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    -- quit, or restart
    , ((modKey .|. shiftMask, xK_r     ), xmonadRestart)
    ]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modKey, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
        , (f, m) <- [(view, 0), (shift, shiftMask)]]
xmonadRestart :: X ()
xmonadRestart = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
