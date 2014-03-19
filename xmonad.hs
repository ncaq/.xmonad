import XMonad
import XMonad.Hooks.DynamicLog

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myBar :: String
myBar = "xmobar"

myPP :: PP
myPP = defaultPP

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey _ = (mod4Mask, xK_F11)

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig = defaultConfig
  {
    terminal = "lilyterm",
    modMask = mod4Mask,
    borderWidth = 0,
    focusFollowsMouse = False
  }
