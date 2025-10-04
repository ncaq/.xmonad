module XMonad.AppMain (appMain) where

import XMonad
import XMonad.Config
import XMonad.Hooks.StatusBar
import XMonad.Pp

appMain :: IO ()
appMain = do
  myConfig <- mkMyConfig
  let toggleStrutsKey XConfig{modMask} = (modMask, xK_u)
      bar = withEasySB (statusBarProp "xmobar-launch" (pure myPP)) toggleStrutsKey myConfig
  directories <- getDirectories
  launch bar directories
