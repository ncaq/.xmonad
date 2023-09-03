module XMonad.AppMain (appMain) where

import           XMonad
import           XMonad.Config
import           XMonad.Hooks.DynamicLog
import           XMonad.Pp

appMain :: IO ()
appMain = do
  myConfig <- mkMyConfig
  let toggleStrutsKey = (\XConfig{modMask} -> (modMask, xK_u))
  bar <- statusBar "xmobar-launch" myPP toggleStrutsKey myConfig
  directories <- getDirectories
  launch bar directories
