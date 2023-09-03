module XMonad.AppMain (appMain) where

import           XMonad
import           XMonad.Config
import           XMonad.Hooks.DynamicLog
import           XMonad.Pp

appMain :: IO ()
appMain = do
  myConfig <- mkMyConfig
  bar <- statusBar "xmobar" myPP (\XConfig{modMask} -> (modMask, xK_u)) myConfig
  directories <- getDirectories
  launch bar directories
