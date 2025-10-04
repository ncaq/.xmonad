module XMonad.Config (mkMyConfig) where

import HostChassis
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Key
import XMonad.Layout.Spiral
import XMonad.ManageHook
import XMonad.Startup

mkMyConfig :: (MonadIO m) => m (XConfig (Choose Full (Choose (Mirror Tall) SpiralWithDir)))
mkMyConfig = do
  myManageHook <- mkMyManageHook
  hostChassis <- getHostChassisXMonad
  return $
    ewmhFullscreen $
      docks $
        def
          { terminal = "kitty"
          , layoutHook = Full ||| Mirror (Tall 0 (3 / 100) 1) ||| spiral (4 / 3)
          , manageHook = myManageHook
          , modMask = mod4Mask
          , XMonad.keys = myKeys hostChassis
          , borderWidth = 0
          , startupHook = myStartupHook
          , focusFollowsMouse = False
          }
