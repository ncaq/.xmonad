module XMonad.Pp (myPP) where

import XMonad.Hooks.DynamicLog

myPP :: PP
myPP = xmobarPP{ppTitle = id}
