module Xmobar.AppMain (appMain) where

import Xmobar
import Xmobar.Config

appMain :: IO ()
appMain = do
  config <- mkConfig
  xmobar config
