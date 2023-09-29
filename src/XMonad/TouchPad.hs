module XMonad.TouchPad (toggleTouchPad, disableTouchPad, enableTouchPad) where

import qualified Data.List       as L
import           XMonad
import           XMonad.Util.Run

-- | タッチパッドの有効無効をトグルします。
toggleTouchPad :: X ()
toggleTouchPad = do
  touchPadEnable <- getTouchPadEnable
  if touchPadEnable
    then disableTouchPad
    else enableTouchPad

disableTouchPad :: X ()
disableTouchPad = safeSpawn "xinput" ["disable", touchPadName]

enableTouchPad :: X ()
enableTouchPad = safeSpawn "xinput" ["enable", touchPadName]

-- | タッチパッドが有効なのか無効なのか判定します。
-- 存在しない場合も意味的には無効扱いします。
getTouchPadEnable :: X Bool
getTouchPadEnable = do
  mDeviceEnabledLine <-
    L.find ("\tDevice Enabled" `L.isPrefixOf`) . L.lines <$>
    runProcessWithInput "xinput" ["list-props", touchPadName] ""
  return $ case mDeviceEnabledLine of
    Nothing                -> False
    Just deviceEnabledLine -> L.last deviceEnabledLine == '1'

-- | 使用しているタッチパッドの名前。
-- 現在使っているラップトップが一つしかないので決め打ちになっています。
touchPadName :: String
touchPadName = "ELAN0688:00 04F3:320B Touchpad"
