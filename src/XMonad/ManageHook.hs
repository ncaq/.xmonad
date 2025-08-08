module XMonad.ManageHook (mkMyManageHook) where

import           XMonad
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.IndependentScreens

mkMyManageHook :: MonadIO m => m ManageHook
mkMyManageHook = do
  screensAmount <- countScreens :: MonadIO m => m Int
  return $ case screensAmount of
    4 -> myManageHookFor4
    _ -> myManageHookForSimple

-- | モニタ数が4つの場合に使われる`ManageHook`です。
myManageHookFor4 :: ManageHook
myManageHookFor4 = composeAll
  [ isDialog               --> doFullFloat
  , className =? "firefox" --> doShift "3"
  , className =? "kitty"   --> doShift "2"
  , return True            --> doShift "1"
  ]

-- | モニタ数が2つ以下の場合に使われる`ManageHook`です。
-- マルチモニタでない部分に複数ワークスペース生成が行われた場合、
-- ワークスペース3にいるときにもワークスペース1にダイアログが表示されたりして不便なため、
-- なるべく一つしかワークスペースを使わないようにします。
myManageHookForSimple :: ManageHook
myManageHookForSimple = composeAll
  [ isDialog --> doFullFloat
  ]
