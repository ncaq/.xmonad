module XMonad.Key (myKeys) where

import qualified Data.List               as L
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           HostChassis
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Screenshot
import           XMonad.StackSet
import           XMonad.TouchPad
import           XMonad.Util.EZConfig

myKeys :: HostChassis -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys hostChassis conf@XConfig{modMask} = mkKeymap conf
  [ ("M-q", kill)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
  -- move focus up or down the window stack
  , ("M-<Tab>", windows focusDown)
  , ("M-S-<Tab>", windows focusUp)
  -- modifying the window order
  , ("M-<Return>", windows swapMaster)
  , ("M-j", windows swapDown)
  , ("M-k", windows swapUp)
  -- resizing the master/slave ratio
  , ("M-a", sendMessage Shrink)
  , ("M-e", sendMessage Expand)
  -- increase or decrease number of windows in the master area
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M-.", sendMessage (IncMasterN (-1)))
  -- floating layer support
  , ("M-;", withFocused $ windows . sink)
  -- audio
  , ("<XF86AudioMute>", spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
  , ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%-")
  , ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%+")
  -- misc
  , ("<Print>", takeScreenshot)
  -- move to application
  , ("M-o",   runOrRaiseNext "libreoffice"             (className ~? "libreoffice"))
  , ("M-S-o", runOrRaiseNext "obs"                     (className ~? "obs"))
  , ("M-i",   runOrRaiseNext "inkscape"                (className =? "Inkscape"))
  , ("M-S-i", runOrRaiseNext "idea-community"          (className ~? "jetbrains-idea"))

  , ("M-p",   runOrRaiseNext "pwvucontrol"             (className ~? "pwvucontrol"))
  , ("M-y",   runOrRaiseNext "rhythmbox"               (className =? "Rhythmbox"))

  , ("M-x",   runOrRaiseNext "steam"                   (className =? "Steam"))

  , ("M-d",   runOrRaiseNext "discord"                 (className =? "discord"))
  , ("M-S-d", runOrRaiseNext "jd.sh"                   (className =? "Jdim"))
  , ("M-h",   runOrRaiseNext "firefox"                 (className ~? "firefox"))
  , ("M-t",   runOrRaiseNext "kitty"                   (className =? "kitty"))
  , ("M-n",   runOrRaiseNext "emacs"                   (className =? "Emacs"))
  , ("M-s",   runOrRaiseNext "slack"                   (className =? "Slack"))

  , ("M-f",   runOrRaiseNext "nautilus"                (className =? "org.gnome.Nautilus"))
  , ("M-g",   runOrRaiseNext "gimp"                    (className ~? "Gimp"))
  , ("M-c",   runOrRaiseNext "claude-desktop"          (className =? "Claude"))
  , ("M-S-c", runOrRaiseNext "chromium"                (className =? "Chromium-browser"))
  , ("M-r",   runOrRaiseNext "evince"                  (className =? "Evince"))

  , ("M-b",   runOrRaiseNext "keepassxc"               (className =? "KeePassXC"))
  , ("M-S-b", runOrRaiseNext "virtualbox"              (className ~? "VirtualBox"))
  , ("M-m",   runOrRaiseNext "thunderbird"             (className ~? "thunderbird"))
  , ("M-S-m", runOrRaiseNext "smplayer"                (className =? "smplayer"))
  , ("M-w",   runOrRaiseNext "eog"                     (className =? "Eog"))
  , ("M-v",   runOrRaiseNext "copyq-show"              (className =? "copyq"))
  , ("M-S-v", runOrRaiseNext "vlc"                     (className =? "vlc"))
  , ("M-z",   runOrRaiseNext "youtube-music"           (className =? "com.github.th_ch.youtube_music"))
  , ("M-S-z", runOrRaiseNext "zoom-fix-v4l"            (className =? "zoom"))
  ]
  <>
  Map.fromList
  [ ((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(greedyView, shiftMask), (shift, 0)]
  ]
  <>
  laptopKeys
  where laptopKeys = mkKeymap conf $
          if hostChassis == HostChassisLaptop
          then [ ("<XF86Calculator>", toggleTouchPad) -- 意味的に適切ではないが、適切なキーがないため代用。
               , ("M-l", spawn "dm-tool lock") -- M-lにロックを割り当てておかないとロックファンクションキーも動かなくなる。
               ]
          else []

-- | クラスネームを部分一致させる。
(~?) :: Query String -> String -> Query Bool
a ~? b = fmap (L.isInfixOf b) a
