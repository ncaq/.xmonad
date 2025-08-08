module Xmobar.Config (mkConfig) where

import           ByDpi
import           Control.Exception
import           HostChassis
import           System.Directory
import           Xmobar
import qualified Data.List as L
import Control.Applicative

mkConfig :: IO Config
mkConfig = do
  (runnable, template) <- mkConfigByDevice
  dpi <- getDpi
  barHeight <- getBarHeight
  return
    defaultConfig
    { font = "monospace"
    , dpi
    , bgColor = "#000000"
    , fgColor = "#E0E0E0"
    , position = TopSize L 90 barHeight
    , lowerOnStart = False
    , commands = runnable
    , sepChar = "%"
    , template = template
    }

mkConfigByDevice :: IO ([Runnable], String)
mkConfigByDevice = do
  temp <- getTemp
  battery <- getBattery
  let runnable =
        [ Run XMonadLog
        , Run $ Cpu ["--ppad", "3"] basicRate
        , Run $ CpuFreq ["-t", "Freq: <max>GHz", "--ddigits", "2"] basicRate
        , Run $ Memory ["-t", "Mem: <used>M"] basicRate
        , Run $ Swap ["-t", "Swap: <used>M"] basicRate
        , Run $ DiskIO [("/", "IO: <read>|<write>")] ["--minwidth", "4"] basicRate
        , Run $ DynNetwork ["-t", "Net: <rx>KB|<tx>KB", "--minwidth", "2"] basicRate
        ] <>
        maybe [] (pure . fst) temp <>
        maybe [] (pure . fst) battery <>
        [ Run $ DateZone "%F%a%T" "ja_JP.utf8" "Japan" "date" 10
        ]
      tempTemplate = maybe "" ((", " <>) . snd) temp
      batteryTemplate = maybe "" ((", " <>) . snd) battery
  return
    ( runnable
    , "}%XMonadLog%{%cpu%, %cpufreq%, %memory%, %swap%, %diskio%, %dynnetwork%" <>
      tempTemplate <>
      batteryTemplate <>
      ", %date%"
    )

getTemp :: IO (Maybe (Runnable, String))
getTemp = getAmdTemp <|> getGenericTemp

getAmdTemp :: IO (Maybe (Runnable, String))
getAmdTemp = do
  k10TempExists <- doesDirectoryExist "/sys/bus/pci/drivers/k10temp"
  return $ if k10TempExists
    then Just (Run $ K10Temp "0000:00:18.3" ["-t", "Temp: <Tctl>°C"] basicRate, "%k10temp%")
    else Nothing

getGenericTemp :: IO (Maybe (Runnable, String))
getGenericTemp = do
  -- Check if any thermal zones exist
  thermalZonesExist <- doesDirectoryExist "/sys/class/thermal"
  if not thermalZonesExist
    then return Nothing
    else do
      -- Check if we have at least one thermal zone
      zones <- listDirectory "/sys/class/thermal" `catch` (\(_ :: IOError) -> return [])
      let hasZones = L.any ("thermal_zone" `L.isPrefixOf`) zones
      return $ if hasZones
        then Just (Run $ MultiCoreTemp ["-t", "Temp: <max>°C"] basicRate, "%multicoretemp%")
        else Nothing

getBattery :: IO (Maybe (Runnable, String))
getBattery = do
  hostChassis <- getHostChassisNormal
  return $ if hostChassis == HostChassisLaptop
    then Just (Run $ Battery ["-t", "Bat: <acstatus> <left>%"] basicRate, "%battery%")
    else Nothing

-- | 基本的な更新頻度。
basicRate :: Rate
basicRate = 300
