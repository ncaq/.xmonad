module Xmobar.Config (mkConfig) where

import           Network.HostName
import           System.Directory
import           Xmobar
import           XMonad.HostChassis

mkConfig :: IO Config
mkConfig = do
  (runnable, template) <- mkConfigByDevice
  dpi <- getDpi
  return
    defaultConfig
    { font = "monospace"
    , dpi
    , bgColor = "#002b36"
    , fgColor = "#93a1a1"
    , position = TopSize L 90 32
    , lowerOnStart = False
    , commands = runnable
    , sepChar = "%"
    , template = template
    }

mkConfigByDevice :: IO ([Runnable], String)
mkConfigByDevice = do
  temp <- getTemp
  hostChassis <- getHostChassis
  let (batteryRunnable, batteryTemplate) = if hostChassis == HostChassisLaptop then ([battery], ", %battery%") else ([], "")
      runnable =
        [ Run StdinReader
        , Run $ Cpu ["--ppad", "3"] 100
        , Run $ CpuFreq ["-t", "Freq: <max>GHz", "--ddigits", "2"] 100
        , Run $ Memory ["-t", "Mem: <used>M"] 100
        , Run $ Swap ["-t", "Swap: <used>M"] 100
        , Run $ DiskIO [("/", "IO: <read>|<write>")] ["--minwidth", "5"] 100
        , Run $ DynNetwork ["-t", "Net: <rx>KB|<tx>KB", "--minwidth", "6"] 100
        , temp
        ]
        <> batteryRunnable <>
        [ Run $ DateZone "%F%a%T" "ja_JP.utf8" "Japan" "date" 10
        ]
  return (runnable, "}%StdinReader%{%cpu%, %cpufreq%, %memory%, %swap%, %diskio%, %dynnetwork%" <> batteryTemplate <> ", %date%")

getTemp :: IO Runnable
getTemp = do
  k10TempExists <- doesDirectoryExist "/sys/bus/pci/drivers/k10temp"
  return $ if k10TempExists
    then amdTemp
    else genericTemp

amdTemp :: Runnable
amdTemp = Run $ K10Temp "0000:00:18.3" ["--minwidth", "3"] 100

genericTemp :: Runnable
genericTemp = Run $ MultiCoreTemp ["-t", "Temp: <max>°C", "--minwidth", "3"] 100

battery :: Runnable
battery = Run $ Battery ["-t", "Bat: <acstatus><left>%", "--minwidth", "3"] 100

getDpi :: IO Double
getDpi = do
  hostName <- getHostName
  return $
    if hostName == "creep"
    then 96
    else 144
