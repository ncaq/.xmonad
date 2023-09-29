module ByDpi (getDpi, getBarHeight) where

import           Network.HostName

-- | DPIを取得します。
getDpi :: IO Double
getDpi = do
  hostName <- getHostName
  return $
    if hostName == "creep"
    then 96
    else 144

-- | xmobarやtrayer-srgの高さを取得します。
-- 単位はpixelです。
getBarHeight :: IO Int
getBarHeight = do
  dpi <- getDpi
  case dpi of
    96 -> return 20
    -- 大きくて致命的に困ることはないので144含めてデフォルト値は26。
    _  -> return 26
