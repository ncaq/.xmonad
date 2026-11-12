module XMonad.Screenshot (takeScreenshot) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Convertible
import Data.Maybe
import Data.Time
import GI.GLib.Constants qualified as G
import GI.GLib.Functions qualified as G
import GI.Gtk qualified as Gtk
import GI.Gtk.Objects.RecentManager qualified as G
import System.Directory
import XMonad
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Run

-- | スクリーンショット撮影中であることを示すロック。
-- 中身が空の時は撮影中です。
-- `TMVar`はシリアライズ出来ないので、
-- xmonadの再起動を跨いでは保持されず、
-- 初回利用時に遅延生成します。
newtype ScreenshotLock = ScreenshotLock (Maybe (TMVar ()))

instance ExtensionClass ScreenshotLock where
  initialValue = ScreenshotLock Nothing

-- | スクリーンショットを取得します。
-- 撮影は別スレッドで行うため、
-- 撮影中もxmonadはイベントを処理し続けます。
-- 撮影中に再度呼び出された場合は何もしません。
-- これによりPrintキーを押しっぱなしにしてキーリピートが発生しても、
-- スクリーンショットを連続で撮ろうとしてしまうことを防ぎます。
takeScreenshot :: X ()
takeScreenshot = do
  lock <- getScreenshotLock
  acquired <- liftIO $ atomically $ isJust <$> tryTakeTMVar lock
  when acquired $ do
    _ <- liftIO $ forkFinally takeScreenshotIO $ \result -> do
      case result of
        Left e -> trace $ "takeScreenshot: " <> displayException e
        Right () -> return ()
      atomically $ putTMVar lock ()
    return ()

-- | スクリーンショットのロックを取得します。
-- まだ存在しなければ生成して保存します。
getScreenshotLock :: X (TMVar ())
getScreenshotLock = do
  ScreenshotLock mLock <- XS.get
  case mLock of
    Just lock -> return lock
    Nothing -> do
      lock <- liftIO $ newTMVarIO ()
      XS.put $ ScreenshotLock $ Just lock
      return lock

-- | 実際にスクリーンショットを撮影して保存します。
takeScreenshotIO :: IO ()
takeScreenshotIO = do
  home <- getHomeDirectory
  time <- formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> getZonedTime
  let path = concat [home, "/Pictures/", "screenshot-", time, ".png"]
  _ <- runProcessWithInput "import" [path] ""
  _ <- recentAddItem path
  _ <- runProcessWithInput "oxipng" ["--strip", "safe", path] ""
  return ()

-- | GTKの最近使ったファイルリストにファイルを追加します。
recentAddItem :: FilePath -> IO ThreadId
recentAddItem filePath = forkIO $ do
  -- `forkIO`しないとxmonad自体が終了してしまいます。
  _ <- Gtk.init Nothing -- Gtk.initしないとアプリケーション名が存在しないと言う警告が出ます
  recentManager <- G.recentManagerGetDefault
  _ <- G.recentManagerAddItem recentManager $ convert $ "file://" <> filePath
  _ <- G.idleAdd G.PRIORITY_DEFAULT_IDLE $ do
    Gtk.mainQuit
    return True
  Gtk.main
