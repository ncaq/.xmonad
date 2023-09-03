module XMonad.Screenshot (takeScreenshot) where

import           Control.Concurrent
import           Data.Convertible
import           Data.Time
import qualified GI.GLib.Constants            as G
import qualified GI.GLib.Functions            as G
import qualified GI.Gtk                       as Gtk
import qualified GI.Gtk.Objects.RecentManager as G
import           System.Directory
import           XMonad
import           XMonad.Util.Run

-- | スクリーンショットを取得します。
takeScreenshot :: X ()
takeScreenshot = do
  home <- liftIO getHomeDirectory
  time <- liftIO $ formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> getZonedTime
  let path = concat [home, "/Pictures/", "screenshot-", time, ".png"]
  _ <- runProcessWithInput "import" [path] ""
  _ <- liftIO $ recentAddItem path
  _ <- runProcessWithInput "oxipng" ["--strip", "safe", path] ""
  return ()

-- | GTKの最近使ったファイルリストにファイルを追加します。
recentAddItem :: FilePath -> IO ThreadId
recentAddItem filePath = forkIO $ do -- `forkIO`しないとxmonad自体が終了してしまいます。
  _ <- Gtk.init Nothing              -- Gtk.initしないとアプリケーション名が存在しないと言う警告が出ます
  recentManager <- G.recentManagerGetDefault
  _ <- G.recentManagerAddItem recentManager $ convert $ "file://" <> filePath
  _ <- G.idleAdd G.PRIORITY_DEFAULT_IDLE $ do
    Gtk.mainQuit
    return True
  Gtk.main
