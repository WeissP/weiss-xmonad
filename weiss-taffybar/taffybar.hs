module Main where

import Data.Default (def)
import GI.Gdk qualified as Gdk
import Paths_weiss_xmonad (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Taffybar (getTaffyFile, startTaffybar)
import System.Taffybar.Context (TaffyIO, TaffybarConfig)
import System.Taffybar.Hooks
import System.Taffybar.SimpleConfig
import Widgets

-- End widgets are packed from the right edge inward.
fullBar :: SimpleTaffyConfig
fullBar =
  defBar
    { startWidgets = [workspaces]
    , centerWidgets = [clock]
    , endWidgets = [weather, disk, memory, cpu, media]
    }

compactBar :: SimpleTaffyConfig
compactBar =
  defBar
    { startWidgets = [mediaCompact]
    , centerWidgets = [clock]
    , endWidgets = [tray, network]
    , barCssClasses = ["weiss-bar", "weiss-compact"]
    , widgetSpacing = 4
    }

-- Appearance and placement shared by both layouts.
defBar :: SimpleTaffyConfig
defBar =
  def
    { barCssClasses = ["weiss-bar"]
    , monitorsAction = useAllMonitors
    , barPosition = Top
    , barPadding = 4
    , barHeight = ExactSize 28
    , widgetSpacing = 6
    }

barConfig :: [FilePath] -> Bool -> SimpleTaffyConfig
barConfig stylesheets compact =
  (if compact then compactBar else fullBar) {cssPaths = stylesheets}

-- Use monitor geometry rather than connector names or ordering, so rotation
-- and monitor changes select the appropriate layout automatically.
configForMonitor :: [FilePath] -> Int -> TaffyIO SimpleMonitorConfig
configForMonitor stylesheets monitorNumber = do
  display <- Gdk.displayGetDefault
  monitor <- maybe (pure Nothing) (\d -> Gdk.displayGetMonitor d $ fromIntegral monitorNumber) display
  compact <- case monitor of
    Nothing -> pure True
    Just m -> do
      geometry <- Gdk.monitorGetGeometry m
      width <- Gdk.getRectangleWidth geometry
      height <- Gdk.getRectangleHeight geometry
      pure $ height > width || width < 1600
  pure $
    SimpleMonitorConfig
      (if compact then "compact" else "full")
      (barConfig stylesheets compact)

taffybarConfig :: [FilePath] -> TaffybarConfig
taffybarConfig stylesheets =
  withLogServer $
    withToggleServer $
      toTaffybarConfigPerMonitor
        (barConfig stylesheets False)
        (configForMonitor stylesheets)

main :: IO ()
main = do
  theme <- getDataFileName "weiss-taffybar/taffybar.css"
  userTheme <- getTaffyFile "taffybar.css"
  createDirectoryIfMissing True $ takeDirectory userTheme
  startTaffybar $ taffybarConfig [theme, userTheme]
