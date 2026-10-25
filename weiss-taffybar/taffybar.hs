module Main where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import GI.Gdk qualified as Gdk
import Paths_weiss_xmonad (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeDirectory)
import System.Taffybar (getTaffyFile, startTaffybar)
import System.Taffybar.Context (TaffyIO, TaffybarConfig)
import System.Taffybar.Hooks
import System.Taffybar.SimpleConfig
import Widgets

-- Bar set selected by the first command-line argument.
data Profile = Mini | Desktop | Default
  deriving (Show)

parseProfile :: [String] -> Either String Profile
parseProfile args = case args of
  [] -> Right Default
  ["default"] -> Right Default
  ["desktop"] -> Right Desktop
  ["mini"] -> Right Mini
  _ -> Left "usage: taffybar [mini|desktop|default]"

-- Bars keyed by xrandr output name, matching the screens in
-- nix-config/nixos/common/autorandr.nix. Screens not listed use
-- 'profileFallback', or get no bar when it is Nothing.
-- End widgets are listed left to right; 'barFor' reverses them because
-- Taffybar packs them from the right edge inward.
profileBars :: Profile -> [(T.Text, SimpleTaffyConfig)]
profileBars profile = case profile of
  Mini ->
    [
      ( "DP-5" -- left, portrait
      , defBar
          { startWidgets = [clock]
          , endWidgets = [media, weather]
          }
      )
    ,
      ( "HDMI-1" -- center, primary
      , defBar
          { startWidgets = [workspaces]
          , centerWidgets = [clock]
          , endWidgets = [media, memory, cpu, disk, weather]
          }
      )
    ,
      ( "DP-4" -- right
      , defBar
          { startWidgets = [clock]
          , endWidgets = [network, tray]
          }
      )
    ]
  Desktop ->
    [
      ( "HDMI-A-0" -- ultrawide, primary
      , defBar
          { startWidgets = [workspaces]
          , centerWidgets = [clock]
          , endWidgets = [media, cpu, memory, disk, weather]
          }
      )
    ,
      ( "DisplayPort-2" -- right, portrait
      , defBar
          { startWidgets = [clock]
          , centerWidgets = []
          , endWidgets = [network, tray]
          }
      )
    ]
  Default -> []

profileFallback :: Profile -> Maybe SimpleTaffyConfig
profileFallback profile = case profile of
  Default ->
    Just
      defBar
        { startWidgets = [workspaces]
        , centerWidgets = [clock]
        , endWidgets = [tray, network, cpu, media]
        }
  _ -> Nothing

barFor :: Profile -> Maybe T.Text -> Maybe SimpleTaffyConfig
barFor profile output =
  leftToRight <$> ((output >>= (`lookup` profileBars profile)) <|> profileFallback profile)
  where
    leftToRight bar = bar {endWidgets = reverse $ endWidgets bar}

-- On X11, GDK reports the xrandr output name as the monitor model.
monitorOutput :: Int -> TaffyIO (Maybe T.Text)
monitorOutput monitorNumber = liftIO $ do
  display <- Gdk.displayGetDefault
  monitor <- maybe (pure Nothing) (`Gdk.displayGetMonitor` fromIntegral monitorNumber) display
  maybe (pure Nothing) Gdk.monitorGetModel monitor

-- Appearance shared by all bars.
defBar :: SimpleTaffyConfig
defBar =
  def
    { barCssClasses = ["weiss-bar"]
    , barPosition = Top
    , barPadding = 4
    , barHeight = ExactSize 28
    , widgetSpacing = 6
    }

-- The key includes the output name, so a bar is rebuilt when a different
-- screen takes its monitor number.
configForMonitor :: Profile -> Int -> TaffyIO SimpleMonitorConfig
configForMonitor profile monitorNumber = do
  output <- monitorOutput monitorNumber
  pure $
    SimpleMonitorConfig
      (T.pack (show profile <> "-") <> fromMaybe "" output)
      (fromMaybe defBar $ barFor profile output)

monitorsWithBars :: Profile -> TaffyIO [Int]
monitorsWithBars profile =
  filterM (fmap (isJust . barFor profile) . monitorOutput) =<< useAllMonitors

taffybarConfig :: Profile -> [FilePath] -> TaffybarConfig
taffybarConfig profile stylesheets =
  withLogServer $
    withToggleServer $
      toTaffybarConfigPerMonitor
        defBar
          { cssPaths = stylesheets
          , monitorsAction = monitorsWithBars profile
          }
        (configForMonitor profile)

main :: IO ()
main = do
  profile <- either die pure . parseProfile =<< getArgs
  theme <- getDataFileName "weiss-taffybar/assets/taffybar.css"
  userTheme <- getTaffyFile "taffybar.css"
  createDirectoryIfMissing True $ takeDirectory userTheme
  startTaffybar $ taffybarConfig profile [theme, userTheme]
