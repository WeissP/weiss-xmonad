module Widgets (
  workspaces,
  workspacesCompact,
  cpu,
  cpuCompact,
  memory,
  memoryCompact,
  disk,
  clock,
  clockCompact,
  media,
  mediaCompact,
  network,
  weather,
  tray,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (evaluate, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as LazyBytes
import Data.Char (isDigit)
import Data.Default (def)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import GI.GLib (markupEscapeText)
import GI.GdkPixbuf qualified as Pixbuf
import GI.Gtk qualified as Gtk
import Network.HTTP.Client (HttpException, Request (checkResponse), httpLbs, newManager, parseRequest, responseBody, throwErrorStatusCodes)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import StatusNotifier.Host.Service qualified as SNI
import StatusNotifier.Tray qualified as Tray
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Hooks (NetworkInfoChan (..), getNetworkChan)
import System.Taffybar.Information.MPRIS2 (NowPlaying (..))
import System.Taffybar.Information.Memory
import System.Taffybar.Information.Network (sumSpeeds)
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelWithVariableDelayAndRefresh)
import System.Taffybar.Widget.Temperature
import System.Taffybar.Widget.Workspaces qualified as Workspaces
import System.Taffybar.WidgetPriority (withPriority)
import Text.Printf (printf)
import Text.Read (readMaybe)
import WorkspaceGroups (workspaceGroupsNew)

-- Each action constructs a fresh widget for its bar. Shared widgets need no variant.
workspaces, workspacesCompact :: TaffyIO Gtk.Widget
workspaces = workspacesFor False
workspacesCompact = workspacesFor True

cpu, cpuCompact :: TaffyIO Gtk.Widget
cpu = cpuFor False
cpuCompact = cpuFor True

memory, memoryCompact :: TaffyIO Gtk.Widget
memory = memoryFor False
memoryCompact = memoryFor True

clock, clockCompact :: TaffyIO Gtk.Widget
clock = clockFor False
clockCompact = clockFor True

media, mediaCompact :: TaffyIO Gtk.Widget
media = mediaFor False
mediaCompact = mediaFor True

tray :: TaffyIO Gtk.Widget
tray =
  card "tray-card" $ do
    host <- getTrayHost False
    wechatIcon <- liftIO $ newMVar Nothing
    let transformItem = transformTrayItem wechatIcon
        transformedHost =
          host
            { SNI.itemInfoMap = SNI.itemInfoMap host >>= traverse transformItem
            , SNI.addUpdateHandler = \handler ->
                SNI.addUpdateHandler host $ \update info ->
                  transformItem info >>= handler update
            }
    sniTrayNewFromHostParams
      Tray.defaultTrayParams
        { Tray.trayImageSize = Tray.TrayImageSize 24
        , Tray.trayCenterIcons = True
        }
      transformedHost

-- Apply per-application rules to both initial tray items and later updates.
transformTrayItem :: MVar (Maybe SNI.ImageInfo) -> SNI.ItemInfo -> IO SNI.ItemInfo
transformTrayItem wechatIcon info
  | SNI.itemId info == Just "wechat" = do
      pixmaps <- cachedWeChatIcon wechatIcon
      pure $
        info
          { SNI.iconName = ""
          , SNI.iconThemePath = Nothing
          , SNI.iconPixmaps = pixmaps
          , SNI.overlayIconName = Nothing
          , SNI.overlayIconPixmaps = []
          }
  | otherwise = pure info

wechatPng :: Bytes.ByteString
wechatPng = $(embedFile "weiss-taffybar/assets/wechat.png")

-- Decode the embedded PNG once per tray, including simultaneous initial updates.
cachedWeChatIcon :: MVar (Maybe SNI.ImageInfo) -> IO SNI.ImageInfo
cachedWeChatIcon wechatIcon = modifyMVar wechatIcon $ \cached ->
  case cached of
    Just pixmaps -> pure (cached, pixmaps)
    Nothing -> do
      loader <- Pixbuf.pixbufLoaderNewWithType "png"
      Pixbuf.pixbufLoaderWrite loader wechatPng
      Pixbuf.pixbufLoaderClose loader
      original <- Pixbuf.pixbufLoaderGetPixbuf loader >>= maybe (fail "Cannot decode embedded WeChat icon") pure
      rgba <- Pixbuf.pixbufAddAlpha original False 0 0 0 >>= maybe (fail "Cannot add alpha to WeChat icon") pure
      width <- Pixbuf.pixbufGetWidth rgba
      height <- Pixbuf.pixbufGetHeight rgba
      stride <- fromIntegral <$> Pixbuf.pixbufGetRowstride rgba
      pixels <- Pixbuf.pixbufGetPixels rgba
      -- SNI's host-side pixmaps use tightly packed RGBA; discard row padding
      -- and copy the bytes so they outlive the temporary GdkPixbuf.
      packed <- evaluate $ Bytes.copy $ Bytes.concat [Bytes.take (fromIntegral width * 4) $ Bytes.drop (row * stride) pixels | row <- [0 .. fromIntegral height - 1]]
      let pixmaps = [(width, height, packed)]
      pure (Just pixmaps, pixmaps)

-- Keep the location fixed and show only conditions and temperature on the bar.
weather :: TaffyIO Gtk.Widget
weather = withPriority 4 $ card "weather-card" $ do
  manager <- liftIO $ newManager tlsManagerSettings
  request <- liftIO $ parseRequest "https://wttr.in/Kaiserslautern?format=%25c%0A%25t%0AWind:+%25w&m&lang=en"
  let unavailable = pure ("Weather N/A", Just "Weather unavailable; click to retry", 30)
      update = do
        result <- try @HttpException $ httpLbs request {checkResponse = throwErrorStatusCodes} manager
        case result of
          Left _ -> unavailable
          Right response -> case decodeUtf8' $ LazyBytes.toStrict $ responseBody response of
            Right report
              | [conditions, rawTemperature, wind] <- Text.lines $ Text.strip report
              , let temperature = Text.dropWhile (== '+') $ Text.strip rawTemperature
              , Just degrees <- readMaybe @Double $ Text.unpack $ Text.takeWhile (\c -> isDigit c || c == '-' || c == '.') temperature -> do
                  let color
                        | degrees <= 0 = "#89b4fa"
                        | degrees < 20 = "#ffffff"
                        | degrees < 30 = "#ebc58b"
                        | otherwise = "#f38ba8"
                      summary = Text.strip conditions <> " " <> temperature
                  icon <- markupEscapeText (Text.strip conditions) (-1)
                  escapedTemperature <- markupEscapeText temperature (-1)
                  let label = "<span foreground='" <> color <> "'><span size='12288'>" <> icon <> "</span> " <> escapedTemperature <> "</span>"
                  tooltip <- markupEscapeText (summary <> "\n" <> wind <> "\nClick to refresh") (-1)
                  pure (label, Just tooltip, 30)
            _ -> unavailable
  pollingLabelWithVariableDelayAndRefresh update True

graphConfig :: GraphConfig
graphConfig =
  def
    { graphPadding = 0
    , graphBorderWidth = 0
    , graphWidth = 76
    , graphHistorySize = 60
    , graphBackgroundColor = (0, 0, 0, 0)
    }

cpuConfig, memoryConfig :: GraphConfig
cpuConfig =
  graphConfig
    { graphDataColors = [(0.65, 0.85, 0.63, 0.6), (0.58, 0.89, 0.84, 0.9)]
    , graphLabel = Just "CPU"
    }
memoryConfig =
  graphConfig
    { graphDataColors = [(0.54, 0.71, 0.98, 0.65)]
    , graphWidth = 48
    }

memoryText :: MemoryInfo -> Text
memoryText info =
  Text.pack $
    printf
      "RAM %.0f/%.0f GB • SWAP %.0f/%.0f GB"
      (memoryUsed info * 1024 * 1024 / 1000000000)
      (memoryTotal info * 1024 * 1024 / 1000000000)
      (memorySwapUsed info * 1024 * 1024 / 1000000000)
      (memorySwapTotal info * 1024 * 1024 / 1000000000)

memoryTooltip :: MemoryInfo -> Text
memoryTooltip info =
  Text.pack $
    printf
      "RAM used: %.0f MB\nRAM total: %.0f MB\nRAM available: %.0f MB\nSwap used: %.0f MB\nSwap total: %.0f MB\nValues on the bar are used / total."
      (memoryUsed info * 1024 * 1024 / 1000000)
      (memoryTotal info * 1024 * 1024 / 1000000)
      (memoryAvailable info * 1024 * 1024 / 1000000)
      (memorySwapUsed info * 1024 * 1024 / 1000000)
      (memorySwapTotal info * 1024 * 1024 / 1000000)

memoryWidget :: Bool -> TaffyIO Gtk.Widget
memoryWidget compact = do
  channel <- getMemoryInfoChan 2
  initial <- getMemoryInfoState 2
  row <- Gtk.boxNew Gtk.OrientationHorizontal 6
  history <- if compact then pure Nothing else Just <$> graphNew memoryConfig
  forM_ history $ \(graph, _) -> Gtk.boxPackStart row graph False False 0
  label <- Gtk.labelNew Nothing
  _ <- widgetSetClassGI label "memory-values"
  Gtk.boxPackStart row label False False 0
  -- The text, tooltip, and graph all use the same memory snapshot.
  let update info = postGUIASync $ do
        Gtk.labelSetText label $ memoryText info
        Gtk.widgetSetTooltipText row $ Just $ memoryTooltip info
        forM_ history $ \(_, handle) -> graphAddSample handle [memoryUsedRatio info]
  _ <- Gtk.onWidgetRealize row $ update initial
  Gtk.toWidget =<< channelWidgetNew row channel update

-- Style the outer wrapper so padding and priority apply to the whole card.
card :: Text -> TaffyIO Gtk.Widget -> TaffyIO Gtk.Widget
card name widget = widget >>= buildContentsBox >>= (`widgetSetClassGI` name)

-- playerctld proxies a real player; rendering both duplicates the same media.
mediaPlayer :: Bool -> UpdateMPRIS2PlayerWidget MPRIS2PlayerControlsWidget
mediaPlayer compact add existing nowPlaying = do
  player <-
    simplePlayerWidgetWithControls
      def
        { setNowPlayingLabel = if compact then playingText 8 14 else playingText 12 24
        , showPlayerWidgetFn = \info ->
            pure $
              npStatus info /= "Stopped"
                && npBusName info /= "org.mpris.MediaPlayer2.playerctld"
        }
      add
      existing
      nowPlaying
  Gtk.widgetSetTooltipText (controlsPreviousButton player) $ Just "Previous track"
  Gtk.widgetSetTooltipText (controlsPlayPauseButton player) $ Just "Play / pause"
  Gtk.widgetSetTooltipText (controlsNextButton player) $ Just "Next track"
  pure player

showWorkspace :: Bool -> WorkspaceInfo -> Bool
showWorkspace compact workspace =
  not (workspaceIsSpecial workspace)
    && not ("NSP" `Text.isPrefixOf` workspaceName (workspaceIdentity workspace))
    && Workspaces.hideEmpty workspace
    && ( not compact
           || workspaceState workspace `elem` [WorkspaceActive, WorkspaceVisible]
           || workspaceHasUrgentWindow workspace
       )

workspacesFor :: Bool -> TaffyIO Gtk.Widget
workspacesFor compact = workspaceGroupsNew $ showWorkspace compact

cpuFor :: Bool -> TaffyIO Gtk.Widget
cpuFor compact = card "cpu-card" $ do
  row <- Gtk.boxNew Gtk.OrientationHorizontal 8
  usage <- cpuMonitorNew cpuConfig {graphWidth = if compact then 56 else 76} 0.5 "cpu"
  Gtk.boxPackStart row usage False False 0
  unless compact $ do
    temperature <- temperatureLabelNewChanWith def {tempPollInterval = 5}
    Gtk.boxPackStart row temperature False False 0
  Gtk.toWidget row

memoryFor :: Bool -> TaffyIO Gtk.Widget
memoryFor compact = card "memory-card" $ memoryWidget compact

-- Free space shows only in the tooltip, keeping the card narrow.
disk :: TaffyIO Gtk.Widget
disk =
  card "disk-card" $
    diskUsageNewWith
      def
        { diskUsagePath = "/"
        , diskUsageIcon = "DISK"
        , diskUsageFormat = "$usedPercent$%"
        , diskUsageTooltipFormat =
            Just "root: $used$ / $total$ used\n$available$ available\nClick to refresh"
        }

network :: TaffyIO Gtk.Widget
network = withPriority 5 $ do
  NetworkInfoChan channel <- getNetworkChan
  row <- Gtk.boxNew Gtk.OrientationHorizontal 4
  let block name arrow tooltip = do
        contents <- Gtk.boxNew Gtk.OrientationHorizontal 4
        icon <- Gtk.imageNewFromIconName (Just arrow) $ fromIntegral $ fromEnum Gtk.IconSizeMenu
        Gtk.imageSetPixelSize icon 18
        Gtk.widgetSetValign icon Gtk.AlignCenter
        _ <- widgetSetClassGI icon "network-arrow"
        value <- Gtk.labelNew $ Just "0MB/s"
        -- Reserve ten monospace characters even when the displayed unit changes.
        Gtk.labelSetWidthChars value 10
        Gtk.labelSetMaxWidthChars value 10
        Gtk.labelSetXalign value 1
        _ <- widgetSetClassGI value "network-rate"
        Gtk.boxPackStart contents icon False False 0
        Gtk.boxPackStart contents value False False 0
        wrapper <- card name $ Gtk.toWidget contents
        Gtk.widgetSetTooltipText wrapper $ Just tooltip
        Gtk.boxPackStart row wrapper False False 0
        pure value
  downstream <- block "network-down" "go-down-symbolic" "Download speed"
  upstream <- block "network-up" "go-up-symbolic" "Upload speed"
  _ <- widgetSetClassGI row "network-card"
  let formatSpeed speed
        | speed <= 300000 = "0MB/s"
        | otherwise = Text.pack $ printf "%.2f%s" (fromRational speed / divisor :: Double) unit
        where
          (divisor, unit)
            | speed >= 1000000000000 = (1000000000000, "TB/s" :: String)
            | speed >= 1000000000 = (1000000000, "GB/s")
            | otherwise = (1000000, "MB/s")
      update speedInfo = postGUIASync $ do
        -- Match the vendored network monitor's transmit/receive ordering.
        let (outgoing, incoming) = sumSpeeds $ fmap snd speedInfo
        Gtk.labelSetText downstream $ formatSpeed incoming
        Gtk.labelSetText upstream $ formatSpeed outgoing
  Gtk.toWidget =<< channelWidgetNew row channel update

clockFor :: Bool -> TaffyIO Gtk.Widget
clockFor compact =
  card "clock-card" $
    textClockNewWith
      def
        { clockFormatString =
            if compact
              then "<b>%H:%M</b>"
              else "<span foreground='#d8dee9' weight='medium'>%a %d %b</span>   <b>%H:%M</b>"
        , clockUpdateStrategy = RoundedTargetInterval 1 0
        }

mediaFor :: Bool -> TaffyIO Gtk.Widget
mediaFor compact =
  withPriority 2 $
    mpris2NewWithConfig
      defaultMPRIS2ControlsConfig
        { mprisWidgetWrapper = (`widgetSetClassGI` "media-card")
        , updatePlayerWidget = mediaPlayer compact
        }
