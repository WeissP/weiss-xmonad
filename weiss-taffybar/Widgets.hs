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

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (evaluate, try)
import Control.Monad (forM_, join, mfilter, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as LazyBytes
import Data.Char (isDigit)
import Data.Default (def)
import DBus (BusName)
import Data.FileEmbed (embedFile)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import GI.GLib (markupEscapeText)
import GI.Gdk qualified as Gdk
import GI.GdkPixbuf qualified as Pixbuf
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango
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
  request <- liftIO $ parseRequest "https://wttr.in/Kaiserslautern?format=%25c%0A%25t%0AWind:+%25w%0A%25S%0A%25s&m&lang=en"
  let unavailable = pure ("Weather N/A", Just "Weather unavailable; click to retry", 30)
      update = do
        result <- try @HttpException $ httpLbs request {checkResponse = throwErrorStatusCodes} manager
        case result of
          Left _ -> unavailable
          Right response -> case decodeUtf8' $ LazyBytes.toStrict $ responseBody response of
            Right report
              | [conditions, rawTemperature, wind, sunrise, sunset] <- Text.lines $ Text.strip report
              , let temperature = Text.dropWhile (== '+') $ Text.strip rawTemperature
              , Just degrees <- readMaybe @Double $ Text.unpack $ Text.takeWhile (\c -> isDigit c || c == '-' || c == '.') temperature -> do
                  let color
                        | degrees <= 0 = "#89b4fa"
                        | degrees < 20 = "#ffffff"
                        | degrees < 30 = "#ebc58b"
                        | otherwise = "#f38ba8"
                      summary = Text.strip conditions <> " " <> temperature
                      -- wttr.in reports local HH:MM:SS; seconds are noise here.
                      clock = Text.take 5 . Text.strip
                      sun = "Sunrise: " <> clock sunrise <> "\nSunset: " <> clock sunset
                  icon <- markupEscapeText (Text.strip conditions) (-1)
                  escapedTemperature <- markupEscapeText temperature (-1)
                  let label = "<span foreground='" <> color <> "'><span size='12288'>" <> icon <> "</span> " <> escapedTemperature <> "</span>"
                  tooltip <- markupEscapeText (summary <> "\n" <> wind <> "\n" <> sun <> "\nClick to refresh") (-1)
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
-- The frame marks 100%, so the fill height reads as a rough percentage.
memoryConfig =
  graphConfig
    { graphDataColors = [(0.54, 0.71, 0.98, 0.65)]
    , graphWidth = 48
    , graphPadding = 1
    , graphBorderWidth = 1
    , graphBorderColor = (0.6, 0.65, 0.74, 0.7)
    , graphBackgroundColor = (1, 1, 1, 0.04)
    }

-- Swap shows only in the tooltip, keeping the card narrow.
memoryText :: MemoryInfo -> Text
memoryText info =
  Text.pack $
    printf
      "RAM %.0f/%.0f GB"
      (memoryUsed info * 1024 * 1024 / 1000000000)
      (memoryTotal info * 1024 * 1024 / 1000000000)

memoryTooltip :: MemoryInfo -> Text
memoryTooltip info =
  Text.pack $
    printf
      "RAM used: %.2f GB\nRAM total: %.2f GB\nRAM available: %.2f GB\nSwap used: %.2f GB\nSwap total: %.2f GB."
      (memoryUsed info * 1024 * 1024 / 1000000000)
      (memoryTotal info * 1024 * 1024 / 1000000000)
      (memoryAvailable info * 1024 * 1024 / 1000000000)
      (memorySwapUsed info * 1024 * 1024 / 1000000000)
      (memorySwapTotal info * 1024 * 1024 / 1000000000)

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

-- The card shows one player; the toggle button cycles through the rest.
-- Playback buttons appear over the title only while the pointer is over the
-- card. They overlay the title instead of taking space: resizing on hover makes
-- priority hiding move the card away from the pointer, flipping the hover state
-- in a loop.
data MediaState = MediaState
  { mediaHovered :: Bool
  , mediaPlaying :: [BusName]
  , mediaToggleLabel :: Text
  , mediaPrimary :: Maybe BusName
  , mediaRows :: Map BusName (NowPlaying, Gtk.Widget, Maybe Gtk.Widget)
  }

-- playerctld proxies a real player; rendering both duplicates the same media.
listedPlayer :: NowPlaying -> Bool
listedPlayer info =
  npStatus info /= "Stopped"
    && npBusName info /= "org.mpris.MediaPlayer2.playerctld"

mediaPlayer :: Bool -> IORef MediaState -> Gtk.Button -> UpdateMPRIS2PlayerWidget MPRIS2PlayerControlsWidget
mediaPlayer compact stateRef toggle add existing nowPlaying = do
  player <-
    simplePlayerWidgetWithControls
      def
        { -- Cap the card by the label's width rather than by character counts,
          -- so wide (e.g. CJK) titles take no more room than Latin ones.
          setNowPlayingLabel = playingText maxBound maxBound
        , setupPlayerLabel = \label -> do
            Gtk.labelSetEllipsize label Pango.EllipsizeModeEnd
            Gtk.labelSetMaxWidthChars label $ if compact then 20 else 36
        , showPlayerWidgetFn = pure . listedPlayer
        }
      add
      existing
      nowPlaying
  Gtk.widgetSetTooltipText (controlsPreviousButton player) $ Just "Previous track"
  Gtk.widgetSetTooltipText (controlsPlayPauseButton player) $ Just "Play / pause"
  Gtk.widgetSetTooltipText (controlsNextButton player) $ Just "Next track"
  liftIO $ do
    -- The widget keeps its last state, which names the player even after it exits.
    bus <- npBusName <$> readMVar (controlsNowPlayingVar player)
    controls <- Gtk.widgetGetParent $ controlsPreviousButton player
    when (isNothing existing) $ forM_ controls $ overlayControls player
    let updateRows = case nowPlaying of
          Just info | listedPlayer info -> Map.insert bus (info, controlsPlayerWidget player, controls)
          _ -> Map.delete bus
    modifyIORef' stateRef $ \state -> state {mediaRows = updateRows $ mediaRows state}
    applyMediaState stateRef toggle
  pure player

-- Move the playback buttons from beside the title onto its text, leaving the
-- player icon uncovered.
overlayControls :: MPRIS2PlayerControlsWidget -> Gtk.Widget -> IO ()
overlayControls player controls = do
  let label = controlsPlayerLabel player
  clickArea <- join <$> (traverse (Gtk.castTo Gtk.Box) =<< Gtk.widgetGetParent label)
  playerBox <- Gtk.castTo Gtk.Box $ controlsPlayerWidget player
  forM_ ((,) <$> clickArea <*> playerBox) $ \(area, box) -> do
    Gtk.containerRemove box controls
    Gtk.containerRemove area label
    -- Gap between the player icon and the title.
    Gtk.boxSetSpacing area 6
    overlay <- Gtk.overlayNew
    Gtk.containerAdd overlay label
    Gtk.overlayAddOverlay overlay controls
    Gtk.widgetSetHalign controls Gtk.AlignEnd
    Gtk.widgetSetValign controls Gtk.AlignCenter
    Gtk.boxPackStart area overlay True True 0
    Gtk.widgetShow overlay

-- Switch when a player starts playing or the shown one stops, but only on those
-- transitions, so a paused player picked with the toggle stays shown.
applyMediaState :: IORef MediaState -> Gtk.Button -> IO ()
applyMediaState stateRef toggle = do
  state <- readIORef stateRef
  let rows = mediaRows state
      buses = Map.keys rows
      playing = Map.keys $ Map.filter (\(info, _, _) -> npStatus info == "Playing") rows
      started = filter (`notElem` mediaPlaying state) playing
      current = mfilter (`Map.member` rows) $ mediaPrimary state
      stopped = maybe False (\bus -> bus `elem` mediaPlaying state && bus `notElem` playing) current
      primary
        | bus : _ <- started = Just bus
        | stopped, bus : _ <- playing = Just bus
        | otherwise = current <|> listToMaybe playing <|> listToMaybe buses
      position = maybe 0 (+ 1) $ (`elemIndex` buses) =<< primary
      label = Text.pack $ show position <> "/" <> show (length buses)
  writeIORef stateRef state {mediaPrimary = primary, mediaPlaying = playing, mediaToggleLabel = label}
  forM_ (Map.toList rows) $ \(bus, (_, row, controls)) -> do
    Gtk.widgetSetVisible row $ Just bus == primary
    forM_ controls $ \box -> do
      -- Fade the buttons rather than hide them: GTK measures hidden widgets as
      -- zero wide. While invisible, clicks pass through to the title.
      Gtk.widgetSetOpacity box $ if mediaHovered state then 1 else 0
      overlay <- join <$> (traverse (Gtk.castTo Gtk.Overlay) =<< Gtk.widgetGetParent box)
      forM_ overlay $ \parent -> do
        Gtk.overlaySetOverlayPassThrough parent box $ not $ mediaHovered state
        -- Overlays don't affect the card's size, so keep short titles wide
        -- enough to fit the buttons.
        (_, width) <- Gtk.widgetGetPreferredWidth box
        Gtk.widgetSetSizeRequest parent width (-1)
  -- Relabeling queues a resize, so skip it when the text is unchanged.
  unless (mediaToggleLabel state == label) $ Gtk.buttonSetLabel toggle label
  Gtk.widgetSetVisible toggle $ length buses > 1

-- Advance to the player after the shown one, wrapping around.
nextMediaPlayer :: MediaState -> MediaState
nextMediaPlayer state = state {mediaPrimary = listToMaybe $ after <> buses}
  where
    buses = Map.keys $ mediaRows state
    after = maybe buses (\bus -> drop 1 $ dropWhile (/= bus) buses) $ mediaPrimary state

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
mediaFor compact = withPriority 2 $ do
  stateRef <-
    liftIO $
      newIORef
        MediaState {mediaHovered = False, mediaPlaying = [], mediaToggleLabel = "", mediaPrimary = Nothing, mediaRows = Map.empty}
  toggle <- Gtk.buttonNew
  Gtk.widgetSetTooltipText toggle $ Just "Next player"
  -- Visibility follows the player count, not a parent's show-all.
  Gtk.widgetSetNoShowAll toggle True
  _ <- widgetSetClassGI toggle "mpris-control"
  _ <- widgetSetClassGI toggle "media-toggle"
  _ <- Gtk.onButtonClicked toggle $ do
    modifyIORef' stateRef nextMediaPlayer
    applyMediaState stateRef toggle
  mpris2NewWithConfig
    defaultMPRIS2ControlsConfig
      { mprisWidgetWrapper = \players -> do
          Gtk.castTo Gtk.Grid players >>= mapM_ (`Gtk.gridSetColumnSpacing` 14)
          row <- Gtk.boxNew Gtk.OrientationHorizontal 0
          Gtk.boxPackStart row players False False 0
          Gtk.boxPackStart row toggle False False 0
          _ <- widgetSetClassGI row "media-card"
          hover <- Gtk.eventBoxNew
          -- A real window parents the buttons, so leaving onto them reports NotifyInferior.
          Gtk.eventBoxSetVisibleWindow hover True
          Gtk.containerAdd hover row
          -- Resizing the card under the pointer emits crossing events; ignore no-ops.
          let setHovered hovered = do
                state <- readIORef stateRef
                unless (mediaHovered state == hovered) $ do
                  writeIORef stateRef state {mediaHovered = hovered}
                  applyMediaState stateRef toggle
          _ <- Gtk.onWidgetEnterNotifyEvent hover $ \_ -> setHovered True >> pure False
          _ <- Gtk.onWidgetLeaveNotifyEvent hover $ \event -> do
            -- Moving onto a button or label is still inside the card.
            detail <- Gdk.getEventCrossingDetail event
            unless (detail == Gdk.NotifyTypeInferior) $ setHovered False
            pure False
          Gtk.widgetShow row
          Gtk.widgetShow hover
          widgetSetClassGI hover "media-hover" >>= Gtk.toWidget
      , updatePlayerWidget = mediaPlayer compact stateRef toggle
      }
