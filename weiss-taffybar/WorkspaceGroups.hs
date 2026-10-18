module WorkspaceGroups (workspaceGroupsNew) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (readMVar)
import Control.Monad (forM_, guard, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Default (def)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (elemIndex, groupBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import System.Taffybar.Context (TaffyIO, runX11, subscribeToPropertyEvents, unsubscribe)
import System.Taffybar.Information.Workspaces.EWMH (getEWMHWorkspaceStateChanAndVar)
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Information.X11DesktopInfo (readAsString)
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Util (widgetSetClassGI)
import System.Taffybar.Widget.Workspaces qualified as Workspaces
import Text.Read (readMaybe)

numberedFamily :: WorkspaceInfo -> Maybe Text
numberedFamily workspace = case Text.splitOn "." $ workspaceName $ workspaceIdentity workspace of
  [family, member] -> do
    familyNumber <- readMaybe (Text.unpack family) :: Maybe Int
    memberNumber <- readMaybe (Text.unpack member) :: Maybe Int
    guard $ familyNumber >= 1 && familyNumber <= 9 && memberNumber >= 1
    pure family
  _ -> Nothing

chineseFamily :: WorkspaceInfo -> Maybe Text
chineseFamily workspace = do
  let name = workspaceName $ workspaceIdentity workspace
      family = Text.take 1 name
      suffix = Text.drop 1 name
  guard $ family `elem` ["览", "邮", "媒", "聊", "设", "记"]
  guard $ Text.null suffix || suffix `elem` (Text.pack . show <$> [2 .. 9 :: Int])
  pure family

chineseWorkspace :: WorkspaceInfo -> Bool
chineseWorkspace = isJust . chineseFamily

-- Bare Chinese labels retain their shared group unless a numbered member is shown.
workspaceGroup :: [Text] -> WorkspaceInfo -> Text
workspaceGroup expanded workspace =
  case chineseFamily workspace of
    Just family | family `notElem` expanded -> "chinese-singletons"
    Just family -> family
    Nothing -> fromMaybe (workspaceName $ workspaceIdentity workspace) $ numberedFamily workspace

workspaceGroupsNew :: (WorkspaceInfo -> Bool) -> TaffyIO Gtk.Widget
workspaceGroupsNew showWorkspace = do
  context <- ask
  (channel, snapshotVar) <- getEWMHWorkspaceStateChanAndVar
  row <- Gtk.boxNew Gtk.OrientationHorizontal 8
  _ <- widgetSetClassGI row "workspaces-card"
  previous <- liftIO $ newIORef Nothing
  let config =
        def
          { Workspaces.maxIcons = Just 0
          , Workspaces.minIcons = 0
          , Workspaces.urgentWorkspaceState = False
          , Workspaces.labelSetter = \workspace ->
              let name = workspaceName $ workspaceIdentity workspace
               in pure $ Text.unpack $ case numberedFamily workspace of
                    Just family -> Text.drop (Text.length family + 1) name
                    Nothing -> name
          }
      render snapshot = do
        -- Read inside the GUI update so queued events always use the latest order.
        familyOrder <- words <$> runX11 (readAsString Nothing "_WEISS_NUMBERED_FAMILY_ORDER")
        let ordered = snapshotWorkspaces snapshot
            identities = fmap workspaceIdentity ordered
            visible = filter showWorkspace ordered
            expanded =
              [ family
              | ws <- visible
              , Just family <- [chineseFamily ws]
              , workspaceName (workspaceIdentity ws) /= family
              ]
            presentation =
              [(workspaceIdentity ws, workspaceState ws, workspaceHasUrgentWindow ws) | ws <- visible]
            signature = (identities, presentation, familyOrder)
        old <- liftIO $ readIORef previous
        -- Window-title changes do not change this names-only view.
        when (old /= Just signature) $ do
          liftIO $ writeIORef previous $ Just signature
          Gtk.containerGetChildren row >>= mapM_ Gtk.widgetDestroy
          forM_ (groupBy (\a b -> workspaceGroup expanded a == workspaceGroup expanded b) visible) $ \members -> do
            group <- Gtk.boxNew Gtk.OrientationHorizontal 1
            when (any chineseWorkspace members) $
              void $
                widgetSetClassGI group "chinese-workspace-group"
            let expandedChinese ws = do
                  family <- chineseFamily ws
                  guard $ family `elem` expanded
                  pure family
                groupConfig =
                  config
                    { Workspaces.labelSetter = \ws -> case expandedChinese ws of
                        Just family ->
                          let suffix = Text.drop (Text.length family) $ workspaceName $ workspaceIdentity ws
                           in pure $ if Text.null suffix then "1" else Text.unpack suffix
                        Nothing -> Workspaces.labelSetter config ws
                    }
            case members of
              first : _ -> forM_ (numberedFamily first <|> expandedChinese first) $ \family -> do
                case numberedFamily first of
                  Just _ -> do
                    void $ widgetSetClassGI group "numbered-workspace-group"
                    case elemIndex (Text.unpack family) familyOrder of
                      Just 0 -> void $ widgetSetClassGI group "family-order-first"
                      Just 1 -> void $ widgetSetClassGI group "family-order-second"
                      _ -> pure ()
                  Nothing -> void $ widgetSetClassGI group "chinese-numbered-workspace-group"
                heading <- Gtk.labelNew $ Just family
                void $ widgetSetClassGI heading "workspace-family-label"
                Gtk.boxPackStart group heading False False 0
              [] -> pure ()
            forM_ members $ \workspace -> do
              controller <- Workspaces.defaultWidgetBuilder groupConfig workspace
              button <- Gtk.eventBoxNew
              -- Keep urgency independent of the controller's active/visible state.
              when (workspaceHasUrgentWindow workspace) $
                void $
                  widgetSetClassGI button "workspace-attention"
              Gtk.eventBoxSetVisibleWindow button False
              Gtk.widgetSetTooltipText button $ Just $ workspaceName $ workspaceIdentity workspace
              Gtk.widgetAddEvents button [Gdk.EventMaskScrollMask]
              Gtk.containerAdd button $ Workspaces.controllerWidget controller
              let switch target = runReaderT (Workspaces.onWorkspaceClick config target) context
              void $ Gtk.onWidgetButtonPressEvent button $ \_ -> switch workspace >> pure True
              void $ Gtk.onWidgetScrollEvent button $ \event -> do
                direction <- Gdk.getEventScrollDirection event
                let step = case direction of
                      Gdk.ScrollDirectionUp -> -1
                      Gdk.ScrollDirectionLeft -> -1
                      Gdk.ScrollDirectionDown -> 1
                      Gdk.ScrollDirectionRight -> 1
                      _ -> 0
                -- Preserve the standard pager's scroll order, including empty workspaces.
                case elemIndex (workspaceIdentity workspace) identities of
                  Just index | step /= 0 -> do
                    switch $ ordered !! ((index + step) `mod` length ordered)
                    pure True
                  _ -> pure False
              Gtk.containerAdd group button
            Gtk.containerAdd row group
          Gtk.widgetShowAll row
      update snapshot = postGUIASync $ runReaderT (render snapshot) context
  subscription <- subscribeToPropertyEvents ["_WEISS_NUMBERED_FAMILY_ORDER"] $ \_ ->
    liftIO $ readMVar snapshotVar >>= update
  void $ Gtk.onWidgetDestroy row $ runReaderT (unsubscribe subscription) context
  render =<< liftIO (readMVar snapshotVar)
  void $ Gtk.onWidgetRealize row $ readMVar snapshotVar >>= update
  Gtk.toWidget =<< channelWidgetNew row channel update
