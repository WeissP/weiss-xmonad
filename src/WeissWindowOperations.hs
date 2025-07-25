{-# LANGUAGE LambdaCase #-}

module WeissWindowOperations (weissSwap, weissSwitchFocus, weissSwitchRecent, weissFocusMaster, borderColorHook, switchScreenBetween) where

import Config (myNormColor, myNormColorPixel, nextFocusColorPixel)
import Control.Monad (when)
import Control.Monad.Extra (whenJustM)
import Data.List qualified as L
import Data.List.Unique
import Data.Map qualified as Map
import Data.Maybe
import TreeActions (weissTreeActions)
import Utils
import WeissScratchpad
import XMonad
import XMonad (windowset)
import XMonad.Actions.EasyMotion (
  ChordKeys (..),
  EasyMotionConfig (..),
  bar,
  selectWindow,
  textSize,
 )
import XMonad.Actions.FocusNth (swapNth)
import XMonad.Actions.GroupNavigation (Direction (..), isOnAnyVisibleWS, nextMatch, onNextMatch, orderedWindowList)
import XMonad.Prelude ((<&>))
import XMonad.StackSet (focusWindow)
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.VisibleWindows (visibleWindows, visibleWindowsOnScreen)
import XMonad.Util.XUtils (stringToPixel)

switchToScreen :: ScreenId -> X ()
switchToScreen sid = whenJustM (screenWorkspace sid) (windows . W.view)

switchScreenBetween :: ScreenId -> ScreenId -> X ()
switchScreenBetween a b = do
  now <- gets windowset <&> W.screen . W.current
  when (now /= a && now /= b) (switchToScreen a)
  if now == b then switchToScreen a else switchToScreen b

easyMotionConf, rightHandMotionConf, leftHandMotionConf :: EasyMotionConfig
easyMotionConf =
  def
    { overlayF = bar 0.3
    , cancelKey = xK_Escape
    }
rightHandMotionConf =
  easyMotionConf
    { sKeys =
        AnyKeys [xK_f, xK_d, xK_s, xK_r, xK_e, xK_w, xK_g, xK_t, xK_x, xK_c, xK_v, xK_b, xK_a, xK_z]
    }
leftHandMotionConf =
  easyMotionConf
    { sKeys =
        AnyKeys [xK_j, xK_k, xK_l, xK_u, xK_i, xK_o, xK_m, xK_n, xK_h, xK_y]
    }

weissSwap :: X ()
weissSwap = onWindowsCount $ \c ->
  if c <= 2
    then windows W.swapDown >> weissFocusMaster
    else do
      win <- selectWindow leftHandMotionConf
      stack <- gets $ W.index . windowset
      let masterWin = listToMaybe stack
      currentWin <- gets (W.peek . windowset)
      let isMasterInvolved = win == masterWin || currentWin == masterWin
      let found = L.find ((win ==) . Just . fst) $ zip stack [0 ..]
      whenJust found $ \(_, idx) -> do
        swapNth idx
        if isMasterInvolved
          then weissFocusMaster
          else whenJust currentWin (windows . W.focusWindow)

weissSwitchFocus :: X ()
weissSwitchFocus = selectWindow rightHandMotionConf >>= (`whenJust` windows . W.focusWindow)

_notScratchWs :: Query Bool
_notScratchWs = do
  w <- ask
  ws <- liftX $ gets windowset
  let tag = W.findTag w ws
  return $ isJust tag && tag /= Just scratchpadWorkspaceTag

-- | Get all unfocused floating windows on any visible workspace.
getUnfocusedVisibleFloats :: X [Window]
getUnfocusedVisibleFloats = do
  ws <- gets windowset
  let visibleWorkspaceTags = W.tag . W.workspace <$> (W.current ws : W.visible ws)
      allFloatingWindows = W.floating ws
      onVisible wsTag = wsTag `elem` visibleWorkspaceTags
      visibleFloatingWindows = Map.filterWithKey (\w _ -> maybe False onVisible (W.findTag w ws)) allFloatingWindows
      maybeFocused = W.peek ws
      unfocusedFloats = case maybeFocused of
        Just focused -> Map.delete focused visibleFloatingWindows
        Nothing -> visibleFloatingWindows
  return $ Map.keys unfocusedFloats

{- | A query that matches switchable windows.
   If there are unfocused floating windows on a visible workspace, it matches only those.
   Otherwise it matches any window returned by Utils.visibleAllWindows.
-}
mySwitchableWindows :: Query Bool
mySwitchableWindows = do
  -- first grab every window currently visible (tiled or floating) on any screen
  visibleWins <- liftX visibleWindows
  -- then see if there are any unfocused floats on those visible workspaces
  unfocusedFloats <- liftX getUnfocusedVisibleFloats
  -- if none, allow any of the visibleWins; otherwise only allow those unfocused floats
  ask
    <&> ( \w ->
            if null unfocusedFloats
              then w `elem` visibleWins
              else w `elem` unfocusedFloats
        )

weissSwitchRecent :: X ()
weissSwitchRecent = nextMatch History mySwitchableWindows

newtype NextSwitchWindow = NextSwitchWindow (Maybe Window)
instance ExtensionClass NextSwitchWindow where
  initialValue = NextSwitchWindow Nothing

updateNextSwitchWindow :: Window -> X ()
updateNextSwitchWindow w = do
  currFocusedMaybe <- gets (W.peek . windowset)
  NextSwitchWindow currNswMaybe <- XS.get
  case (currFocusedMaybe, currNswMaybe) of
    (Just _, Just currNsw)
      | currFocusedMaybe /= currNswMaybe ->
          setWindowBorderColor myNormColorPixel currNsw
    (Nothing, Just currNsw) -> setWindowBorderColor myNormColorPixel currNsw
    _ -> return ()
  XS.put (NextSwitchWindow (Just w))
  setWindowBorderColor nextFocusColorPixel w

borderColorHook :: X ()
borderColorHook = do
  hisWindows <- orderedWindowList History
  onNextMatch mySwitchableWindows updateNextSwitchWindow (return ()) hisWindows

-- | Focus the master window of the current workspace
weissFocusMaster :: X ()
weissFocusMaster = do
  stack <- gets $ W.index . windowset
  windows $
    W.focusWindow (head stack)
