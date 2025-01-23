{-# LANGUAGE LambdaCase #-}

module WeissWindowOperations (weissSwap, weissSwitchFocus, weissSwitchRecent, weissFocusMaster) where

import Data.List qualified as L
import Data.List.Unique
import Data.Map qualified
import Data.Map qualified as M
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
import XMonad.Actions.GroupNavigation (Direction (..), isOnAnyVisibleWS, nextMatch)
import XMonad.StackSet (focusWindow)

-- import XMonad.StackSet qualified as SS
import XMonad.StackSet qualified as W
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

easyMotionConf, rightHandMotionConf, leftHandMotionConf :: EasyMotionConfig
easyMotionConf =
  def
    { overlayF = bar 0.5
    , cancelKey = xK_Escape
    }
rightHandMotionConf =
  easyMotionConf
    { sKeys =
        PerScreenKeys
          ( Map.fromList
              [ (0, [xK_f, xK_d, xK_s, xK_r, xK_e, xK_w, xK_g, xK_t])
              , (1, [xK_x, xK_c, xK_v, xK_b, xK_a, xK_z])
              ]
          )
    }
leftHandMotionConf =
  easyMotionConf
    { sKeys =
        AnyKeys [xK_j, xK_k, xK_l, xK_u, xK_i, xK_o, xK_m, xK_n, xK_h, xK_y]
    }

weissSwap :: X ()
weissSwap = onWindowsCount $ \c ->
  if c <= 2
    then windows W.swapDown >> windows W.focusDown
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

weissSwitchRecent :: X ()
weissSwitchRecent = nextMatch History isOnAnyVisibleWS
  where
    _notScratchWs = do
      w <- ask
      ws <- liftX $ gets windowset
      let tag = W.findTag w ws
      return $ isJust tag && tag /= Just scratchpadWorkspaceTag

-- | Focus the master window of the current workspace
weissFocusMaster :: X ()
weissFocusMaster = do
  stack <- gets $ W.index . windowset
  windows $
    W.focusWindow (head stack)
