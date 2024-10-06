{-# LANGUAGE LambdaCase #-}

module WeissWindowOperations (weissSwap, weissSwitchFocus) where

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
  selectWindow,
  textSize,
 )
import XMonad.Actions.FocusNth (swapNth)
import XMonad.StackSet (focusWindow)
import XMonad.StackSet qualified as W
import XMonad.Util.Loggers

easyMotionConf, rightHandMotionConf, leftHandMotionConf :: EasyMotionConfig
easyMotionConf =
  def
    { overlayF = textSize
    , cancelKey = xK_Escape
    }
rightHandMotionConf =
  easyMotionConf
    { sKeys =
        PerScreenKeys
          ( Map.fromList
              [ (0, [xK_f, xK_d, xK_s, xK_r, xK_e, xK_w])
              , (1, [xK_x, xK_c, xK_v, xK_a])
              ]
          )
    }
leftHandMotionConf =
  easyMotionConf
    { sKeys =
        PerScreenKeys
          ( Map.fromList
              [ (0, [xK_j, xK_k, xK_l, xK_u, xK_i, xK_o])
              , (1, [xK_m, xK_n, xK_h, xK_y])
              ]
          )
    }

weissSwap :: X ()
weissSwap = onWindowsCount $ \c ->
  if c <= 2
    then windows W.swapDown >> windows W.focusDown
    else withFocused $ \focused -> do
      win <- selectWindow leftHandMotionConf
      stack <- gets $ W.index . windowset
      let match = L.find ((win ==) . Just . fst) $ zip stack [0 ..]
      whenJust match $ \(_, idx) -> swapNth idx >> focus focused

weissSwitchFocus :: X ()
weissSwitchFocus = onWindowsCount $ \c ->
  if c <= 3
    then windows $ \s -> skipFloating s W.focusDown
    else selectWindow rightHandMotionConf >>= (`whenJust` windows . W.focusWindow)
