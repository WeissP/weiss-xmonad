{-# LANGUAGE LambdaCase #-}

module WeissWindowOperations (weissFocusDown, weissFocusUp, weissSwapMaster, easySwap, weissSwitchFocus) where

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
import XMonad.StackSet qualified as W
import XMonad.Util.Loggers

onWindowsCount :: (Int -> X ()) -> X ()
onWindowsCount f = do
  winCount <- length . W.index . windowset <$> get
  f winCount

handleOp :: X () -> X () -> X ()
handleOp handleNSP handleNormal = withFocused $ \w ->
  ifM
    (isNSP w)
    (curNSP_ >> handleNSP >> repositionNSP)
    (ifM (isFloating w) (return ()) handleNormal)

weissFocusDown :: X ()
weissFocusDown = handleOp weissTreeActions weissFocusDown_

weissFocusDown_ :: X ()
weissFocusDown_ = do
  l <- logLayout
  let run f = windows $ \s -> skipFloating s f
  case trimLayoutModifiers l of
    Just "TwoPane" -> run focusDownTwoPane
    Just "Mirror Tall" -> run $ skipMaster W.focusUp
    Just "Tall" -> run $ skipMaster W.focusDown
    _ -> run W.focusDown
  where
    focusDownTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    focusDownTwoPane = W.modify' $ \stack -> case stack of
      W.Stack r1 (l : up) (r2 : down) -> W.Stack r2 [l] (r1 : up <> down)
      W.Stack l [] (r1 : r2 : down) -> W.Stack r1 [l] (r2 : down)
      _ -> W.focusDown' stack
    skipMaster ::
      (W.StackSet i l a s sd -> W.StackSet i l a s sd) ->
      W.StackSet i l a s sd ->
      W.StackSet i l a s sd
    skipMaster f x =
      if isMaster x
        then f x
        else
          let newS = f x
           in if isMaster newS then f newS else newS

weissFocusUp :: X ()
weissFocusUp = handleOp prevNSP weissFocusUp_

weissFocusUp_ :: X ()
weissFocusUp_ = do
  l <- logLayout
  let run f = windows $ \s -> skipFloating s f
  case trimLayoutModifiers l of
    Just "TwoPane" -> run focusUpTwoPane
    Just "Mirror Tall" -> run $ backToMaster W.focusDown
    Just "Tall" -> run $ backToMaster W.focusUp
    _ -> run W.focusUp
  where
    focusUpTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    focusUpTwoPane = W.modify' $ \stack -> case stack of
      -- W.Stack r2 (l : r1 : up) down -> W.Stack l [] (r2 : r1 : down)
      W.Stack r1 (l : _) (r2 : down) -> W.Stack l [] (r1 : r2 : down)
      W.Stack l [] (r1 : r2 : down) -> W.Stack r2 [l] (r1 : down)
      _ -> W.focusUp' stack
    backToMaster ::
      (W.StackSet i l a s sd -> W.StackSet i l a s sd) ->
      W.StackSet i l a s sd ->
      W.StackSet i l a s sd
    backToMaster f x = if isMaster x then f x else W.focusMaster x

weissSwapMaster :: X ()
weissSwapMaster = handleOp initialNSP weissSwapMaster_

weissSwapMaster_ :: X ()
weissSwapMaster_ = do
  l <- logLayout
  case trimLayoutModifiers l of
    Just "TwoPane" -> windows swapMasterTwoPane
    _ -> windows $ W.modify' swapBetweenMasterAndSlave
  where
    swapBetweenMasterAndSlave :: W.Stack a -> W.Stack a
    swapBetweenMasterAndSlave stack = case stack of
      W.Stack f [] [] -> stack
      W.Stack f [] ds -> W.Stack (last ds) [] (f : init ds)
      W.Stack t ls rs -> W.Stack t [] (xs <> (x : rs))
        where
          (x : xs) = reverse ls
    swapMasterTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    swapMasterTwoPane = W.modify' $ \stack -> case stack of
      W.Stack r2 (l : r1 : up) down -> W.Stack r1 [r2] (l : down)
      W.Stack r1 (l : up) (r2 : down) -> W.Stack r2 [r1] (l : down)
      W.Stack l [] (r1 : r2 : down) -> W.Stack l [] (r2 : r1 : down)
      _ -> swapBetweenMasterAndSlave stack

easyMotionConf :: EasyMotionConfig
easyMotionConf =
  def
    { overlayF = textSize
    , cancelKey = xK_Escape
    , sKeys =
        PerScreenKeys
          ( Map.fromList
              [ (0, [xK_j, xK_k, xK_l, xK_u, xK_i, xK_o])
              , (1, [xK_m, xK_n, xK_h, xK_y])
              ]
          )
    }

easySwap :: X ()
easySwap = do
  win <- selectWindow easyMotionConf
  stack <- gets $ W.index . windowset
  let match = L.find ((win ==) . Just . fst) $ zip stack [0 ..]
  whenJust match $ swapNth . snd

weissSwitchFocus :: X ()
weissSwitchFocus = onWindowsCount $ \c ->
  if c <= 3
    then windows $ \s -> skipFloating s W.focusDown
    else selectWindow easyMotionConf >>= (`whenJust` windows . W.focusWindow)
