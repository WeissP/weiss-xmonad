{-# LANGUAGE LambdaCase #-}

module MyWindowOperations where
import           Data.List.Unique
import qualified Data.Map                      as M
import qualified Data.Map
import           Data.Maybe
import           MyLogger
import           XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.Loggers

isMaster :: W.StackSet i l a s sd -> Bool
isMaster ss = case W.stack . W.workspace . W.current $ ss of
    Just (W.Stack _ [] _) -> True
    _                     -> False

isFloating :: Window -> X Bool
isFloating w = do
    ws <- gets windowset
    return $ M.member w (W.floating ws)

existsFloating :: X Bool
existsFloating = withWindowSet $ \winSet -> do
    let windows      = W.integrate' (W.stack . W.workspace . W.current $ winSet)
        allFloatings = W.floating winSet
    return $ not $ allUnique $ windows ++ M.keys allFloatings

myFocusDownPure :: X ()
myFocusDownPure =
    focusWithFloating (windows (`skipFloating` W.focusDown)) myFocusDownPure'

myFocusUpPure :: X ()
myFocusUpPure =
    focusWithFloating (windows (`skipFloating` W.focusUp)) myFocusUpPure'

focusWithFloating :: X () -> X () -> X ()
focusWithFloating withFloating withoutFloating = do
    floatP <- existsFloating
    if floatP then withFloating else withoutFloating

myFocusDownPure' :: X ()
myFocusDownPure' = do
    l <- logLayout
    case trimLayoutModifiers l of
        Just "TwoPane"     -> windows focusDownTwoPane
        Just "Mirror Tall" -> windows $ skipMaster W.focusUp 
        Just "Tall"        -> windows $ skipMaster W.focusDown
        _                  -> windows W.focusDown
  where
    focusDownTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    focusDownTwoPane = W.modify' $ \stack -> case stack of
        W.Stack r1 (l : up) (r2 : down) -> W.Stack r2 [l] (r1 : up ++ down)
        W.Stack l [] (r1 : r2 : down) -> W.Stack r1 [l] (r2 : down)
        _ -> W.focusDown' stack
    skipMaster :: (W.StackSet i l a s sd -> W.StackSet i l a s sd) -> W.StackSet i l a s sd -> W.StackSet i l a s sd
    skipMaster f x = if isMaster x
        then f x
        else
            let newS = f x
            in  if isMaster newS then f newS else newS

myFocusUpPure' :: X ()
myFocusUpPure' = do
    l <- logLayout
    case trimLayoutModifiers l of
        Just "TwoPane"     -> windows focusUpTwoPane
        Just "Mirror Tall" -> windows $ backToMaster W.focusDown 
        Just "Tall"        -> windows $ backToMaster W.focusUp
        _                  -> windows W.focusUp
  where
    focusUpTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    focusUpTwoPane = W.modify' $ \stack -> case stack of
        -- W.Stack r2 (l : r1 : up) down -> W.Stack l [] (r2 : r1 : down)
        W.Stack r1 (l : up) (r2 : down) -> W.Stack l [] (r1 : r2 : down)
        W.Stack l [] (r1 : r2 : down) -> W.Stack r2 [l] (r1 : down)
        _ -> W.focusUp' stack
    backToMaster :: (W.StackSet i l a s sd -> W.StackSet i l a s sd) -> W.StackSet i l a s sd -> W.StackSet i l a s sd
    backToMaster f x = if isMaster x then f x else W.focusMaster x

mySwapMasterPure :: X ()
mySwapMasterPure = do
    l <- logLayout
    case trimLayoutModifiers l of
        Just "TwoPane" -> windows swapMasterTwoPane
        _              -> windows $ W.modify' swapBetweenMasterAndSlave
  where
    swapBetweenMasterAndSlave :: W.Stack a -> W.Stack a
    swapBetweenMasterAndSlave stack = case stack of
        W.Stack f [] [] -> stack
        W.Stack f [] ds -> W.Stack (last ds) [] (f : init ds)
        W.Stack t ls rs -> W.Stack t [] (xs ++ x : rs)
            where (x : xs) = reverse ls
    swapMasterTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    swapMasterTwoPane = W.modify' $ \stack -> case stack of
        W.Stack r2 (l : r1 : up) down -> W.Stack r1 [r2] (l : down)
        W.Stack r1 (l : up) (r2 : down) -> W.Stack r2 [r1] (l : down)
        W.Stack l [] (r1 : r2 : down) -> W.Stack l [] (r2 : r1 : down)
        _ -> swapBetweenMasterAndSlave stack




-- | if the workspace is visible in some screen, then focus to this screen, else switch current screen to that workspace
switchOrFocus :: WorkspaceId -> X ()
switchOrFocus ws = switchOrFocusHelp ws 0
  where
    switchOrFocusHelp ws sc = screenWorkspace sc >>= \case
        Nothing -> windows $ W.greedyView ws
        Just x  -> if x == ws
            then windows $ W.view x
            else switchOrFocusHelp ws (sc + 1)

-- from https://www.reddit.com/r/xmonad/comments/hm2tg0/how_to_toggle_floating_state_on_a_window/
toggleFloat :: Window -> X ()
toggleFloat w = windows
    (\s -> if M.member w (W.floating s)
        then W.sink w s
        else W.float w (W.RationalRect 0 0 1 1) s
    )

shiftThenSwitchOrFocus i = do
    windows $ W.shift i
    switchOrFocus i



-- comes from https://gist.github.com/gilbertw1/603c3af68a21a10f1833
skipFloating
    :: (Eq a, Ord a)
    => W.StackSet i l a s sd
    -> (W.StackSet i l a s sd -> W.StackSet i l a s sd)
    -> W.StackSet i l a s sd
skipFloating stacks f | isNothing curr = stacks
                      | -- short circuit if there is no currently focused window
                        otherwise      = skipFloatingR stacks curr f
    where curr = W.peek stacks

skipFloatingR
    :: (Eq a, Ord a)
    => W.StackSet i l a s sd
    -> (Maybe a)
    -> (W.StackSet i l a s sd -> W.StackSet i l a s sd)
    -> W.StackSet i l a s sd
skipFloatingR stacks startWindow f
    | isNothing nextWindow      = stacks
    | -- next window is nothing return current stack set
      nextWindow == startWindow = newStacks
    | -- if next window is the starting window then return the new stack set
      M.notMember (fromJust nextWindow) (W.floating stacks) = newStacks
    | -- if next window is not a floating window return the new stack set
      otherwise                 = skipFloatingR newStacks startWindow f -- the next window is a floating window so keep recursing (looking)
  where
    newStacks  = f stacks
    nextWindow = W.peek newStacks



