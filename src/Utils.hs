module Utils where

import Config
import Control.Monad (filterM, liftM)
import Control.Monad.Trans.Maybe
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Unique (allUnique)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import XMonad
import XMonad.Hooks.StatusBar.PP
import XMonad.Prelude (Endo (..), WindowScreen)
import XMonad.StackSet qualified as W
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.PureX (curScreenId)

{- |
Find all visible windows on a given screen.
If a full-screen floating window exists, it returns only that window,
treating all other windows on the same screen as invisible.
-}
visibleWindows :: ScreenId -> X [Window]
visibleWindows sid = do
  XState {mapped = mappedWins, windowset = ws} <- get
  -- all windows on the workspace driving this screen
  let scrn = L.find ((== sid) . W.screen) (W.screens ws)
      allOnScreen = foldMap (W.integrate' . W.stack . W.workspace) scrn
      visibleStack = filter (`Set.member` mappedWins) allOnScreen
      floatMap = W.floating ws
      -- filter out any full‐screen floating windows
      fullFloats = filter (\w -> maybe False isFullScreen (Map.lookup w floatMap)) visibleStack
  -- if a full-screen float exists, show only that; otherwise show the normal list
  return $ case fullFloats of
    [] -> visibleStack
    fs -> fs

-- | Find all visible windows on all screens.
visibleAllWindows :: X [Window]
visibleAllWindows = do
  XState {windowset = ws} <- get
  let sids = fmap W.screen (W.screens ws)
  concat <$> mapM visibleWindows sids

onWindowsCount :: (Int -> X ()) -> X ()
onWindowsCount f = do
  winCount <- length . W.index . windowset <$> get
  f winCount

liftMaybeT :: (Monad m) => m a -> MaybeT m a
liftMaybeT act = MaybeT $ Just `liftM` act

numToKey :: Int -> String
numToKey s = ["m", ",", ".", "j", "k", "l", "u", "i", "o", "-"] !! (s - 1)

-- from https://www.reddit.com/r/xmonad/comments/hm2tg0/how_to_toggle_floating_state_on_a_window/
toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if Map.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect 0 0 1 1) s
    )

-- Query: starts with
(^=?) :: (Eq a) => Query [a] -> [a] -> Query Bool
q ^=? x = L.isPrefixOf x <$> q

-- receive one sperate and three funs to format count, focused window and unfocused window
myLogTitles ::
  String ->
  String ->
  (Int -> String) ->
  (String -> String) ->
  ([String] -> String) ->
  Logger
myLogTitles sep1 sep2 formatCount formatFoc formatUnfoc = do
  winset <- gets windowset
  let focWin = W.peek winset
      wins = W.index winset
      winsUnfoc = filter (\w -> Just w /= focWin) wins
      count = length wins
  winNamesUnfoc <- case winsUnfoc of
    [] -> pure ""
    xs -> (sep2 ++) . formatUnfoc <$> traverse (fmap show . getName) xs
  focWinName <- case focWin of
    Just justFoc ->
      (sep1 ++)
        . formatFoc
        . shorten focusedTitleLength
        . show
        <$> getName justFoc
    Nothing -> pure ""
  pure . Just $ formatCount count <> focWinName <> winNamesUnfoc

logWindowCount :: X (Maybe String)
logWindowCount = withWindowSet ct
  where
    ct ss =
      return $
        Just $
          show $
            length $
              W.integrate' $
                W.stack $
                  W.workspace $
                    W.current ss

logMaster :: X Bool
logMaster = withWindowSet isM
  where
    isM ss = return $ case W.stack . W.workspace . W.current $ ss of
      Just (W.Stack _ [] _) -> True
      _ -> False

withFocusedG :: (Window -> X a) -> X (Maybe a)
withFocusedG f = withWindowSet $ mapM f . W.peek

logFocusedScreenRect :: X Rectangle
logFocusedScreenRect = withWindowSet $ return . screenRect . W.screenDetail . W.current

logWinCount :: X Int
logWinCount = length . W.index . windowset <$> get

logIsVerticalScreen :: X Bool
logIsVerticalScreen = do
  rect <- logFocusedScreenRect
  return $ rect_height rect > rect_width rect

data ScreenType = Screen1080p | Screen2K | ScreenUltraWide
  deriving (Show, Eq)

-- | Determine screen type based on physical dimensions and aspect ratio
logScreenType :: X ScreenType
logScreenType = do
  rect <- logFocusedScreenRect
  isVertical <- logIsVerticalScreen
  let (w, h) =
        if isVertical
          then (rect_height rect, rect_width rect)
          else (rect_width rect, rect_height rect)
      aspectRatio = (fromIntegral w :: Double) / fromIntegral h
  return $
    if aspectRatio > 2.0
      then ScreenUltraWide
      else
        if w > 2560 -- 2K width threshold
          then Screen2K
          else Screen1080p

trimPrefixWithList :: [String] -> String -> String
-- trimPrefixWithList _ Nothing = Nothing
trimPrefixWithList xs s = case mapMaybe (`L.stripPrefix` s) xs of
  [] -> s
  n : _ -> trimPrefixWithList xs n

trimLayoutModifiers :: String -> String
trimLayoutModifiers = trimPrefixWithList ["Spacing", " "]

isMaster :: W.StackSet i l a s sd -> Bool
isMaster ss = case W.stack . W.workspace . W.current $ ss of
  Just (W.Stack _ [] _) -> True
  _ -> False

isFloating :: Window -> X Bool
isFloating w = do
  ws <- gets windowset
  return $ Map.member w (W.floating ws)

existsFloating :: X Bool
existsFloating = withWindowSet $ \winSet -> do
  let ws = W.integrate' (W.stack . W.workspace . W.current $ winSet)
      allFloatings = W.floating winSet
  return $ not $ allUnique $ ws <> Map.keys allFloatings

-- comes from https://gist.github.com/gilbertw1/603c3af68a21a10f1833
skipFloating ::
  (Eq a, Ord a) =>
  W.StackSet i l a s sd ->
  (W.StackSet i l a s sd -> W.StackSet i l a s sd) ->
  W.StackSet i l a s sd
skipFloatingR ::
  (Eq a, Ord a) =>
  W.StackSet i l a s sd ->
  Maybe a ->
  (W.StackSet i l a s sd -> W.StackSet i l a s sd) ->
  W.StackSet i l a s sd
skipFloating stacks f
  | isNothing curr = stacks
  | -- short circuit if there is no currently focused window
    otherwise =
      skipFloatingR stacks curr f
  where
    curr = W.peek stacks
skipFloatingR stacks startWindow f
  | isNothing nextWindow = stacks
  | -- next window is nothing return current stack set
    nextWindow == startWindow =
      newStacks
  | -- if next window is the starting window then return the new stack set
    Map.notMember (fromJust nextWindow) (W.floating stacks) =
      newStacks
  | -- if next window is not a floating window return the new stack set
    otherwise =
      skipFloatingR newStacks startWindow f -- the next window is a floating window so keep recursing (looking)
  where
    newStacks = f stacks
    nextWindow = W.peek newStacks

-- | if the workspace is visible in some screen, then focus to this screen, else switch current screen to that workspace
switchOrFocus :: WorkspaceId -> X ()
switchOrFocus ws = switchOrFocusHelp ws 0
  where
    switchOrFocusHelp ws sc =
      screenWorkspace sc >>= \case
        Nothing -> windows $ W.greedyView ws
        Just x ->
          if x == ws
            then windows $ W.view x
            else switchOrFocusHelp ws (sc + 1)

runManageHook :: ManageHook -> Window -> X ()
runManageHook hook w = userCodeDef (Endo id) (runQuery hook w) >>= windows . appEndo

floatOnScreen :: (ScreenId -> W.RationalRect) -> ManageHook
floatOnScreen f =
  ask >>= \w -> doF $ \s -> do
    let sid = W.screen $ W.current s
    W.float w (f sid) s

-- | Returns True if the given RationalRect covers the entire screen
isFullScreen :: W.RationalRect -> Bool
isFullScreen (W.RationalRect x y w h) =
  x == 0
    && y == 0
    && w == 1
    && h == 1
