module XMonad.Util.VisibleWindows (visibleWindowsOnScreen, visibleWindows) where

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import XMonad
import qualified XMonad.StackSet as W

-- | Returns True if the given RationalRect covers the entire screen
isFullScreen :: W.RationalRect -> Bool
isFullScreen (W.RationalRect x y w h) =
  x == 0
    && y == 0
    && w == 1
    && h == 1

{- |
Find all visible windows on a given screen.
If a full-screen floating window exists, it returns only that window,
treating all other windows on the same screen as invisible.
-}
visibleWindowsOnScreen :: ScreenId -> X [Window]
visibleWindowsOnScreen sid = do
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
visibleWindows :: X [Window]
visibleWindows = do
  XState {windowset = ws} <- get
  let sids = fmap W.screen (W.screens ws)
  concat <$> mapM visibleWindowsOnScreen sids
