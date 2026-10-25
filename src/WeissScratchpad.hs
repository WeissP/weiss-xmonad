module WeissScratchpad where

import Config
import Control.Monad (liftM)
import Control.Monad.Extra (andM, firstJustM, unlessM, whenJustM, whenM)
import Control.Monad.Trans.Maybe
import Data.Foldable (toList)
import Data.Foldable.Extra (findM)
import Data.Functor (void)
import Data.List (find, maximumBy, minimumBy)
import Data.Map qualified as M
import Data.Maybe (isJust, isNothing, maybeToList)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Tree (Tree (Node))
import Utils
import XMonad
import XMonad.Actions.GridSelect (runSelectedAction)
import XMonad.Actions.GroupNavigation (Direction (History), orderedWindowList)
import XMonad.Actions.ShowText (flashText)
import XMonad.Actions.TreeSelect (TSNode (..), treeselectAction)
import XMonad.Actions.WithAll (withAll)
import XMonad.Hooks.ManageDocks (calcGap)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Prelude (Endo (..), catMaybes, safeGetWindowAttributes, (<&>))
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Types (Direction2D (..))

termNSP, timeNSP, pavuNSP, scratchNSP :: NamedScratchpad
termNSP =
  NS
    "wezterm"
    (myTerminal <> " --config-file $XDG_CONFIG_HOME/wezterm/scratch.lua start --cwd $HOME/nix-config")
    (title ^=? "[Scratchpad]" <&&> (className =? "org.wezfurlong.wezterm"))
    niceFloating
timeNSP =
  NS
    "time-tracking"
    "Exec=GTK_IM_MODULE= QT_IM_MODULE= XMODIFIERS= emacs --file /home/weiss/Documents/notes/misc/notes/20240428T091509--time-tracking.org --title '[Scratchpad] time-tracking'"
    (title =? "[Scratchpad] time-tracking")
    floatTimeTracking
scratchNSP =
  NS
    "scratch"
    "Exec=GTK_IM_MODULE= QT_IM_MODULE= XMODIFIERS= emacs --title '[Scratchpad] scratch'"
    (title =? "[Scratchpad] scratch")
    niceFloating
pavuNSP = NS "pwvu" "pwvucontrol" (className =? "pwvucontrol") niceFloating

myScratchPads :: [NamedScratchpad]
myScratchPads = [termNSP, scratchNSP, pavuNSP]

myScratchpadNames :: [String]
myScratchpadNames = name <$> myScratchPads

scratchpadLogHook :: X ()
scratchpadLogHook = refocusLastLogHook >> nsSingleScratchpadPerWorkspace myScratchPads

myScratchPadsManageHook :: ManageHook
myScratchPadsManageHook = namedScratchpadManageHook myScratchPads

floatTimeTracking :: ManageHook
floatTimeTracking = do
  isV <- liftX logIsVerticalScreen
  doRectFloat $
    if isV
      then W.RationalRect (3 / 50) (5 / 50) (45 / 50) (35 / 50)
      else W.RationalRect (20 / 50) (5 / 50) (25 / 50) (40 / 50)

-- Float the window beside the focused tiled window, in the largest free
-- area of the screen, so it does not cover what is being worked on.
niceFloating :: ManageHook
niceFloating = do
  self <- ask
  sr <- liftX logFocusedScreenRect
  area <- liftX $ calcGap (S.fromList [U, D, L, R]) <&> ($ sr)
  target <- liftX $ focusedTiledRect self
  doRectFloat $ toScreenRational sr $ placeAside area target

-- Geometry of the focused tiled window on the current workspace, ignoring
-- the window being placed. Falls back to the most recently focused one, which
-- is what is left when a scratchpad was just summoned and took the focus.
focusedTiledRect :: Window -> X (Maybe Rectangle)
focusedTiledRect self = withWindowSet $ \ws -> do
  history <- toList <$> orderedWindowList History
  let tiled w = w /= self && w `elem` W.index ws && not (M.member w (W.floating ws))
  case find tiled (maybeToList (W.peek ws) <> history) of
    Nothing -> pure Nothing
    Just w -> fmap toRect <$> safeGetWindowAttributes w
  where
    toRect wa =
      let b = 2 * wa_border_width wa
       in Rectangle
            (fromIntegral (wa_x wa))
            (fromIntegral (wa_y wa))
            (fromIntegral (wa_width wa + b))
            (fromIntegral (wa_height wa + b))

-- Place a float of a preferred size inside @area@ without overlapping
-- @target@. Picks the largest of the free strips around the target that still
-- fits a reasonably sized window; otherwise anchors the float to the screen
-- edge that overlaps the target the least.
placeAside :: Rectangle -> Maybe Rectangle -> Rectangle
placeAside (Rectangle ax' ay' aw' ah') target =
  case target of
    Nothing -> mkRect anchored
    Just t -> case filter fits (fitIn <$> strips (clip t)) of
      [] -> mkRect $ minimumOn (overlap (clip t)) [anchored, anchored']
      cs -> mkRect $ maximumOn (\(_, _, w, h) -> w * h) cs
  where
    (ax, ay, aw, ah) = (toInteger ax', toInteger ay', toInteger aw', toInteger ah')
    isV = ah > aw
    margin = 10
    (pw, ph) = if isV then (aw * 92 `div` 100, ah * 40 `div` 100) else (aw * 43 `div` 100, ah * 70 `div` 100)
    (minW, minH) = if isV then (aw `div` 2, ah `div` 4) else (aw `div` 4, ah `div` 3)

    clip (Rectangle x y w h) =
      let x0 = max ax (toInteger x)
          y0 = max ay (toInteger y)
          x1 = min (ax + aw) (toInteger x + toInteger w)
          y1 = min (ay + ah) (toInteger y + toInteger h)
       in (x0, y0, max 0 (x1 - x0), max 0 (y1 - y0))

    strips (tx, ty, tw, th) =
      [ (tx + tw, ay, ax + aw - tx - tw, ah) -- right
      , (ax, ay, tx - ax, ah) -- left
      , (ax, ty + th, aw, ay + ah - ty - th) -- bottom
      , (ax, ay, aw, ty - ay) -- top
      ]

    fitIn (sx, sy, sw, sh) =
      let w = min pw (sw - 2 * margin)
          h = min ph (sh - 2 * margin)
       in (sx + (sw - w) `div` 2, sy + (sh - h) `div` 2, w, h)

    fits (_, _, w, h) = w >= minW && h >= minH

    (anchored, anchored')
      | isV =
          ( (ax + (aw - pw) `div` 2, ay + ah - ph - margin, pw, ph) -- bottom
          , (ax + (aw - pw) `div` 2, ay + margin, pw, ph) -- top
          )
      | otherwise =
          ( (ax + aw - pw - margin, ay + (ah - ph) `div` 2, pw, ph) -- right
          , (ax + margin, ay + (ah - ph) `div` 2, pw, ph) -- left
          )

    overlap (x0, y0, w0, h0) (x1, y1, w1, h1) =
      max 0 (min (x0 + w0) (x1 + w1) - max x0 x1)
        * max 0 (min (y0 + h0) (y1 + h1) - max y0 y1)

    mkRect (x, y, w, h) = Rectangle (fromInteger x) (fromInteger y) (fromInteger w) (fromInteger h)

    minimumOn f = minimumBy (comparing f)
    maximumOn f = maximumBy (comparing f)

-- Express an absolute rectangle relative to the screen rectangle, as
-- expected by 'doRectFloat'.
toScreenRational :: Rectangle -> Rectangle -> W.RationalRect
toScreenRational (Rectangle sx sy sw sh) (Rectangle x y w h) =
  W.RationalRect
    (fromIntegral (x - sx) / fromIntegral sw)
    (fromIntegral (y - sy) / fromIntegral sh)
    (fromIntegral w / fromIntegral sw)
    (fromIntegral h / fromIntegral sh)

newtype CurrentScratchpadName = CurrentScratchpadName String deriving newtype (Show, Read)
instance ExtensionClass CurrentScratchpadName where
  initialValue = CurrentScratchpadName (head myScratchpadNames)
  extensionType = PersistentExtension

findNSP :: Window -> X (Maybe NamedScratchpad)
findNSP w = findM (\(NS {..}) -> runQuery query w) myScratchPads

hideAllNSP :: X ()
hideAllNSP = withAll $ \w ->
  whenJustM
    (findNSP w)
    (\(NS name _ _ _) -> namedScratchpadAction myScratchPads name)

isNSP :: Window -> X Bool
isNSP w = isJust <$> findNSP w

-- reposition the focus window by its ManageHook if it is a NSP
repositionNSP :: X ()
repositionNSP = withFocused $ \w -> void $ runMaybeT $ do
  NS {..} <- MaybeT (findNSP w)
  liftMaybeT $ runManageHook hook w

scratchpadsExclusives :: X ()
scratchpadsExclusives = addExclusives [myScratchPads <&> name]

curNSP :: X ()
curNSP = do
  CurrentScratchpadName cur <- XS.get
  namedScratchpadAction myScratchPads cur
  resetFocusedNSP

initialNSP :: X ()
initialNSP = do
  let c@(CurrentScratchpadName name) = initialValue
  XS.put c
  namedScratchpadAction myScratchPads name

nextNSP :: X ()
nextNSP = do
  CurrentScratchpadName cur <- XS.get
  let next = dropWhile (/= cur) (cycle myScratchpadNames) !! 1
  namedScratchpadAction myScratchPads next
  XS.put (CurrentScratchpadName next)

prevNSP :: X ()
prevNSP = do
  CurrentScratchpadName cur <- XS.get
  let prev = dropWhile (/= cur) (cycle (reverse myScratchpadNames)) !! 1
  namedScratchpadAction myScratchPads prev
  XS.put (CurrentScratchpadName prev)

switchNSP :: String -> X ()
switchNSP name = do
  namedScratchpadAction myScratchPads name
  XS.put (CurrentScratchpadName name)
  resetFocusedNSP
