module WeissScratchpad where

import Config
import Control.Monad (liftM)
import Control.Monad.Extra (andM, firstJustM)
import Control.Monad.Trans.Maybe
import Data.Foldable.Extra (findM)
import Data.Functor (void)
import Data.Maybe (isJust)
import Utils
import XMonad
import XMonad.Actions.ShowText (flashText)
import XMonad.Hooks.ManageHelpers
import XMonad.Prelude (Endo (..))
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad

termNSP, timeNSP, pavuNSP :: NamedScratchpad
termNSP =
  NS
    "term"
    (myTerminal <> " --config-file $XDG_CONFIG_HOME/wezterm/scratch.lua")
    (title ^=? "[Scratchpad]" <&&> (className =? "org.wezfurlong.wezterm"))
    niceFloating
timeNSP =
  NS
    "time-tracking"
    "emacs --file /home/weiss/Documents/notes/misc/notes/20240428T091509--time-tracking.org --title '[Scratchpad] time-tracking'"
    (title =? "[Scratchpad] time-tracking")
    floatTimeTracking
pavuNSP = NS "pavu" "pavucontrol" (className =? "Pavucontrol") niceFloating

myScratchPads :: [NamedScratchpad]
myScratchPads = [termNSP, timeNSP, pavuNSP]

myScratchpadNames :: [String]
myScratchpadNames = name <$> myScratchPads

myScratchPadsManageHook :: ManageHook
myScratchPadsManageHook = namedScratchpadManageHook myScratchPads

floatTimeTracking :: ManageHook
floatTimeTracking = floatOnScreen f
  where
    f 0 = W.RationalRect (20 / 50) (5 / 50) (25 / 50) (40 / 50)
    f _ = W.RationalRect (3 / 50) (5 / 50) (45 / 50) (35 / 50)

-- try to float window in a way that do not overlap the currently focused window
niceFloating :: ManageHook
niceFloating = do
  m <- liftX logMaster
  l <- liftX logLayout
  let r = W.RationalRect
  doRectFloat $ case (m, trimLayoutModifiers l) of
    (_, Just "StackTile") -> r (1 / 50) (26 / 50) (45 / 50) (20 / 50)
    (True, Just "Mirror Tall") -> r (1 / 50) (26 / 50) (45 / 50) (20 / 50)
    (False, Just "Mirror Tall") -> r (1 / 50) (5 / 50) (45 / 50) (20 / 50)
    (True, _) -> r (26 / 50) (6 / 50) (23 / 50) (20 / 50)
    (False, _) -> r (1 / 50) (6 / 50) (23 / 50) (20 / 50)

newtype CurrentScratchpadName = CurrentScratchpadName String
instance ExtensionClass CurrentScratchpadName where
  initialValue = CurrentScratchpadName (head myScratchpadNames)

findNSP :: Window -> X (Maybe NamedScratchpad)
findNSP w = findM (\(NS {..}) -> runQuery query w) myScratchPads

isNSP :: Window -> X Bool
isNSP w = isJust <$> findNSP w

-- reposition the focus window by its ManageHook if it is a NSP
repositionNSP :: X ()
repositionNSP = withFocused $ \w -> void $ runMaybeT $ do
  NS {..} <- MaybeT (findNSP w)
  liftMaybeT $ runManageHook hook w

curNSP_ :: X ()
curNSP_ = do
  CurrentScratchpadName cur <- XS.get
  namedScratchpadAction myScratchPads cur

curNSP :: X ()
curNSP = curNSP_ >> repositionNSP

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
