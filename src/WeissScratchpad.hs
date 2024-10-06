module WeissScratchpad where

import Config
import Control.Monad (liftM)
import Control.Monad.Extra (andM, firstJustM, unlessM, whenJustM, whenM)
import Control.Monad.Trans.Maybe
import Data.Foldable.Extra (findM)
import Data.Functor (void)
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import Data.Tree (Tree (Node))
import Utils
import XMonad
import XMonad.Actions.GridSelect (runSelectedAction)
import XMonad.Actions.ShowText (flashText)
import XMonad.Actions.TreeSelect (TSNode (..), treeselectAction)
import XMonad.Actions.WithAll (withAll)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Prelude (Endo (..), catMaybes, (<&>))
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad

termNSP, timeNSP, pavuNSP, scratchNSP :: NamedScratchpad
termNSP =
  NS
    "wezterm"
    (myTerminal <> " --config-file $XDG_CONFIG_HOME/wezterm/scratch.lua")
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
pavuNSP = NS "pavu" "pavucontrol" (className =? "pavucontrol") niceFloating

myScratchPads :: [NamedScratchpad]
myScratchPads = [termNSP, timeNSP, pavuNSP, scratchNSP]

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

-- try to float window in a way that do not overlap the currently focused window
niceFloating :: ManageHook
niceFloating = do
  let r = W.RationalRect
  isV <- liftX logIsVerticalScreen
  isM <- liftX logMaster
  layout <- liftX logLayout <&> fmap trimLayoutModifiers
  c <- liftX logWinCount <&> (\c -> c - 1) -- exclude the NSP window
  doRectFloat $ case (isV, isM, layout) of
    (False, False, Just l)
      | c == 3 && "ThreeCol" `isInfixOf` l -> r (33 / 100) (6 / 50) (32 / 100) (25 / 50)
    (False, False, Just l)
      | c == 2 && "ThreeCol" `isInfixOf` l ->
          r (1 / 100) (6 / 50) (30 / 100) (25 / 50)
    (False, _, _) -> r (66 / 100) (6 / 50) (33 / 100) (25 / 50)
    -- (_, Just "StackTile") -> r (1 / 50) (26 / 50) (45 / 50) (20 / 50)
    -- (True, Just "Mirror Tall") -> r (1 / 50) (26 / 50) (45 / 50) (20 / 50)
    -- (False, Just "Mirror Tall") -> r (1 / 50) (5 / 50) (45 / 50) (20 / 50)
    (True, True, _) -> r (1 / 50) (26 / 50) (46 / 50) (20 / 50)
    (True, False, _) -> r (1 / 50) (3 / 50) (46 / 50) (20 / 50)

newtype CurrentScratchpadName = CurrentScratchpadName String
instance ExtensionClass CurrentScratchpadName where
  initialValue = CurrentScratchpadName (head myScratchpadNames)

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
