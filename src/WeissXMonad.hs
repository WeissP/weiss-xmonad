{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
module WeissXMonad (runXmonad, module XMonad) where

import Config
import Data.List
import Data.Maybe
import System.IO (hPutStrLn)
import Text.Regex
import TreeActions
import Utils
import WeissPromptPass
import WeissScratchpad
import WeissWindowOperations
import WeissXmobar
import WorkspaceFamily
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.EasyMotion (EasyMotionConfig (..), selectWindow)
import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Actions.MouseResize
import XMonad.Actions.ShowText (flashText, handleTimerEvent)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.StatusBar
import XMonad.Layout.Accordion
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MouseResizableTile (MouseResizableTile (..), mouseResizableTile)
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.ResizableThreeColumns (ResizableThreeCol (..))
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
import XMonad.Layout.ThreeColumns (ThreeCol (..))
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import XMonad.Prompt (
  XPConfig (..),
  XPPosition (..),
  font,
  height,
  position,
 )
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.EZConfig (parseKey, parseKeyCombo)
import XMonad.Util.Loggers
import XMonad.Util.Parser (runParser)
import XMonad.Util.Paste
import XMonad.Util.Run (
  runInTerm,
  runProcessWithInput,
  safeSpawn,
  spawnPipe,
 )
import XMonad.Util.Ungrab

mylogLayout :: Logger
mylogLayout = withWindowSet $ return . Just . ld
  where
    ld = description . W.layout . W.workspace . W.current

-- Gaps around and between windows
-- Changes only seem to apply if I log out then in again
-- Dimensions are given as (Border top bottom right left)
mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing =
  spacingRaw
    True -- Only for >1 window
    -- The bottom edge seems to look narrower than it is
    (Border 0 0 0 0) -- Size of screen edge gaps
    True -- Enable screen edge gaps
    (Border 5 5 5 5) -- Size of window gaps
    True -- Enable window gaps

-- prompt config
myXPConfig :: XPConfig
myXPConfig =
  def
    { position = CenteredAt 0.5 0.5
    , font = "xft:DejaVu Sans:size=9"
    , height = 40
    , autoComplete = Just 800
    }

myLayout =
  avoidStruts $
    mySpacing $
      smartBorders $
        mouseResize $
          windowArrange $
            ifWider
              1500
              (threeCol ||| Full)
              (Mirror threeCol ||| mouseResizableTile {isMirrored = True, masterFrac = 0.7} ||| Full)
  where
    threeCol = ResizableThreeColMid 1 (3 / 100) (-(1 / 3)) []
    myMulCol = multiCol [1, 1] 0 0.01 (-0.5)
    twoPane = TwoPane delta ratio
    myTall = Tall nmaster delta ratio
    myStackTile = StackTile 1 (3 / 100) (4 / 9)
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myKeys :: [([Char], X ())]
myKeys =
  [
    ( "<XF86Launch5>"
    , spawn
        "rofi -m -4 -no-lazy-grab -run-command \"zsh -i -c '{cmd}'\" -show run"
    )
  , ("<XF86Launch8>", weissTreeActions)
  , ("<F6>", curNSP)
  , ("<F11>", withFocused toggleFloat)
  , ("<XF86Launch6>", weissSwap)
  , ("M-<Escape>", kill)
  , ("M-1", weissSwitchRecent)
  , ("M-2", weissSwitchFocus)
  , ("M-<Left>", sendMessage Shrink)
  , ("M-<Right>", sendMessage Expand)
  , ("M-k", spawn myTerminal)
  ,
    ( "M-p"
    , spawn "rofi -m -4 -no-lazy-grab -run-command \"zsh -i -c '{cmd}'\" -show run"
    )
    -- , ("C-<Tab>"      , unGrab *> spawn "xdotool key Control_L+Tab")
    -- , ("C-<Tab>"      , myFocusDown)
    -- , ("M-4"          , moveFloat $ namedScratchpadAction myScratchPads "tmux")
  ]
    <> workspaceKeys
    -- ++ [ ("M-4 " ++ key, fun)
    --    | (key, fun) <-
    --         [ ("v", spawnHereNamedScratchpadAction myScratchPads "pavu")
    --         , ("t", windows (`skipFloating` W.focusDown))
    --         ]
    --    ]
    <> [ ("<XF86Launch7> " <> key, fun)
       | (key, fun) <-
          [ ("t", sendMessage NextLayout)
          , -- , ("e", pure ())
            ("r", spawn "xmonad --restart")
          , ("v", spawn "sh $HOME/.screenlayout/vertical.sh")
          , ("b", spawn "sh $HOME/.screenlayout/horizontal.sh")
          , ("s", spawn "flameshot gui")
          , -- ("f", spawn "fcitx-remote -s fcitx-keyboard-de-nodeadkeys"),
            ("w", spawn "$SCRIPTS_DIR/notify_window_title.sh")
          , ("p", mkPassPrompt "select pass" sendToClj myXPConfig)
          , ("M-2", nextScreen)
          -- , ("h"      , spawn "rofi-pass")
          -- ("<Left>", sendMessage $ Move L),
          -- ("<Right>", sendMessage $ Move R),
          -- ("<Up>", sendMessage $ Move U),
          -- ("<Down>", sendMessage $ Move D)
          ]
       ]

myManageHook :: ManageHook
myManageHook =
  myScratchPadsManageHook
    <> composeAll
      ( concat
          [ [isDialog --> doFloat]
          , -- , [className =? "vivaldi-stable" --> doShift "览"]
            [className =? "Mattermost" --> doShift "聊2"]
          , [className =? "p3x-onenote" --> doShift "记3"]
          , [className =? x --> doIgnore | x <- myIgnoreClass]
          , [className =? x --> doHideIgnore | x <- myHideIgnoreClass]
          , [className =? x --> doCenterFloat | x <- myCenterFloatClass]
          , [title =? x --> doCenterFloat | x <- myCenterFloatTitle]
          , [title *=? x --> doCenterFloat | x <- myCenterFloatTitleReg]
          , [className *=? x --> doFullFloat | x <- myFullFloatClass]
          ]
      )
  where
    (*=?) :: (Functor f) => f String -> String -> f Bool
    q *=? x =
      let matchReg a b = isJust $ matchRegex (mkRegex a) b
       in fmap (matchReg x) q
    myIgnoreClass = ["trayer"]
    myHideIgnoreClass = ["Blueman-applet"]
    myCenterFloatClass =
      ["Blueman-manager", "zoom", "Pavucontrol", "SimpleScreenRecorder"]
    myCenterFloatTitle = ["tmux-Scratchpad", "flameshot"]
    myCenterFloatTitleReg = []
    myFullFloatClass = ["MPlayer", "mpv", "steam_app_.*"]
    netName = stringProperty "_NET_WM_NAME"

myConfig =
  def
    { modMask = myModMask
    , terminal = myTerminal
    , manageHook = myManageHook
    , workspaces = myWorkspaces
    , logHook = historyHook >> updatePointer (0.95, 0.5) (0, 0)
    , borderWidth = myBorderWidth
    , layoutHook = myLayout
    , normalBorderColor = myNormColor
    , focusedBorderColor = myFocusColor
    , -- return () to avoid infinite mutual recursion
      startupHook = return () >> checkKeymap myConfig myKeys >> scratchpadsExclusives >> spawn "systemctl --user start autostart.target"
    , handleEventHook = handleEventHook def <> handleTimerEvent
    , focusFollowsMouse = True
    }
    -- `removeKeysP` ["M-4"]
    `additionalKeysP` myKeys

runXmonad :: String -> IO ()
runXmonad xmobarDir = do
  xmonad $
    ewmhFullscreen $
      ewmh $
        withEasySB (xmobarVertical xmobarDir <> xmobarHori xmobarDir) defToggleStrutsKey $
          docks myConfig
