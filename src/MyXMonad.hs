{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module MyXMonad (runXmonad) where

import Data.List
import Data.Maybe
import MyLogger
import MyNamedScratchpad
import MyPromptPass
import MyWindowOperations
import MyWorkspace
import MyXmobar
import System.IO (hPutStrLn)
import Text.Regex
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Layout.Accordion
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
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
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Paste
import XMonad.Util.Run (
    runInTerm,
    runProcessWithInput,
    safeSpawn,
    spawnPipe,
 )
import XMonad.Util.Ungrab

myTerminal = "wezterm"

myBorderWidth :: Dimension
myBorderWidth = 3 -- Sets border width for windows

myNormColor :: String
myNormColor = "#282c34" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#46d9ff" -- Border color of focused windows

myModMask :: KeyMask
myModMask = mod4Mask

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

myXPConfig :: XPConfig
myXPConfig =
    def
        { position = Top
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
                        ifWider 1500 (myMulCol ||| myTall ||| Full) (Mirror myTall ||| myStackTile ||| Full)
  where
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
    , ("<XF86Launch8>", nextScreen)
    , ("<F6>", spawnHereNamedScratchpadAction myScratchPads "term")
    , ("<F11>", withFocused toggleFloat)
    , ("<XF86Launch6>", mySwapMaster)
    , ("M-<Escape>", kill)
    , ("M-1", myFocusUp)
    , ("M-2", myFocusDown)
    , ("M-<Up>", sendMessage Shrink)
    , ("M-<Down>", sendMessage Expand)
    , ("M-k", spawn "wezterm")
    , ("M-4", spawnHereNamedScratchpadAction myScratchPads "pavu")
    ,
        ( "M-p"
        , spawn "rofi -m -4 -no-lazy-grab -run-command \"zsh -i -c '{cmd}'\" -show run"
        )
        -- , ("C-<Tab>"      , unGrab *> spawn "xdotool key Control_L+Tab")
        -- , ("C-<Tab>"      , myFocusDown)
        -- , ("M-4"          , moveFloat $ namedScratchpadAction myScratchPads "tmux")
    ]
        ++ workspaceKeys
        -- ++ [ ("M-4 " ++ key, fun)
        --    | (key, fun) <-
        --         [ ("v", spawnHereNamedScratchpadAction myScratchPads "pavu")
        --         , ("t", windows (`skipFloating` W.focusDown))
        --         ]
        --    ]
        ++ [ ("<XF86Launch7> " ++ key, fun)
           | (key, fun) <-
                [ ("t", sendMessage NextLayout)
                ,
                    ( "r"
                    , spawn
                        "xmonad --restart"
                    )
                , ("v", spawn "sh $HOME/.screenlayout/vertical.sh")
                , ("b", spawn "sh $HOME/.screenlayout/horizontal.sh")
                , ("s", spawn "flameshot gui")
                , -- ("f", spawn "fcitx-remote -s fcitx-keyboard-de-nodeadkeys"),
                  ("w", spawn "$SCRIPTS_DIR/notify_window_title.sh")
                , ("p", mkPassPrompt "select pass" sendToClj myXPConfig)
                -- , ("h"      , spawn "rofi-pass")
                -- ("<Left>", sendMessage $ Move L),
                -- ("<Right>", sendMessage $ Move R),
                -- ("<Up>", sendMessage $ Move U),
                -- ("<Down>", sendMessage $ Move D)
                ]
           ]

-- Query: starts with
(^=?) :: (Eq a) => Query [a] -> [a] -> Query Bool
q ^=? x = isPrefixOf x <$> q

myScratchPads :: [NamedScratchpad]
myScratchPads =
    [ NS
        "term"
        (myTerminal ++ " --config-file $XDG_CONFIG_HOME/wezterm/scratch.lua")
        (title ^=? "[Scratchpad]")
        moveFloat
    , NS "pavu" "pavucontrol" (className =? "Pavucontrol") moveFloat
    ]
  where
    moveFloat :: Window -> X ()
    moveFloat a = do
        m <- logMaster
        l <- logLayout
        case (m, trimLayoutModifiers l) of
            (_, Just "StackTile") ->
                windows $
                    W.float
                        a
                        (W.RationalRect (1 / 50) (26 / 50) (45 / 50) (20 / 50))
            (True, Just "Mirror Tall") ->
                windows $
                    W.float
                        a
                        (W.RationalRect (1 / 50) (26 / 50) (45 / 50) (20 / 50))
            (False, Just "Mirror Tall") ->
                windows $
                    W.float
                        a
                        (W.RationalRect (1 / 50) (5 / 50) (45 / 50) (20 / 50))
            (True, _) ->
                windows $
                    W.float
                        a
                        (W.RationalRect (26 / 50) (6 / 50) (23 / 50) (20 / 50))
            (False, _) ->
                windows $
                    W.float
                        a
                        (W.RationalRect (1 / 50) (6 / 50) (23 / 50) (20 / 50))

myManageHook :: ManageHook
myManageHook =
    composeAll
        ( concat
            [ [isDialog --> doFloat]
            , -- , [className =? "Chromium" --> doShift (getWorkspace 2)]
              -- , [className =? "Google-chrome" --> doShift (getWorkspace 3)]
              -- [className =? "Thunderbird" --> doShift "邮H"],
              [className =? "Cider" --> doShift "音"]
            , [className =? "Spotify" --> doShift "音"]
            , [className =? "Mattermost" --> doShift "聊"]
            , [className =? x --> doIgnore | x <- myIgnoreClass]
            , [className =? x --> doHideIgnore | x <- myHideIgnoreClass]
            , [className =? x --> doCenterFloat | x <- myCenterFloatClass]
            , [title =? x --> doCenterFloat | x <- myCenterFloatTitle]
            , [title *=? x --> doCenterFloat | x <- myCenterFloatTitleReg]
            , [className =? x --> doFullFloat | x <- myFullFloatClass]
            ]
        )
  where
    (*=?) :: (Functor f) => f String -> String -> f Bool
    q *=? x =
        let matchReg = \a b -> isJust $ matchRegex (mkRegex a) b
         in fmap (matchReg x) q
    myIgnoreClass = ["trayer"]
    myHideIgnoreClass = ["Blueman-applet"]
    myCenterFloatClass =
        ["Blueman-manager", "zoom", "Pavucontrol", "SimpleScreenRecorder"]
    myCenterFloatTitle = ["tmux-Scratchpad", "flameshot"]
    myCenterFloatTitleReg = []
    myFullFloatClass = ["MPlayer", "mpv"]
    netName = stringProperty "_NET_WM_NAME"

myConfig =
    def
        { modMask = myModMask
        , terminal = myTerminal
        , -- , startupHook        = myStartupHook
          manageHook = myManageHook
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , layoutHook = myLayout
        , normalBorderColor = myNormColor
        , focusedBorderColor = myFocusColor
        , -- , logHook            = myLogHook
          handleEventHook = handleEventHook def <+> fullscreenEventHook
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

myFocusUp, myFocusDown, mySwapMaster :: X ()
myFocusUp = myFocusUpWithNSP myScratchPads
myFocusDown = myFocusDownWithNSP myScratchPads
mySwapMaster = mySwapMasterWithNsp myScratchPads
