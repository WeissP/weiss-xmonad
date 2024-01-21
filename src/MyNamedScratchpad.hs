module MyNamedScratchpad where

import           XMonad
import           XMonad.Actions.SpawnOn         ( spawnHere )
import           XMonad.Hooks.DynamicLog        ( PP
                                                , ppSort
                                                )
import           XMonad.Hooks.ManageHelpers     ( doRectFloat )
import           XMonad.Hooks.RefocusLast       ( withRecentsIn )
import           XMonad.Prelude                 ( filterM
                                                , find
                                                , unless
                                                , when
                                                )

import qualified Data.List.NonEmpty            as NE

import           Control.Concurrent
import           Control.Monad
import qualified Data.Map                      as M
import           Data.Maybe
import           MyWindowOperations
import           System.Timeout
import qualified XMonad.StackSet               as W

-- | Single named scratchpad configuration
data NamedScratchpad = NS
    { name  :: String      -- ^ Scratchpad name
    , cmd   :: String      -- ^ Command used to run application
    , query :: Query Bool  -- ^ Query to find already running application
    , after :: Window -> X ()         -- ^ this function will be called after the scratchpad is shifted to the current workspace
    }

-- | Named scratchpads configuration
type NamedScratchpads = [NamedScratchpad]

existsNsp :: NamedScratchpads -> X Bool
existsNsp nsp = withWindowSet $ \winSet -> isJust <$> findNspFromWindows
    nsp
    (W.integrate' (W.stack . W.workspace . W.current $ winSet))

focusWithNsp :: X () -> NamedScratchpads -> X ()
focusWithNsp f scratchpads = withFocused $ \win -> do
    mShiftedScratchpads <- shiftBackAllNspFromCurrentWsp scratchpads
    case mShiftedScratchpads of
        []                 -> f
        shiftedScratchpads -> do
            f
            withFocused $ \focused -> do
                shiftHereAllNsp shiftedScratchpads
                focus focused

myFocusDownWithNSP :: NamedScratchpads -> X ()
myFocusDownWithNSP = focusWithNsp myFocusDownPure

myFocusUpWithNSP :: NamedScratchpads -> X ()
myFocusUpWithNSP = focusWithNsp myFocusUpPure

mySwapMasterWithNsp :: NamedScratchpads -> X ()
mySwapMasterWithNsp scratchpads = withFocused $ \win -> do
    mShiftedScratchpads <- shiftBackAllNspFromCurrentWsp scratchpads
    case mShiftedScratchpads of
        []                 -> mySwapMasterPure
        shiftedScratchpads -> withFocused $ \focused -> do
            shiftHereAllNsp shiftedScratchpads
            focus focused

findNsp :: NamedScratchpads -> Window -> X (Maybe NamedScratchpad)
findNsp []       a = return Nothing
findNsp (x : xs) a = do
    fromNsp <- runQuery (query x) a
    if fromNsp then return (Just x) else findNsp xs a

findNspFromWindows :: NamedScratchpads -> [Window] -> X (Maybe NamedScratchpad)
findNspFromWindows _           []       = return Nothing
findNspFromWindows scratchpads (a : as) = do
    mNsp <- findNsp scratchpads a
    case mNsp of
        Nothing  -> findNspFromWindows scratchpads as
        Just nsp -> return $ Just nsp

findNspCurrentWsp :: NamedScratchpads -> X (Maybe NamedScratchpad)
findNspCurrentWsp nsp = withWindowSet $ \winSet -> findNspFromWindows nsp
    $ W.integrate' (W.stack . W.workspace . W.current $ winSet)


-- | Finds named scratchpad configuration by name
findByName :: NamedScratchpads -> String -> Maybe NamedScratchpad
findByName c s = find ((s ==) . name) c

-- | Runs application which should appear in specified scratchpad
runApplication :: NamedScratchpad -> X ()
runApplication = spawn . cmd

-- | Runs application which should appear in a specified scratchpad on the workspace it was launched on
runApplicationHere :: NamedScratchpad -> X ()
runApplicationHere = spawnHere . cmd

-- | Action to pop up specified named scratchpad
namedScratchpadAction
    :: NamedScratchpads -- ^ Named scratchpads configuration
    -> String           -- ^ Scratchpad name
    -> X ()
namedScratchpadAction = customRunNamedScratchpadAction runApplication

-- | Action to pop up specified named scratchpad, initially starting it on the current workspace.
spawnHereNamedScratchpadAction
    :: NamedScratchpads           -- ^ Named scratchpads configuration
    -> String                     -- ^ Scratchpad name
    -> X ()
spawnHereNamedScratchpadAction =
    customRunNamedScratchpadAction runApplicationHere

-- | Action to pop up specified named scratchpad, given a custom way to initially start the application.
customRunNamedScratchpadAction
    :: (NamedScratchpad -> X ())  -- ^ Function initially running the application, given the configured @scratchpad@ cmd
    -> NamedScratchpads           -- ^ Named scratchpads configuration
    -> String                     -- ^ Scratchpad name
    -> X ()
customRunNamedScratchpadAction =
    someNamedScratchpadAction (\f ws -> f $ NE.head ws)

allNamedScratchpadAction :: NamedScratchpads -> String -> X ()
allNamedScratchpadAction = someNamedScratchpadAction mapM_ runApplication

-- | execute some action on a named scratchpad
someNamedScratchpadAction
    :: ((Window -> X ()) -> NE.NonEmpty Window -> X ())
    -> (NamedScratchpad -> X ())
    -> NamedScratchpads
    -> String
    -> X ()
someNamedScratchpadAction f runApp scratchpadConfig scratchpadName =
    case findByName scratchpadConfig scratchpadName of
        Just conf -> withWindowSet $ \winSet -> do
            matchingOnAll <- filterM (runQuery (query conf))
                                     (W.allWindows winSet)
            let nonMatchedOnCurrent = case NE.nonEmpty matchingOnAll of
                    Nothing -> do
                        runApp conf
                        -- waitRun 10000 conf
                    Just wins -> do
                        f (windows . W.shiftWin (W.currentTag winSet)) wins
                        after conf (NE.head wins)
            case W.stack . W.workspace . W.current $ winSet of
                Nothing     -> nonMatchedOnCurrent
                Just curStk -> do
                    isFocused         <- runQuery (query conf) (W.focus curStk)
                    matchingOnCurrent <- filterM (runQuery (query conf))
                                                 (W.integrate curStk)
                    case NE.nonEmpty matchingOnCurrent of
                        Nothing   -> nonMatchedOnCurrent
                        Just wins -> if isFocused
                            then shiftBack (W.focus curStk)
                            else focus (NE.head wins)

        Nothing -> return ()
  where
    waitRun :: Int -> NamedScratchpad -> X ()
    waitRun limit conf = if limit <= 0
        then return ()
        else withWindowSet $ \winSet -> do
            refresh
            matchingOnAll <- filterM (runQuery (query conf))
                                     (W.allWindows winSet)
            case NE.nonEmpty matchingOnAll of
                Nothing   -> waitRun (limit - 1) conf
                Just wins -> do
                    windows $ W.focusWindow (NE.head wins)
                    after conf (NE.head wins)

-- | Tag of the scratchpad workspace
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "板"

shiftBack :: Window -> X ()
shiftBack a = windows $ W.shiftWin scratchpadWorkspaceTag a

shiftBackAllNsp :: NamedScratchpads -> [Window] -> X NamedScratchpads
shiftBackAllNsp _           []       = return []
shiftBackAllNsp scratchpads (a : as) = do
    mScratchpad <- findNsp scratchpads a
    case mScratchpad of
        Nothing -> shiftBackAllNsp scratchpads as
        Just scratchpad ->
            shiftBack a >> fmap (scratchpad :) (shiftBackAllNsp scratchpads as)

shiftBackAllNspFromCurrentWsp :: NamedScratchpads -> X NamedScratchpads
shiftBackAllNspFromCurrentWsp scratchpads = withWindowSet $ \winSet ->
    shiftBackAllNsp
        scratchpads
        (W.integrate' (W.stack . W.workspace . W.current $ winSet))


shiftHereAllNsp :: NamedScratchpads -> X ()
shiftHereAllNsp scratchpads = foldr
    (\elem res -> namedScratchpadAction scratchpads $ name elem)
    (return ())
    scratchpads

-- | Shift some windows to the scratchpad workspace according to the
-- given function.  The workspace is created if necessary.
-- shiftToNSP :: [WindowSpace] -> ((Window -> X ()) -> X ()) -> X ()
-- shiftToNSP ws f = do
--     unless (any ((scratchpadWorkspaceTag ==) . W.tag) ws)
--         $ addHiddenWorkspace scratchpadWorkspaceTag
--     f (windows . W.shiftWin scratchpadWorkspaceTag)

-- | Transforms a workspace list containing the NSP workspace into one that
-- doesn't contain it. Intended for use with logHooks.
namedScratchpadFilterOutWorkspace :: [WindowSpace] -> [WindowSpace]
namedScratchpadFilterOutWorkspace =
    filter (\(W.Workspace tag _ _) -> tag /= scratchpadWorkspaceTag)
{-# DEPRECATED namedScratchpadFilterOutWorkspace "Use XMonad.Util.WorkspaceCompare.filterOutWs [scratchpadWorkspaceTag] instead" #-}

-- | Transforms a pretty-printer into one not displaying the NSP workspace.
--
-- A simple use could be:
--
-- > logHook = dynamicLogWithPP . namedScratchpadFilterOutWorkspace $ def
--
-- Here is another example, when using "XMonad.Layout.IndependentScreens".
-- If you have handles @hLeft@ and @hRight@ for bars on the left and right screens, respectively, and @pp@ is a pretty-printer function that takes a handle, you could write
--
-- > logHook = let log screen handle = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP . marshallPP screen . pp $ handle
-- >           in log 0 hLeft >> log 1 hRight
namedScratchpadFilterOutWorkspacePP :: PP -> PP
namedScratchpadFilterOutWorkspacePP pp =
    pp { ppSort = fmap (. namedScratchpadFilterOutWorkspace) (ppSort pp) }
{-# DEPRECATED namedScratchpadFilterOutWorkspacePP "Use XMonad.Hooks.DynamicLog.filterOutWsPP [scratchpadWorkspaceTag] instead" #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:
