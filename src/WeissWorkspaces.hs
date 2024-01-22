module WeissWorkspaces (myWorkspaces, workspaceKeys) where

import Data.Bifunctor (Bifunctor (first), second)
import Data.List
import Data.List.Extra (cons, snoc)
import WeissNamedScratchpad
import WeissWindowOperations
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

type NumWS = Int
numWorkspaces :: [NumWS]
numWorkspaces = [1 .. 9]

type SubWS = Int
subWorkspaces :: [SubWS]
subWorkspaces = numWorkspaces

freqWorkspaces :: [Workspace]
freqWorkspaces = FreqWS <$> ["览", "邮", "音", "娱", "聊", scratchpadWorkspaceTag]

newtype CommonSubWS
  = CommonSubWS SubWS
  deriving newtype (Show)
instance ExtensionClass CommonSubWS where
  initialValue = CommonSubWS $ head subWorkspaces

newtype NormalRootWS = NormalRootWS Int
  deriving newtype (Num, Show)

numToKey :: NumWS -> String
numToKey s = ["m", ",", ".", "j", "k", "l", "u", "i", "o"] !! (s - 1)

normalRootWorkspaces :: [NormalRootWS]
normalRootWorkspaces = NormalRootWS <$> numWorkspaces
instance ExtensionClass NormalRootWS where
  initialValue = head normalRootWorkspaces

data NormalWorkspace = NormalWorkspace
  {rootWS :: NormalRootWS, subWS :: SubWS}
  deriving (Show)

data Workspace
  = FreqWS String
  | NormalWS NormalRootWS SubWS
  | CommonWS CommonSubWS
  deriving (Show)

toWorkspaceId :: Workspace -> WorkspaceId
toWorkspaceId (FreqWS s) = s
toWorkspaceId (NormalWS root sub) = show root <> "." <> show sub
toWorkspaceId (CommonWS sub) = "通" <> show sub

data PartialWorkspace
  = FullWS Workspace
  | NormalSubWS SubWS
  | LastCommonWS
  deriving (Show)

toFullWorkspace :: PartialWorkspace -> X Workspace
toFullWorkspace (FullWS ws) = pure ws
toFullWorkspace (NormalSubWS sub) = (\root -> NormalWS root sub) <$> XS.get
toFullWorkspace LastCommonWS = CommonWS <$> XS.get

applyWorkspaceOp :: (WorkspaceId -> X ()) -> PartialWorkspace -> X ()
applyWorkspaceOp op p = toFullWorkspace p >>= op . toWorkspaceId

data WorkSpaceEffect = WseSwitch | WseShift | WseSwitchOrFocus deriving (Eq)

executeWSE :: WorkSpaceEffect -> PartialWorkspace -> X ()
executeWSE WseSwitch = applyWorkspaceOp (windows . W.greedyView)
executeWSE WseShift = applyWorkspaceOp (windows . W.shift)
executeWSE WseSwitchOrFocus = applyWorkspaceOp switchOrFocus

executeAllWSE :: PartialWorkspace -> [WorkSpaceEffect] -> X ()
executeAllWSE ws = mapM_ (flip executeWSE ws)

setValueIfSwitch ::
  (ExtensionClass v) => v -> PartialWorkspace -> [WorkSpaceEffect] -> X ()
setValueIfSwitch v ws effects =
  if elem WseSwitch effects
    then XS.put v >> wse
    else wse
  where
    wse = executeAllWSE ws effects

myWorkspaces :: [WorkspaceId]
myWorkspaces = toWorkspaceId <$> myWorkspaces'

myWorkspaces' :: [Workspace]
myWorkspaces' = freqWorkspaces <> normalWorkspaces <> commonWorkspaces
  where
    commonWorkspaces = CommonWS . CommonSubWS <$> numWorkspaces
    normalWorkspaces =
      [ NormalWS (NormalRootWS root) sub
      | root <- numWorkspaces
      , sub <- numWorkspaces
      ]

workspaceKeys :: [(String, X ())]
workspaceKeys = fmap (first $ unwords . cons "<XF86Launch7>") workspaceKeys'

workspaceKeys' :: [([String], X ())]
workspaceKeys' =
  [ (wsKey <> opKey <> [subWsKey], run op)
  | (wsKey, subWsKey, run) <-
      ([], "n", executeAllWSE LastCommonWS)
        : normalSubWSPairs
          <> freqWSPairs
          <> rootWSPairs
          <> commonWSPairs
  , (opKey, op) <-
      [ ([], [WseSwitch])
      , (["<Escape>"], [WseShift])
      , (["<Space>"], [WseShift, WseSwitch])
      ]
  ]
  where
    normalSubWSPairs =
      [([], numToKey ws, executeAllWSE (NormalSubWS ws)) | ws <- numWorkspaces]
    rootWSPairs =
      [ (["<Return>"], numToKey ws, setValueIfSwitch rootWs partWs)
      | ws <- numWorkspaces
      , let rootWs = NormalRootWS ws
      , let partWs = FullWS (NormalWS rootWs 1)
      ]
    commonWSPairs =
      [ (["<End>"], numToKey ws, setValueIfSwitch commonWs partWs)
      | ws <- numWorkspaces
      , let commonWs = CommonSubWS ws
      , let partWs = FullWS (CommonWS commonWs)
      ]
    freqWSPairs =
      zip3
        (repeat [])
        ["<Down>", "h", "<Up>", "-", "y", "0"]
        (executeAllWSE . FullWS <$> freqWorkspaces)
