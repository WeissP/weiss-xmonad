module WorkspaceFamily (myWorkspaces, workspaceKeys, logWorkspaceFamilies) where

import Control.Monad (when)
import Control.Monad.Extra (fromMaybeM, pureIf)
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable
import Data.List (
  elemIndex,
  find,
  intercalate,
  intersperse,
  isPrefixOf,
  singleton,
  sort,
 )
import Data.List qualified as L
import Data.List.Extra (cons, firstJust, snoc)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Maybe.Utils (forceMaybe)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Utils
import WeissWindowOperations qualified as Op
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.ShowText (flashText)
import XMonad.Actions.SwapWorkspaces (swapWithCurrent, swapWorkspaces)
import XMonad.Actions.WorkspaceNames (getCurrentWorkspaceName, getWorkspaceName)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (parseKey, parseKeyCombo)
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

type Key = String
type FamilyName = String
type FamilyNum = Int

newtype NumberedFamily = NumberedFamily Int deriving newtype (Show, Eq)
allNumberedFamilies = NumberedFamily <$> [1 .. 9]
instance Family NumberedFamily where
  fName = show
  fPrefix nf = fName nf <> "."
  idWithMember nf = (fPrefix nf <>) . show
  hasWorkspace f ws = fPrefix f `isPrefixOf` ws
instance Node NumberedFamily where
  workspaceID = fullID
  onActivate n = currentNumberedFamily >>= swapOrderedNumFamilies n
  nodeKeys (NumberedFamily n) = ["<Return>", numToKey n]

currentNumberedFamily :: X NumberedFamily
currentNumberedFamily = fromMaybeM (pure $ head allNumberedFamilies) $ runMaybeT $ do
  ws <- MaybeT logCurrent
  MaybeT $ pure $ find (`hasWorkspace` ws) allNumberedFamilies

swapOrderedNumFamilies :: NumberedFamily -> NumberedFamily -> X ()
swapOrderedNumFamilies a b = XS.modify $ \(OrderedNumFamilies fs) -> OrderedNumFamilies $ swapElements a b fs

data LabeledFamily = LabeledFamily FamilyName Key deriving (Show)
allLabeledFamilies =
  uncurry LabeledFamily
    <$> [ ("览", "<Down>")
        , ("邮", "h")
        , ("媒", "<Up>")
        , ("聊", "y")
        , ("设", "n")
        , ("记", "-")
        , (scratchpadWorkspaceTag, "0")
        ]

instance Family LabeledFamily where
  fName (LabeledFamily name _) = name
  fPrefix = fName
  idWithMember f (FamilyMember 1) = fPrefix f
  idWithMember f m = fPrefix f <> show m

instance Node LabeledFamily where
  workspaceID = fullID
  onActivate _ = pure ()
  nodeKeys (LabeledFamily _ key) = [key]

class Family a where
  fName :: a -> FamilyName
  fPrefix :: a -> String
  idWithMember :: a -> FamilyMember -> WorkspaceId
  hasWorkspace :: a -> WorkspaceId -> Bool
  hasWorkspace f ws = fPrefix f `isPrefixOf` ws
  fullID :: a -> X WorkspaceId
  fullID f = storedFamilyMember (fName f) <&> idWithMember f

data GFamily = forall a. (Family a) => GFamily a
instance Family GFamily where
  fName (GFamily f) = fName f
  fPrefix (GFamily f) = fPrefix f
  idWithMember (GFamily f) = idWithMember f

allFamilies :: [GFamily]
allFamilies = (allLabeledFamilies <&> GFamily) <> (allNumberedFamilies <&> GFamily)

allFamiliyNames :: [FamilyName]
allFamiliyNames = allFamilies <&> fName

currentFamily :: X GFamily
currentFamily = fromMaybeM (pure $ head allFamilies) $ runMaybeT $ do
  ws <- MaybeT logCurrent
  MaybeT $ pure $ find (`hasWorkspace` ws) allFamilies

newtype FamilyMember = FamilyMember Int deriving newtype (Show)
allFamilyMembers = FamilyMember <$> [1 .. 9]
instance Node FamilyMember where
  workspaceID m = currentFamily <&> (`idWithMember` m)
  onActivate m = currentFamily >>= (`updateFamilyStore` m) . fName
  nodeKeys (FamilyMember m) = ["<End>", numToKey m]

instance Node (FamilyOrder, FamilyMember) where
  workspaceID (idx, m) = nthNumberedFamily idx <&> (`idWithMember` m)
  onActivate (idx, m) = nthNumberedFamily idx >>= (`updateFamilyStore` m) . fName
  nodeKeys (idx, FamilyMember m) = singleton $ numToKey $ m + [0, 6, 9] !! idx

instance Node (LabeledFamily, FamilyMember) where
  workspaceID = pure . uncurry idWithMember
  onActivate (f, m) = updateFamilyStore (fName f) m
  nodeKeys (f, FamilyMember m) = "<End>" : nodeKeys f `snoc` numToKey m

class Node n where
  workspaceID :: n -> X WorkspaceId
  onActivate :: n -> X ()
  runOn :: WsEffect -> n -> X ()
  (WsEffect {..}) `runOn` n = when mayActiviate (onActivate n) >> workspaceID n >>= runWsEffect
  nodeKeys :: n -> [Key]

data WsEffect = WsEffect {runWsEffect :: WorkspaceId -> X (), mayActiviate :: Bool}
instance Semigroup WsEffect where
  x <> y =
    WsEffect
      (\ws -> runWsEffect x ws >> runWsEffect y ws)
      (mayActiviate x || mayActiviate y)

type FamilyOrder = Int -- zero based
newtype OrderedNumFamilies = OrderedNumFamilies [NumberedFamily]
  deriving newtype (Show)
instance ExtensionClass OrderedNumFamilies where
  initialValue = OrderedNumFamilies allNumberedFamilies

newtype FamilyStore = FamilyStore (HashMap FamilyName FamilyMember)
instance ExtensionClass FamilyStore where
  initialValue = FamilyStore $ M.fromList ((,FamilyMember 1) <$> allFamiliyNames)

workspaceKeys' :: [([String], X ())]
workspaceKeys' =
  L.concat
    [ [op prefix effect, op ("<XF86Launch8>" : prefix) (onNextScreen <> effect)]
    | op <-
        make membersWithOrder
          <> make allFamilyMembers
          <> make allNumberedFamilies
          <> make allLabeledFamilies
          <> make labeldFamilyMember
    , (prefix, effect) <-
        [ ([], switch)
        , (["<Escape>"], shift)
        , (["<Space>"], shift <> switchOrFocusE)
        , (["<Home>"], swap)
        ]
    ]
  where
    toPair n effPrefix eff = (effPrefix <> nodeKeys n, eff `runOn` n)
    make nodes = toPair <$> nodes
    membersWithOrder :: [(FamilyOrder, FamilyMember)] =
      (take 6 allFamilyMembers <&> (0,)) <> (take 3 allFamilyMembers <&> (1,))
    labeldFamilyMember = [(f, m) | f <- allLabeledFamilies, m <- allFamilyMembers]
    switch = WsEffect (windows . W.greedyView) True
    shift = WsEffect (windows . W.shift) False
    switchOrFocusE = WsEffect switchOrFocus True
    swap = WsEffect (windows . swapWithCurrent) True
    onNextScreen = WsEffect (const nextScreen) True

workspaceKeys :: [(String, X ())]
workspaceKeys = fmap (first $ unwords . cons "<XF86Launch7>") workspaceKeys'

storedFamilyMember :: FamilyName -> X FamilyMember
storedFamilyMember name =
  XS.get <&> \(FamilyStore hm) -> forceMaybe $ M.lookup name hm

nthNumberedFamily :: FamilyOrder -> X NumberedFamily
nthNumberedFamily idx = do
  (OrderedNumFamilies fs) <- XS.get
  return $ fs !! idx

updateFamilyStore :: FamilyName -> FamilyMember -> X ()
updateFamilyStore name m = XS.modify $ \(FamilyStore hm) -> FamilyStore $ M.insert name m hm

swapElements :: (Eq a) => a -> a -> [a] -> [a]
swapElements _ _ [] = []
swapElements n m (x : xs)
  | n == x = m : swapElements n m xs
  | m == x = n : swapElements n m xs
  | otherwise = x : swapElements n m xs

myWorkspaces = [idWithMember f m | f <- allFamilies, m <- allFamilyMembers]

logWorkspaceFamilies :: Logger
logWorkspaceFamilies = do
  OrderedNumFamilies nums <- XS.get
  ids <- traverse workspaceID (take 2 nums)
  return $ Just $ intercalate "→" ids
