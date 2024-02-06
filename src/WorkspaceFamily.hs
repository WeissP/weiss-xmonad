module WorkspaceFamily (myWorkspaces, workspaceKeys, logWorkspaceFamilies) where

import Control.Monad (when)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
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
import Data.List.Extra (cons, snoc)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Maybe.Utils (forceMaybe)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Printf (printf)
import WeissNamedScratchpad
import qualified WeissWindowOperations as Op
import XMonad
import XMonad.Actions.ShowText (flashText)
import XMonad.Actions.SwapWorkspaces (swapWithCurrent, swapWorkspaces)
import XMonad.Actions.WorkspaceNames (getCurrentWorkspaceName, getWorkspaceName)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (parseKey, parseKeyCombo)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Loggers

type Key = String
type FamilyName = String
type FamilyNum = Int

data Family = NumFamily FamilyNum | LabeledFamily FamilyName Key deriving (Show)
allFamilyNums = [1 .. 9]
allFamilies =
  (NumFamily <$> allFamilyNums)
    <> ( uncurry LabeledFamily
          <$> [ ("览", "<Down>")
              , ("邮", "h")
              , ("媒", "<Up>")
              , ("聊", "y")
              , ("设", "n")
              , ("记", "-")
              , (scratchpadWorkspaceTag, "0")
              ]
       )
myWorkspaces = [idWithMember f m | f <- allFamilies, m <- allFamilyMembers]

data WsEffect = WsEffect {runWsEffect :: WorkspaceId -> X (), mayActiviate :: Bool}
instance Semigroup WsEffect where
  x <> y =
    WsEffect
      (\ws -> runWsEffect x ws >> runWsEffect y ws)
      (mayActiviate x || mayActiviate y)

type FamilyOrder = Int -- zero based
newtype OrderedFamilies = OrderedFamilies [FamilyNum] deriving newtype (Show)
instance ExtensionClass OrderedFamilies where
  initialValue = OrderedFamilies allFamilyNums

class Node n where
  fullID :: n -> X WorkspaceId
  onActivate :: n -> X ()
  runOn :: WsEffect -> n -> X ()
  (WsEffect {..}) `runOn` n = when mayActiviate (onActivate n) >> fullID n >>= runWsEffect
  nodeKeys :: n -> [Key]

instance Node Family where
  fullID f = storedFamilyMember (fName f) <&> idWithMember f
  onActivate (LabeledFamily _ _) = pure ()
  onActivate (NumFamily n) = currentFamilyNum >>= swapOrderedFamilies n
  nodeKeys (LabeledFamily _ k) = [k]
  nodeKeys (NumFamily n) = ["<Return>", numToKey n]

nthNumFamily :: FamilyOrder -> X FamilyNum
nthNumFamily idx = do
  (OrderedFamilies fs) <- XS.get
  return $ fs !! idx

newtype FamilyStore = FamilyStore (HashMap FamilyName FamilyMember)
instance ExtensionClass FamilyStore where
  initialValue = FamilyStore $ M.fromList ((,FamilyMember 1) . fName <$> allFamilies)

storedFamilyMember :: FamilyName -> X FamilyMember
storedFamilyMember name =
  XS.get <&> \(FamilyStore hm) -> forceMaybe $ M.lookup name hm

updateFamilyStore :: FamilyName -> FamilyMember -> X ()
updateFamilyStore name m = XS.modify $ \(FamilyStore hm) -> FamilyStore $ M.insert name m hm

currentFamily :: X Family
currentFamily = fromMaybeM (pure $ head allFamilies) $ runMaybeT $ do
  ws <- MaybeT logCurrent
  MaybeT $ pure $ find (`hasWorkspace` ws) allFamilies

currentFamilyNum :: X FamilyNum
currentFamilyNum =
  currentFamily >>= \case
    NumFamily n -> pure n
    LabeledFamily _ _ -> XS.get <&> \(OrderedFamilies fs) -> head fs

logWorkspaceFamilies :: Logger
logWorkspaceFamilies = do
  OrderedFamilies nums <- XS.get
  ids <- traverse (fullID . NumFamily) (take 3 nums)
  return $ Just $ intercalate "→" ids

-- 1 based
newtype FamilyMember = FamilyMember Int deriving newtype (Show)
allFamilyMembers = FamilyMember <$> [1 .. 6]

instance Node (FamilyOrder, FamilyMember) where
  fullID (idx, m) = nthNumFamily idx <&> (`idWithMember` m) . NumFamily
  onActivate (idx, m) = nthNumFamily idx >>= (`updateFamilyStore` m) . fName . NumFamily
  nodeKeys (idx, FamilyMember m) = singleton $ numToKey $ m + [0, 6, 9] !! idx

newtype CurrentFamilyMember = CurrentFamilyMember FamilyMember
  deriving newtype (Show)
instance Node CurrentFamilyMember where
  fullID (CurrentFamilyMember m) = currentFamily <&> (`idWithMember` m)
  onActivate (CurrentFamilyMember m) = currentFamily >>= (`updateFamilyStore` m) . fName
  nodeKeys (CurrentFamilyMember (FamilyMember m)) = ["<End>", numToKey m]

workspaceKeys' :: [([String], X ())]
workspaceKeys' =
  [ op prefix effect
  | op <-
      make membersWithOrder
        <> make (CurrentFamilyMember <$> allFamilyMembers)
        <> make allFamilies
  , (prefix, effect) <-
      [ ([], switch)
      , (["<Escape>"], shift)
      , (["<Space>"], shift <> switchOrFocus)
      , (["<Home>"], swap)
      ]
  ]
  where
    toPair n effPrefix eff = (effPrefix <> nodeKeys n, eff `runOn` n)
    make nodes = toPair <$> nodes
    membersWithOrder :: [(FamilyOrder, FamilyMember)] =
      (allFamilyMembers <&> (0,)) <> (take 3 allFamilyMembers <&> (1,))

    switch = WsEffect (windows . W.greedyView) True
    shift = WsEffect (windows . W.shift) False
    switchOrFocus = WsEffect Op.switchOrFocus True
    swap = WsEffect (windows . swapWithCurrent) True

workspaceKeys :: [(String, X ())]
workspaceKeys = fmap (first $ unwords . cons "<XF86Launch7>") workspaceKeys'

fName :: Family -> FamilyName
fName (NumFamily num) = show num
fName (LabeledFamily name _) = name

fPrefix :: Family -> String
fPrefix nf@(NumFamily _) = fName nf <> "."
fPrefix lf@(LabeledFamily _ _) = fName lf

idWithMember :: Family -> FamilyMember -> WorkspaceId
idWithMember (LabeledFamily name _) (FamilyMember 1) = name
idWithMember f m = fPrefix f <> show m

hasWorkspace :: Family -> WorkspaceId -> Bool
hasWorkspace f ws = fPrefix f `isPrefixOf` ws

numToKey :: Int -> String
numToKey s = ["m", ",", ".", "j", "k", "l", "u", "i", "o", "-"] !! (s - 1)

swapOrderedFamilies :: FamilyNum -> FamilyNum -> X ()
swapOrderedFamilies a b = XS.modify $ \(OrderedFamilies fs) -> OrderedFamilies $ swapElements a b fs

swapElements :: (Eq a) => a -> a -> [a] -> [a]
swapElements _ _ [] = []
swapElements n m (x : xs)
  | n == x = m : swapElements n m xs
  | m == x = n : swapElements n m xs
  | otherwise = x : swapElements n m xs
