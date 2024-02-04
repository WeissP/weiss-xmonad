module WorkspaceFamily (myWorkspaces, workspaceKeys) where

import Control.Monad (when)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.List (elemIndex, find, isPrefixOf)
import Data.List.Extra (cons, snoc)
import Data.Maybe.Utils (forceMaybe)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Printf (printf)
import WeissNamedScratchpad
import qualified WeissWindowOperations as Op
import XMonad
import XMonad.Actions.WorkspaceNames (getCurrentWorkspaceName, getWorkspaceName)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

myWorkspaces :: [WorkspaceId]
myWorkspaces =
  (nodeName <$> singles)
    <> [f `idWithMember` m | f <- M.elems allFamilies, m <- allFamilyMembers]

type Key = String
newtype RootName = RootName String deriving stock (Eq, Generic)
instance Hashable RootName

type FamilyName = String
familyNames :: [FamilyName]
familyNames = show <$> [1 :: Int .. 9]

allFamilies :: HashMap FamilyName Family
allFamilies =
  M.fromList $
    [1 .. 9]
      <&> \n ->
        let ns = show n
            pref = ns <> "."
            f =
              Family
                ns
                (numToKey n)
                (\(FamilyMember sub) -> pref <> show sub)
                (pref `isPrefixOf`)
         in (ns, f)

currentFamilyMember :: FamilyName -> X FamilyMember
currentFamilyMember name =
  XS.get <&> \(FamilyStore hm) -> forceMaybe $ M.lookup name hm

currentFamilyOrder :: X FamilyOrder
currentFamilyOrder = fromMaybeM (pure 1) $ runMaybeT $ do
  ws <- MaybeT getCurrentWorkspaceName
  f <- MaybeT $ pure $ find (`hasWorkspace` ws) (M.elems allFamilies)
  MaybeT $ pure $ elemIndex (fName f) familyNames

data Family = Family
  { fName :: FamilyName
  , fKey :: Key
  , idWithMember :: FamilyMember -> WorkspaceId
  , hasWorkspace :: WorkspaceId -> Bool
  }
instance Node Family where
  fullID (Family {..}) = currentFamilyMember fName <&> idWithMember
  onActivate (Family {..}) = do
    wantIdx <- currentFamilyOrder
    XS.modify $ \(OrderedFamilies fs) ->
      OrderedFamilies $
        let curIdx = forceMaybe $ elemIndex fName fs
         in swapElementsAt wantIdx curIdx fs
  nodeName (Family {..}) = fName
  nodeKey (Family {..}) = fKey

type FamilyOrder = Int -- zero based
newtype OrderedFamilies = OrderedFamilies [FamilyName] deriving newtype (Show)
instance ExtensionClass OrderedFamilies where
  initialValue = OrderedFamilies familyNames

nthFamilyName :: FamilyOrder -> X FamilyName
nthFamilyName idx = do
  (OrderedFamilies fs) <- XS.get
  return $ fs !! idx

nthFamily :: FamilyOrder -> X Family
nthFamily idx = forceMaybe . flip M.lookup allFamilies <$> nthFamilyName idx

-- one based
newtype FamilyMember = FamilyMember Int deriving newtype (Show)
instance Node (FamilyOrder, FamilyMember) where
  fullID (idx, m) = nthFamily idx <&> (`idWithMember` m)
  onActivate (idx, m) = do
    name <- nthFamilyName idx
    XS.modify $ \(FamilyStore hm) -> FamilyStore $ M.insert name m hm
  nodeName (_, FamilyMember m) = show m
  nodeKey (idx, FamilyMember m) = numToKey $ m + [0, 6, 9] !! idx
allFamilyMembers :: [FamilyMember]
allFamilyMembers = FamilyMember <$> [1 .. 6]

newtype FamilyStore = FamilyStore (HashMap FamilyName FamilyMember)
instance ExtensionClass FamilyStore where
  initialValue = FamilyStore $ M.fromList $ (,FamilyMember 1) <$> familyNames

data Single = Single {sName :: String, sKey :: Key}

instance Node Single where
  fullID = pure . sName
  onActivate _ = pure ()
  nodeKey = sKey
  nodeName = sName
singles :: [Single]
singles =
  uncurry Single
    <$> [ ("览", "<Down>")
        , ("邮", "h")
        , ("音", "<Up>")
        , ("娱", "n")
        , ("聊", "y")
        , (scratchpadWorkspaceTag, "0")
        ]

workspaceKeys' :: [([String], X ())]
workspaceKeys' =
  [ op prefix effect
  | op <- make singles <> make fMembers <> make (M.elems allFamilies)
  , (prefix, effect) <-
      [([], switch), (["<Escape>"], shift), (["<Space>"], shift <> switchOrFocus)]
  ]
  where
    toPair n effPrefix eff = (nodeKeyPrefix n <> effPrefix `snoc` nodeKey n, eff `runOn` n)
    -- make :: (Node a) => [String] -> [a] -> [[String] -> WsEffect -> ([String], X ())]
    make nodes = toPair <$> nodes
    fMembers :: [(FamilyOrder, FamilyMember)] =
      (allFamilyMembers <&> (1,))
        <> (take 3 allFamilyMembers <&> (2,))
        <> (take 3 allFamilyMembers <&> (3,))
    switch = WsEffect (windows . W.greedyView) True
    shift = WsEffect (windows . W.shift) False
    switchOrFocus = WsEffect Op.switchOrFocus True

workspaceKeys :: [(String, X ())]
workspaceKeys = fmap (first $ unwords . cons "<XF86Launch7>") workspaceKeys'

data WsEffect = WsEffect {runWsEffect :: WorkspaceId -> X (), mayActiviate :: Bool}
instance Semigroup WsEffect where
  x <> y = WsEffect (runWsEffect x >> runWsEffect y) (mayActiviate x || mayActiviate y)

class Node n where
  fullID :: n -> X WorkspaceId
  onActivate :: n -> X ()
  runOn :: WsEffect -> n -> X ()
  (WsEffect {..}) `runOn` n = when mayActiviate (onActivate n) >> fullID n >>= runWsEffect
  nodeKeyPrefix :: n -> [Key]
  nodeKeyPrefix _ = []
  nodeKey :: n -> Key
  nodeName :: n -> String

numToKey :: Int -> String
numToKey s = ["m", ",", ".", "j", "k", "l", "u", "i", "o", "\\", "-", "\""] !! (s - 1)

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt a b list = if a < b then help a b else help b a
  where
    help front back = list1 <> [list !! back] <> list2 <> [list !! front] <> list3
      where
        list1 = take front list
        list2 = drop (succ front) (take back list)
        list3 = drop (succ back) list
