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
import Data.List (elemIndex, find, intercalate, intersperse, isPrefixOf, sort)
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

myWorkspaces :: [WorkspaceId]
myWorkspaces =
  (nodeName <$> singles)
    <> [f `idWithMember` m | f <- M.elems allFamilies, m <- allFamilyMembers]

type Key = String
newtype RootName = RootName String deriving stock (Eq, Generic)
instance Hashable RootName

type FamilyName = String
familyNames :: [FamilyName]
familyNames = sort $ M.keys allFamilies

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

getFamily :: FamilyName -> Family
getFamily name = forceMaybe $ M.lookup name allFamilies

currentFamilyMember :: FamilyName -> X FamilyMember
currentFamilyMember name =
  XS.get <&> \(FamilyStore hm) -> forceMaybe $ M.lookup name hm

currentFamily :: X Family
currentFamily = fromMaybeM (pure $ head $ M.elems allFamilies) $ runMaybeT $ do
  ws <- MaybeT logCurrent
  MaybeT $ pure $ find (`hasWorkspace` ws) (M.elems allFamilies)

logWorkspaceFamilies :: Logger
logWorkspaceFamilies = do
  OrderedFamilies names <- XS.get
  let fs = mapMaybe (`M.lookup` allFamilies) names
  ids <- traverse fullID fs
  return $ Just $ intercalate "→" (take 3 ids)

data Family = Family
  { fName :: FamilyName
  , fKey :: Key
  , idWithMember :: FamilyMember -> WorkspaceId
  , hasWorkspace :: WorkspaceId -> Bool
  }
instance Node Family where
  fullID (Family {..}) = currentFamilyMember fName <&> idWithMember
  onActivate this = do
    wantF <- currentFamily
    swapOrderedFamilies (fName this) (fName wantF)

  nodeName (Family {..}) = fName
  nodeKeyPrefix _ = ["<Return>"]
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

-- 1 based
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

newtype CurrentFamilyMember = CurrentFamilyMember FamilyMember
  deriving newtype (Show)
instance Node CurrentFamilyMember where
  fullID (CurrentFamilyMember m) = currentFamily <&> (`idWithMember` m)
  onActivate _ = pure ()
  nodeName = show
  nodeKey (CurrentFamilyMember (FamilyMember m)) = numToKey m

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
        , ("媒", "<Up>")
        , ("聊", "y")
        , (scratchpadWorkspaceTag, "0")
        ]

workspaceKeys' :: [([String], X ())]
workspaceKeys' =
  [ op prefix effect
  | op <- make singles <> make fMembers <> make (M.elems allFamilies)
  , (prefix, effect) <-
      [ ([], switch)
      , (["<Escape>"], shift)
      , (["<Space>"], shift <> switchOrFocus)
      ]
  ]
    <> [(["g"], logWorkspaceFamilies >>= flashText def 2 . show)]
    <> [ ( ["<End>"] <> nodeKeyPrefix f `snoc` nodeKey f
         , swapWithCurrentFamily (fName f) >>= (switch `runOn`)
         )
       | f <- M.elems allFamilies
       ]
    <> [ toPair n ["<End>"] swap
       | n <- CurrentFamilyMember <$> allFamilyMembers
       ]
  where
    toPair n effPrefix eff =
      ( nodeKeyPrefix n <> effPrefix `snoc` nodeKey n
      , eff `runOn` n
      )
    make nodes = toPair <$> nodes
    fMembers :: [(FamilyOrder, FamilyMember)] =
      (allFamilyMembers <&> (0,))
        <> (take 3 allFamilyMembers <&> (1,))
        <> (take 1 allFamilyMembers <&> (2,))
    switch = WsEffect (windows . W.greedyView) True
    shift = WsEffect (windows . W.shift) False
    switchOrFocus = WsEffect Op.switchOrFocus True
    swap = WsEffect (windows . swapWithCurrent) True

workspaceKeys :: [(String, X ())]
workspaceKeys = fmap (first $ unwords . cons "<XF86Launch7>") workspaceKeys'

data WsEffect = WsEffect {runWsEffect :: WorkspaceId -> X (), mayActiviate :: Bool}
instance Semigroup WsEffect where
  x <> y =
    WsEffect
      (\ws -> runWsEffect x ws >> runWsEffect y ws)
      (mayActiviate x || mayActiviate y)

class Node n where
  fullID :: n -> X WorkspaceId
  onActivate :: n -> X ()
  runOn :: WsEffect -> n -> X ()
  (WsEffect {..}) `runOn` n = when mayActiviate (onActivate n) >> fullID n >>= runWsEffect
  nodeKeyPrefix :: n -> [Key]
  nodeKeyPrefix _ = []
  nodeKey :: n -> Key
  nodeName :: n -> String

swapFamilies :: FamilyName -> FamilyName -> X ()
swapFamilies a b = traverse_ swapPair pairs >> swapOrderedFamilies a b
  where
    allMembers name = allFamilyMembers <&> (getFamily name `idWithMember`)
    pairs = zip (allMembers a) (allMembers b)
    swapPair (x, y) = windows (swapWorkspaces x y)

-- swap the given family with current family, return the replaced family
swapWithCurrentFamily :: FamilyName -> X Family
swapWithCurrentFamily name = do
  target <- currentFamily
  swapFamilies name (fName target)
  return target

numToKey :: Int -> String
numToKey s = ["m", ",", ".", "j", "k", "l", "u", "i", "o", "-"] !! (s - 1)

swapOrderedFamilies :: FamilyName -> FamilyName -> X ()
swapOrderedFamilies a b = XS.modify $ \(OrderedFamilies fs) -> OrderedFamilies $ swapElements a b fs

swapElements :: (Eq a) => a -> a -> [a] -> [a]
swapElements _ _ [] = []
swapElements n m (x : xs)
  | n == x = m : swapElements n m xs
  | m == x = n : swapElements n m xs
  | otherwise = x : swapElements n m xs
