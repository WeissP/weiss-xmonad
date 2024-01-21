module MyWorkspace (myWorkspaces, workspaceKeys) where

import MyNamedScratchpad
import MyWindowOperations
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

newtype RootWorkspace = RootWorkspace {fromRootWorkspace :: Int} deriving (Show)

-- newtype GeneralWorkspace = GeneralWorkspace Int deriving (Show)
type SubWorkspace = Int

data Workspace = Id String | SubWorkspace Int deriving (Show)

instance ExtensionClass RootWorkspace where
    initialValue = RootWorkspace 1

rootWorkspaces :: [RootWorkspace]
rootWorkspaces = map RootWorkspace [1 .. 9]

subWorkspaces :: [SubWorkspace]
subWorkspaces = [1 .. 9]

commonWorkspaces :: [String]
commonWorkspaces = ["览", "邮", "泛", "音", "娱", "聊", scratchpadWorkspaceTag]

myWorkspaces :: [WorkspaceId]
myWorkspaces = [toWorkspaceId r s | r <- rootWorkspaces, s <- subWorkspaces] ++ commonWorkspaces

toWorkspaceId :: RootWorkspace -> SubWorkspace -> WorkspaceId
toWorkspaceId (RootWorkspace r) s = show r ++ "." ++ show s

getWorkspaceId :: SubWorkspace -> X WorkspaceId
getWorkspaceId s = (`toWorkspaceId` s) <$> XS.get

applyWorkspace :: (WorkspaceId -> X ()) -> Workspace -> X ()
applyWorkspace f (SubWorkspace s) = getWorkspaceId s >>= f
applyWorkspace f (Id id) = f id

switchWorkspace :: Workspace -> X ()
switchWorkspace = applyWorkspace (windows . W.greedyView)

shiftWorkspace :: Workspace -> X ()
shiftWorkspace = applyWorkspace (windows . W.shift)

shiftSwitchWorkspace :: Workspace -> X ()
shiftSwitchWorkspace = applyWorkspace shiftThenSwitchOrFocus

switchRootWorkspace :: RootWorkspace -> X ()
switchRootWorkspace r = XS.put r >> switchWorkspace (SubWorkspace 1)

workspaceKeys :: [(String, X ())]
workspaceKeys =
    let subWorkspaceKeys = ["m", ",", ".", "j", "k", "l", "u", "i", "o"]
        subWorkspacePairs = zip subWorkspaceKeys (map SubWorkspace subWorkspaces)
        commonWorkspacePairs = zip ["<Down>", "h", "n", "<Up>", "-", "y", "0"] (map Id commonWorkspaces)
     in [ (keyPrefix ++ " " ++ k, fun subWorkspace)
        | (k, subWorkspace) <- commonWorkspacePairs ++ subWorkspacePairs
        , (keyPrefix, fun) <-
            [ ("<XF86Launch7>", switchWorkspace)
            , ("<XF86Launch7> <Space>", shiftSwitchWorkspace)
            , ("<XF86Launch7> <Escape>", shiftWorkspace)
            ]
        ]
            ++ zip (map ("<XF86Launch7> <XF86Launch7> " ++) subWorkspaceKeys) (map switchRootWorkspace rootWorkspaces)
