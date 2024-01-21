module MyWorkspaces where

import MyNamedScratchpad
import MyWindowOperations
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

data WithSubWS a = WithSubWS {rootWS :: a, subWS :: Int} deriving (Show)

data Workspace = FreqWS String | NormalWS (WithSubWS Int) | CommonWS (WithSubWS Int) deriving (Show)
