module Config where

import XMonad

totalTitlesLength, unfocusedTitleLength :: Int
totalTitlesLength = 90
unfocusedTitleLength = 30

myTerminal :: String
myTerminal = "wezterm"

myBorderWidth :: Dimension
myBorderWidth = 3 -- Sets border width for windows

myNormColor :: String
myNormColor = "#282c34" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#46d9ff" -- Border color of focused windows

myModMask :: KeyMask
myModMask = mod4Mask
