module Treeselect where

import MyPrelude

weissTreeActions =
  treeselectAction
    def
    [ Node (TSNode "Hello" "displays hello" (spawn "xmessage hello!")) []
    , Node (TSNode "Shutdown" "Poweroff the system" (spawn "shutdown")) []
    ]
  where
    scratchpadActions = myScratchPads <&> error "TODO"
