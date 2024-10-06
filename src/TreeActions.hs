module TreeActions where

import Data.Map qualified as Map
import Data.Tree (Tree (..))
import WeissScratchpad (myScratchPads, switchNSP)
import XMonad
import XMonad.Actions.TreeSelect
import XMonad.Prelude ((<&>))
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS))

myTreeConf =
  TSConfig
    { ts_hidechildren = True
    , ts_background = 0xdd282c34
    , ts_font = "xft:Ubuntu:bold"
    , ts_node = (0xffc678dd, 0xff202328)
    , ts_nodealt = (0xffc678dd, 0xff202020)
    , ts_highlight = (0xff000000, 0xff46D9FF) -- black, cyan
    , ts_extra = 0xff78DD9D
    , ts_node_width = 200
    , ts_node_height = 30
    , ts_originX = 500
    , ts_originY = 500
    , ts_indent = 80
    , ts_navigate = navigation
    }
  where
    navigation =
      Map.fromList
        [ ((0, xK_Escape), cancel)
        , ((0, xK_Return), select)
        , ((0, xK_space), select)
        , ((0, xK_Up), movePrev)
        , ((0, xK_Down), moveNext)
        , ((0, xK_Left), moveParent)
        , ((0, xK_Right), moveChild)
        , ((0, xK_k), movePrev)
        , ((0, xK_j), moveNext)
        , ((0, xK_i), moveParent)
        , ((0, xK_l), moveChild)
        , ((0, xK_u), moveHistBack)
        , ((0, xK_o), moveHistForward)
        ]

weissTreeActions =
  treeselectAction
    myTreeConf
    $ [ Node
          (TSNode "System" "System operations" (return ()))
          [ Node (TSNode "Shutdown" "Poweroff the system" (spawn " sudo shutdown now")) []
          , Node (TSNode "Reboot" "Reboot the system" (spawn "sudo reboot")) []
          , Node (TSNode "Suspend" "Suspend the system" (spawn "sudo systemctl suspend")) []
          ]
      ]
      <> scratchpadActions
  where
    scratchpadActions =
      myScratchPads <&> \(NS name _ _ _) ->
        Node
          (TSNode name ("Activate scratchpad " <> name) (switchNSP name))
          []
