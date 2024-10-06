module WeissXmobar where

import Config
import Data.Functor ((<&>))
import Data.List
import Data.List.Utils
import Utils
import WorkspaceFamily (logWorkspaceFamilies)
import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.StackSet qualified as W
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows

{- | Windows should have *some* title, which should not not exceed a
 sane length.
-}
ppWindow :: Int -> String -> String
ppWindow limit =
  xmobarRaw . (\w -> if null w then "untitled" else w) . shorten limit

blue, lowWhite, magenta, red, white, yellow :: String -> String
magenta = xmobarColor "#ff79c6" ""
blue = xmobarColor "#bd93f9" ""
white = xmobarColor "#f8f8f2" ""
yellow = xmobarColor "#f1fa8c" ""
red = xmobarColor "#ff5555" ""
lowWhite = xmobarColor "#bbbbbb" ""

fn1, fn2, fn3 :: String -> String
fn1 = wrap "<fn=1>" "</fn>"
fn2 = wrap "<fn=2>" "</fn>"
fn3 = wrap "<fn=3>" "</fn>"

extrasWrap :: String -> X (Maybe String) -> String -> X (Maybe String)
extrasWrap pre x post = fmap (wrap pre post) <$> x

data ExtraString = Normal String | Special (X (Maybe String))
toX :: ExtraString -> X (Maybe String)
toX e = case e of
  Normal s -> return (Just s)
  Special x -> x
toPPExtras :: [ExtraString] -> X (Maybe String)
toPPExtras =
  foldl
    ( \res elem -> do
        mres <- res
        melem <- toX elem
        return $ do
          pres <- mres
          pelem <- melem
          return $ pres ++ pelem
    )
    (toX $ Normal "")

replaceSymbol :: String -> String
replaceSymbol = replace "Ʀ" "R"

myTitles :: Logger
myTitles =
  myLogTitles
    " "
    (fn1 $ blue " | ")
    (wrap (lowWhite "[") (lowWhite "]") . white . show)
    (magenta . replaceSymbol)
    ( intercalate (fn1 $ yellow " | ")
        . map (yellow . replaceSymbol . ppWindow unfocusedTitleLength)
    )

myXmobarHoriPP :: PP
myXmobarHoriPP =
  def {ppOrder = \[_, _, _, wins] -> [wins], ppExtras = [myTitles]}

-- , ppRename = \s ws -> fn1 s

workspacePP :: PP
workspacePP =
  def
    { ppSep = fn2 $ blue " | " -- blue " •|• "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = magenta . wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppVisible = white
    , ppHidden = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \(ws : l : _ : wsFamilies : _) -> [wsFamilies, ws, l]
    , ppExtras = [logWorkspaceFamilies]
    , ppLayout = trimLayoutModifiers
    -- , ppRename = \s ws -> fn1 s
    }

xmobarVertical dir =
  statusBarPropTo
    "_XMONAD_LOG_workspace"
    ("xmobar -x 0 " ++ dir ++ "/xmobarrc0.hs")
    (pure workspacePP)

xmobarHori dir =
  statusBarPropTo
    "_XMONAD_LOG_Hori"
    ("xmobar -x 1 " ++ dir ++ "/xmobarrc1.hs")
    (pure myXmobarHoriPP)

-- xmobar3 = statusBarPropTo
--     "_XMONAD_LOG_3"
--     "xmobar -x 1 /home/weiss/.config/xmobar/xmobarrc2.hs"
--     (pure def)
