-- |
-- Module: WildBind.DebugIto.Main
-- Description: executable Main module
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module WildBind.DebugIto.Main
       ( main,
         numBinding
       ) where

import Data.Monoid ((<>), mconcat)
import WildBind (Binding)
import WildBind.Input.NumPad (NumPadUnlocked)
import WildBind.X11
  ( withX11Front, makeFrontEnd,
    X11Front, ActiveWindow
  )
import qualified WildBind.DebugIto as D

main :: IO ()
main = withX11Front $ \x11 -> do
  let front = makeFrontEnd x11
      numb = numBinding x11
      keyb = D.vivaldiKey x11
  D.execEitherWildBind numb keyb front
       
numBinding :: X11Front i -> Binding ActiveWindow NumPadUnlocked
numBinding x11 =
  D.base x11
  <> mconcat [ c x11 m | c <- [D.forTotem, D.forVLC], m <- [D.videoPlayer]]
  <> D.thunar x11
  <> D.thunarMenu x11 "menu_numpaar"
  <> D.firefox x11
  <> D.vivaldi x11
  <> D.gimp x11 (D.defGimpConfig x11)
  <> D.global (D.xfceGlobalConfig x11 "/home/toshio/numpaar/menu_numpaar")

