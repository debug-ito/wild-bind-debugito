module Main
       (main) where

import Data.Monoid ((<>))
import WildBind (wildBind, Binding)
import WildBind.Input.NumPad (NumPadUnlocked)
import WildBind.X11
  ( withX11Front, makeFrontEnd,
    X11Front, ActiveWindow,
    alt
  )
import WildBind.X11.Emulate (push)
import WildBind.X11.KeySym
import qualified WildBind.DebugIto as D

main :: IO ()
main = withX11Front $ \x11 -> wildBind (myBinding x11) (makeFrontEnd x11)

myBinding :: X11Front i -> Binding ActiveWindow NumPadUnlocked
myBinding x11 = D.base x11
                <> D.vivaldi x11
                <> D.global x11 gconfig
  where
    gconfig = D.GlobalConfig
              { D.globalMaximize = push x11 (alt xK_F7),
                D.globalMenu = return ()
              }
