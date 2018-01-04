-- |
-- Module: WildBind.DebugIto.Main
-- Description: executable Main module
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module WildBind.DebugIto.Main
       (main) where

import Data.Monoid ((<>), mconcat)
import Data.Text (intercalate)
import qualified Data.Text.IO as TIO
import WildBind
  ( wildBind, Binding,
    convInput, whenFront,
    justBefore, justAfter,
    describe, revise
  )
import WildBind.Indicator
  ( withNumPadIndicator, wildBindWithIndicator,
    adaptIndicator, toggleBinding
  )
import WildBind.Input.NumPad (NumPadUnlocked, NumPadLocked(NumLDivide))
import WildBind.Seq
  ( toSeq, withPrefix, withCancel, fromSeq,
    reviseSeq
  )
import WildBind.X11
  ( withX11Front, makeFrontEnd,
    X11Front, ActiveWindow, defaultRootWindow,
    winInstance, winClass, winName,
    alt, ctrl, shift, press,
    XKeyEvent, ToXKeyEvent
  )
import WildBind.X11.Emulate (push, pushTo, remap, remapR)
import WildBind.X11.KeySym
import qualified WildBind.DebugIto as D

main :: IO ()
main = withNumPadIndicator $ \ind -> do
  withX11Front $ \x11 -> do
    let front = makeFrontEnd x11
        binding = convInput Left num_binding
                  <> convInput Right (normalBinding x11)
        num_binding = (numBinding x11) <> toggleBinding ind NumLDivide
        ind' = adaptIndicator Left (either Just (const Nothing)) ind
    wildBindWithIndicator ind' binding front
       
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

normalBinding :: X11Front i -> Binding ActiveWindow XKeyEvent
normalBinding x11 = whenFront condVivaldi binding
                    <> whenFront condXev binding -- for testing.
  where
    condVivaldi w = winClass w == "Vivaldi-stable"
    condXev w = winName w == "Event Tester"
    remap' :: (ToXKeyEvent a, ToXKeyEvent b) => a -> b -> Binding ActiveWindow XKeyEvent
    remap' = remap x11
    remapR' :: (ToXKeyEvent a, ToXKeyEvent b) => a -> b -> Binding ActiveWindow XKeyEvent
    remapR' = remapR x11
    toSeq' ps = withPrefix ps . toSeq
    fromSeq' = fromSeq . reviseSeq rev
      where
        rev ps _ _ = justBefore $ TIO.putStr ( "(Prefix: " <> (intercalate "," $ fmap describe ps) <> ") " )
    revInput () _ i = justAfter $ TIO.putStrLn ("Input " <> describe i)
    binding = revise revInput $ mconcat
              [ remap' (ctrl xK_n) (xK_Down),
                remap' (ctrl xK_p) (xK_Up),
                remap' (ctrl xK_f) (xK_Right),
                
                remapR' (ctrl xK_m) (xK_Return),
                
                remapR' (ctrl xK_g) (xK_Escape),
                
                fromSeq' $ withCancel [ctrl xK_g]
                $ mconcat [ toSeq' [ctrl xK_z] z_binding,
                            toSeq' [ctrl xK_x] x_binding
                          ]
              ]
    z_binding = mconcat [ remap' (ctrl xK_n) (ctrl xK_Page_Down),
                          remap' (ctrl xK_p) (ctrl xK_Page_Up),
                          remap' (ctrl xK_c) (ctrl xK_t),
                          remap' (ctrl xK_k) (ctrl xK_w),
                          remap' (ctrl xK_slash) (shift $ ctrl xK_T)
                        ]
    x_binding = mconcat [ remap' (ctrl xK_f) (ctrl xK_o),
                          remap' (ctrl xK_s) (press xK_slash)
                        ]

