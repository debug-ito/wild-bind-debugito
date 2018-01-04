{-# LANGUAGE FlexibleContexts, PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures  #-}
-- |
-- Module: WildBind.DebugIto
-- Description: Bindings used by debugito
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module WildBind.DebugIto
       ( -- * Actions
         push,
         pushes,
         cmd',
         -- * Combined execution
         execEitherWildBind,
         -- * Simple Binding
         base,
         -- * Global
         GlobalConfig(..),
         xfceGlobalConfig,
         global,
         -- * Video players
         VideoPlayerConfig(..),
         videoPlayer,
         dvdPlayer,
         forTotem,
         forVLC,
         -- * Thunar
         thunar,
         thunarMenu,
         -- * GIMP
         GimpConfig(..),
         defGimpConfig,
         gimp,
         -- * Web browsers
         WebBrowserConfig(..),
         defFirefoxConfig,
         defVivaldiConfig,
         webBrowser,
         firefox,
         vivaldi,
         -- * Keyboard bindings
         vivaldiKey
       ) where

import Control.Monad (void, forM_)
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Control.Monad.State as State
import Data.Monoid ((<>), mempty)
import Data.Text (Text, isInfixOf, isSuffixOf, unpack)
import System.Process (callCommand, spawnCommand)
import WildBind.Input.NumPad (NumPadUnlocked(..), NumPadLocked(NumLDivide))
import WildBind.Indicator
  ( withNumPadIndicator, wildBindWithIndicator,
    adaptIndicator, toggleBinding
  )
import WildBind
  ( Binding, Binding',
    binds, on, as, run,
    whenFront, convInput,
    startFrom, ifBack, binds', extend,
    advice, before, after, revise,
    bindsF, bindsF',
    FrontEnd
  )
import WildBind.Seq
  ( toSeq, withPrefix, withCancel, fromSeq,
    reviseSeq
  )
import WildBind.X11
  ( winClass, winInstance, winName, ActiveWindow, Window,
    defaultRootWindow,
    ToXKeyEvent, X11Front, XKeyEvent,
    alt, ctrl, shift, press
  )
import WildBind.X11.Emulate (push, pushTo, remap, remapR)
import WildBind.X11.KeySym

-- | Push a sequence of keys
pushes :: (ToXKeyEvent k, MonadIO m, MonadReader Window m) => X11Front i -> [k] -> m ()
pushes x11 = mapM_ (push x11)

-- | Run a command in background.
cmd' :: MonadIO m => String -> m ()
cmd' = liftIO . void . spawnCommand

-- | Basic, easily overridden bindings
base :: X11Front i -> Binding ActiveWindow NumPadUnlocked
base x11 = bindsF $ do
  on NumCenter `as` "Enter" `run` push x11 xK_Return

execEitherWildBind :: Binding s NumPadUnlocked
                   -> Binding s XKeyEvent
                   -> FrontEnd s (Either NumPadUnlocked XKeyEvent)
                   -> IO ()
execEitherWildBind num_binding key_binding front = withNumPadIndicator $ \ind -> do
  let binding = convInput Left num_binding' <> convInput Right key_binding
      num_binding' = num_binding <> toggleBinding ind NumLDivide
      ind' = adaptIndicator Left (either Just (const Nothing)) ind
  wildBindWithIndicator ind' binding front

data GlobalConfig =
  GlobalConfig
  { globalMaximize :: ReaderT ActiveWindow IO (),
    -- ^ action to maximize the active window.
    globalMenu :: ReaderT ActiveWindow IO (),
    -- ^ action to open the menu window.
    globalClose :: ReaderT ActiveWindow IO ()
    -- ^ action to close the current window
  }

xfceGlobalConfig :: X11Front i
                 -> String -- ^ path to the menu directory.
                 -> GlobalConfig
xfceGlobalConfig x11 menu_dir =
  GlobalConfig
  { globalMaximize = pushTo x11 rwin (alt xK_F7),
    globalMenu = cmd' ("Thunar " <> menu_dir),
    globalClose = pushTo x11 rwin (alt xK_F4)
  }
  where
    rwin = defaultRootWindow x11

-- | Binding that should be globally active
global :: GlobalConfig -> Binding ActiveWindow NumPadUnlocked
global conf = global_nostate <> global_non_switcher where
  global_nostate = bindsF $ do
    on NumMinus `as` "Close" `run` globalClose conf
    on NumPlus `as` "Maximize" `run` globalMaximize conf
    on NumMulti `as` "Menu" `run` globalMenu conf
  global_non_switcher = whenFront (\w -> winInstance w /= "boring-window-switcher") $ binds $ do
    on NumEnter `as` "Switch" `run` cmd' "boring-window-switcher"


data VideoPlayerConfig =
  VideoPlayerConfig
  { vpPlayPause, vpVolumeUp, vpVolumeDown,
    vpBackNormal, vpForwardNormal,
    vpBackBig, vpForwardBig,
    vpBackSmall, vpForwardSmall,
    vpToggleFull,
    vpToggleDVDMenu :: ReaderT ActiveWindow IO ()
  }

data PlayerMode = NormalPlayer | DVDPlayer deriving (Show, Eq, Ord)

videoPlayerBase :: VideoPlayerConfig -> Binding' PlayerMode ActiveWindow NumPadUnlocked
videoPlayerBase conf = (ifBack (== NormalPlayer) normal_mode dvd_mode) <> common where
  act field = lift $ field conf
  normal_mode = bindsF' $ do
    on NumHome `as` "Back (L)" `run` act vpBackBig
    on NumUp `as` "Vol up" `run` act vpVolumeUp
    on NumPageUp `as` "Forward (L)" `run` act vpForwardBig
    on NumLeft `as` "Back (M)" `run` act vpBackNormal
    on NumCenter `as` "Play/Pause" `run` act vpPlayPause
    on NumRight `as` "Forward (M)" `run` act vpForwardNormal
    on NumEnd `as` "Back (S)" `run` act vpBackSmall
    on NumDown `as` "Vol down" `run` act vpVolumeDown
    on NumPageDown `as` "Forward (S)" `run` act vpForwardSmall
    on NumDelete `as` "DVD Mode" `run` State.put DVDPlayer
  dvd_mode = bindsF' $ do
    on NumDelete `as` "Normal Mode" `run` State.put NormalPlayer
    on NumPageDown `as` "Toggle Menu" `run` act vpToggleDVDMenu
  common = bindsF' $ do
    on NumInsert `as` "Toggle Full Screen" `run` act vpToggleFull

videoPlayer :: VideoPlayerConfig -> Binding ActiveWindow NumPadUnlocked
videoPlayer = startFrom NormalPlayer . videoPlayerBase

dvdPlayer :: VideoPlayerConfig -> Binding ActiveWindow NumPadUnlocked
dvdPlayer = startFrom DVDPlayer . videoPlayerBase

forTotem :: X11Front k -> (VideoPlayerConfig -> Binding ActiveWindow i) -> Binding ActiveWindow i
forTotem x11 maker = whenFront (\w -> winInstance w == "totem") $ maker conf where
  push' :: (ToXKeyEvent k, _) => k -> _
  push' = push x11
  conf = VideoPlayerConfig
         { vpPlayPause = push' xK_p,
           vpVolumeUp = push' xK_Up,
           vpVolumeDown = push' xK_Down,
           vpBackNormal = push' xK_Left,
           vpForwardNormal = push' xK_Right,
           vpBackBig = push' (ctrl xK_Left),
           vpForwardBig = push' (ctrl xK_Right),
           vpBackSmall = push' (shift xK_Left),
           vpForwardSmall = push' (shift xK_Right),
           vpToggleFull = push' xK_f,
           vpToggleDVDMenu = push' xK_m
         }

forVLC :: X11Front k -> (VideoPlayerConfig -> Binding ActiveWindow i) -> Binding ActiveWindow i
forVLC x11 maker = whenFront (\w -> winInstance w == "vlc") $ maker conf where
  push' :: (ToXKeyEvent k, _) => k -> _
  push' = push x11
  conf = VideoPlayerConfig
         { vpPlayPause = push' xK_space,
           vpVolumeUp = push' (ctrl xK_Up),
           vpVolumeDown = push' (ctrl xK_Down),
           vpBackNormal = push' (alt xK_Left),
           vpForwardNormal = push' (alt xK_Right),
           vpBackBig = push' (ctrl xK_Left),
           vpForwardBig = push' (ctrl xK_Right),
           vpBackSmall = push' (shift xK_Left),
           vpForwardSmall = push' (shift xK_Right),
           vpToggleFull = push' xK_f,
           vpToggleDVDMenu = push' (shift xK_M)
         }

thunar :: X11Front i -> Binding ActiveWindow NumPadUnlocked
thunar x11 = whenFront (\w -> winInstance w == "Thunar" && winClass w == "Thunar") $ bindsF $ do
  on NumHome `as` "Home directory" `run` push x11 (alt xK_Home)
  on NumPageUp `as` "Parent directory" `run` push x11 (alt xK_Up)

thunarMenu :: X11Front i
           -> Text -- ^ a string that should be part of the menu window's title.
           -> Binding ActiveWindow NumPadUnlocked
thunarMenu x11 menu_window_name_part = whenFront frontCondition $ thunar x11 <> ext where
  frontCondition w = menu_window_name_part `isInfixOf` winName w
  ext = bindsF $ do
    on NumCenter `as` "Run" `run` do
      push x11 xK_Return
      -- cmd' ("sleep 0.3; xdotool search --name '" ++ unpack menu_window_name_part ++ "' windowkill")
      push x11 (alt xK_F4)
  

data GimpConfig = GimpConfig { gimpSwapColor :: ReaderT ActiveWindow IO ()
                             }

defGimpConfig :: X11Front i -> GimpConfig
defGimpConfig x11 = GimpConfig { gimpSwapColor = push x11 xK_F12 }

-- | Binding for GIMP.
gimp :: X11Front i -> GimpConfig -> Binding ActiveWindow NumPadUnlocked
gimp x11 conf = whenFront (\w -> "Gimp" `isInfixOf` winClass w) $ bindsF $ do
  on NumCenter `as` "ペン" `run` push' xK_p
  on NumDelete `as` "鉛筆" `run` push' xK_n
  on NumLeft `as` "スポイト" `run` push' xK_o
  on NumRight `as` "消しゴム" `run` push' (shift xK_E)
  on NumHome `as` "矩形選択" `run` push' xK_r
  on NumUp `as` "色スワップ" `run` gimpSwapColor conf
  on NumPageUp `as` "パス" `run` push' xK_o
  on NumEnd `as` "やり直し" `run` push' (ctrl xK_z)
  on NumDown `as` "縮小" `run` push' xK_minus
  on NumInsert `as` "保存" `run` push' (ctrl xK_s)
  on NumPageDown `as` "拡大" `run` push' xK_plus
  where
    push' :: (ToXKeyEvent k, _) => k -> _
    push' = push x11


data WebBrowserConfig =
  WebBrowserConfig
  { wbCancel,
    wbLeftTab, wbRightTab, wbCloseTab,
    wbToggleBookmarks,
    wbLink, wbLinkNewTab, wbLinkFinish,
    wbReload,
    wbBack, wbForward, wbHome,
    wbRestoreTab,
    wbFontNormal, wbFontBigger, wbFontSmaller :: ReaderT ActiveWindow IO (),
    wbFrontCondition :: ActiveWindow -> Bool
  }

defFirefoxConfig :: X11Front i -> WebBrowserConfig
defFirefoxConfig x11 =
  WebBrowserConfig
  { wbCancel = push' (ctrl xK_g),
    wbLeftTab = push' (shift $ ctrl xK_Tab),
    wbRightTab = push' (ctrl xK_Tab),
    wbCloseTab = pushes' [ctrl xK_q, ctrl xK_w],
    wbToggleBookmarks = pushes' [ctrl xK_q, ctrl xK_b],
    wbLink = pushes' [ctrl xK_u, press xK_e],
    wbLinkNewTab = pushes' [ctrl xK_u, shift xK_E],
    wbLinkFinish = push' xK_Return,
    wbReload = push' xK_F5,
    wbBack = push' (shift xK_B),
    wbForward = push' (shift xK_F),
    wbHome = push' (alt xK_Home),
    wbRestoreTab = pushes' [ctrl xK_c, press xK_u],
    wbFontNormal = push' (ctrl xK_0),
    wbFontBigger = push' (ctrl xK_plus),
    wbFontSmaller = pushes' [ctrl xK_q, ctrl xK_minus],
    wbFrontCondition = \w -> winInstance w == "Navigator" && winClass w == "Firefox"
  }
  where
    push' :: (ToXKeyEvent k, _) => k -> _
    push' = push x11
    pushes' = pushes x11

defVivaldiConfig :: X11Front i -> WebBrowserConfig
defVivaldiConfig x11 =
  WebBrowserConfig
  { wbCancel = push' xK_Escape,
    wbLeftTab = push' (ctrl xK_Page_Up),
    wbRightTab = push' (ctrl xK_Page_Down),
    wbCloseTab = push' (ctrl xK_w),
    wbToggleBookmarks = push' xK_F6,
    wbLink = pushes x11 [xK_asciicircum, xK_f],
    wbLinkNewTab = pushes x11 [press xK_asciicircum, shift xK_C],
    wbLinkFinish = return (),
    wbReload = push' xK_F5,
    wbBack = push' (ctrl xK_Left),
    wbForward = push' (ctrl xK_Right),
    wbHome = push' (alt xK_Home),
    wbRestoreTab = push' (shift $ ctrl xK_T),
    wbFontNormal = push' (ctrl xK_0),
    wbFontBigger = push' (shift $ ctrl xK_semicolon),
    wbFontSmaller = push' (ctrl xK_minus),
    wbFrontCondition = \w -> winClass w == "Vivaldi-stable"
  }
  where
    push' :: (ToXKeyEvent k, _) => k -> _
    push' = push x11

data WebBrowserState = WBBase | WBExt | WBFont | WBLink | WBBookmark deriving (Show,Eq,Ord)

firefox :: X11Front i -> Binding ActiveWindow NumPadUnlocked
firefox x11 = webBrowser x11 $ defFirefoxConfig x11

vivaldi :: X11Front i -> Binding ActiveWindow NumPadUnlocked
vivaldi x11 = webBrowser x11 $ defVivaldiConfig x11

webBrowser :: X11Front i -> WebBrowserConfig -> Binding ActiveWindow NumPadUnlocked
webBrowser x11 conf = impl where
  push' :: (ToXKeyEvent k, _) => k -> _
  push' = push x11
  impl = whenFront (wbFrontCondition conf) $ startFrom WBBase
         $ binds_cancel
         <> ( ifBack (== WBBase) binds_base
              $ ifBack (== WBExt) (binds_all_cancel <> binds_ext)
              $ ifBack (== WBFont) (binds_all_cancel <> binds_font)
              $ ifBack (== WBLink) (binds_all_cancel <> binds_link)
              $ ifBack (== WBBookmark) binds_bookmark
              $ mempty
            )
  act field = lift $ field conf
  cancel_act = id `as` "Cancel" `run` (State.put WBBase >> act wbCancel)
  binds_all_cancel = bindsF' $ do
    forM_ (enumFromTo minBound maxBound) $ \k -> on k cancel_act
  binds_cancel = bindsF' $ do
    on NumDelete cancel_act
  binds_base = bindsF' $ do
    on NumLeft `as` "Left tab" `run` act wbLeftTab
    on NumRight `as` "Right tab" `run` act wbRightTab
    on NumEnd `as` "Close tab" `run` act wbCloseTab
    on NumInsert `as` "Bookmark" `run` do
      State.put WBBookmark
      act wbToggleBookmarks
    on NumCenter `as` "Link" `run` do
      State.put WBLink
      act wbLink
    on NumHome `as` "Ext." `run` State.put WBExt
  binds_ext = binds_ext_base <> binds_font
  binds_ext_base = bindsF' $ do
    on NumHome `as` "Link new tab" `run` do
      State.put WBLink
      act wbLinkNewTab
    advice (before $ State.put WBBase) $ do
      on NumPageUp `as` "Reload" `run` act wbReload
      on NumLeft `as` "Back" `run` act wbBack
      on NumRight `as` "Forward" `run` act wbForward
      on NumPageDown `as` "Home" `run` act wbHome
      on NumEnd `as` "Restore tab" `run` act wbRestoreTab
  binds_font = bindsF' $ do
    on NumCenter `as` "Normal font" `run` do
      State.put WBBase
      act wbFontNormal
    advice (before $ State.put WBFont) $ do
      on NumUp `as` "Bigger font" `run` act wbFontBigger
      on NumDown `as` "Smaller font" `run` act wbFontSmaller
  binds_link = bindsF' $ do
    forM_ (enumFromTo minBound maxBound) $ \k -> on k cancel_act
    on NumUp `as` "OK" `run` do
      State.put WBBase
      act wbLinkFinish
    on NumLeft `as` "4" `run` push' xK_4
    on NumCenter `as` "5" `run` push' xK_5
    on NumRight `as` "6" `run` push' xK_6
  binds_bookmark = bindsF' $ do
    on NumEnd `as` "Tab" `run` push' xK_Tab
    advice (after $ act wbToggleBookmarks) $ do
      forM_ [NumInsert, NumDelete] $ \k -> on k cancel_act
      advice (after $ State.put WBBase) $ do
        on NumCenter `as` "Select (new tab)" `run` push' (ctrl xK_Return)
        on NumHome `as` "Select (cur tab)" `run` push' xK_Return


vivaldiKey :: X11Front i -> Binding ActiveWindow XKeyEvent
vivaldiKey x11 = whenFront condVivaldi binding
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
        -- rev ps _ _ = justBefore $ TIO.putStr ( "(Prefix: " <> (intercalate "," $ fmap describe ps) <> ") " )
        rev _ _ _ = Just
    -- revInput () _ i = justAfter $ TIO.putStrLn ("Input " <> describe i)
    revInput _ _ _ = Just
    binding = revise revInput $ mconcat
              [ remap' (ctrl xK_n) (xK_Down),
                remap' (ctrl xK_p) (xK_Up),
                remap' (ctrl xK_s) (press xK_slash),
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
                          remap' (ctrl xK_s) (ctrl xK_s)
                        ]

