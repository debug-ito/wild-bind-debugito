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
         -- * Simple Binding
         base,
         -- * Global
         GlobalConfig(..),
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
         -- * Firefox
         FirefoxConfig(..),
         defFirefoxConfig,
         firefox
       ) where

import Control.Monad (void, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad.Trans.State as State
import Data.Monoid ((<>), mempty)
import Data.Text (Text, isInfixOf, isSuffixOf, unpack)
import System.Process (callCommand, spawnCommand)
import WildBind.Input.NumPad (NumPadUnlocked(..))
import WildBind.Binding
  ( Binding, Binding',
    binds, on, as, run,
    whenFront,
    startFrom, ifBack, binds', extend,
    advice, before, after
  )
import WildBind.X11 (winClass, winInstance, winName, ActiveWindow)


-- | Push a key
push :: MonadIO m => String -> m ()
push k = liftIO $ callCommand ("xdotool key " ++ k)

-- | Push a sequence of keys
pushes :: MonadIO m => [String] -> m ()
pushes [] = return ()
pushes (k:rest) = push k >> pushes rest

-- | Run a command in background.
cmd' :: MonadIO m => String -> m ()
cmd' = liftIO . void . spawnCommand

-- | Basic, easily overridden bindings
base :: Binding s NumPadUnlocked
base = binds $ do
  on NumCenter `as` "Enter" `run` push "Return"


data GlobalConfig = GlobalConfig { globalMaximize :: IO (), -- ^ action to maximize the active window.
                                   globalMenu :: IO () -- ^ action to open the menu window.
                                 }

-- | Binding that should be globally active
global :: GlobalConfig -> Binding ActiveWindow NumPadUnlocked
global conf = global_nostate <> global_non_switcher where
  global_nostate = binds $ do
    on NumMinus `as` "Close" `run` push "Alt+F4"
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
    vpToggleDVDMenu :: IO ()
  }


data PlayerMode = NormalPlayer | DVDPlayer deriving (Show, Eq, Ord)

videoPlayerBase :: VideoPlayerConfig -> Binding' PlayerMode s NumPadUnlocked
videoPlayerBase conf = (ifBack (== NormalPlayer) normal_mode dvd_mode) <> common where
  act field = liftIO $ field conf
  normal_mode = binds' $ do
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
  dvd_mode = binds' $ do
    on NumDelete `as` "Normal Mode" `run` State.put NormalPlayer
    on NumPageDown `as` "Toggle Menu" `run` act vpToggleDVDMenu
  common = binds $ do
    on NumInsert `as` "Toggle Full Screen" `run` act vpToggleFull

videoPlayer :: VideoPlayerConfig -> Binding s NumPadUnlocked
videoPlayer = startFrom NormalPlayer . videoPlayerBase

dvdPlayer :: VideoPlayerConfig -> Binding ActiveWindow NumPadUnlocked
dvdPlayer = startFrom DVDPlayer . videoPlayerBase

forTotem :: (VideoPlayerConfig -> Binding ActiveWindow i) -> Binding ActiveWindow i
forTotem maker = whenFront (\w -> winInstance w == "totem") $ maker conf where
  conf = VideoPlayerConfig
         { vpPlayPause = push "p",
           vpVolumeUp = push "Up",
           vpVolumeDown = push "Down",
           vpBackNormal = push "Left",
           vpForwardNormal = push "Right",
           vpBackBig = push "Control+Left",
           vpForwardBig = push "Control+Right",
           vpBackSmall = push "Shift+Left",
           vpForwardSmall = push "Shift+Right",
           vpToggleFull = push "f",
           vpToggleDVDMenu = push "m"
         }

forVLC :: (VideoPlayerConfig -> Binding ActiveWindow i) -> Binding ActiveWindow i
forVLC maker = whenFront (\w -> winInstance w == "vlc") $ maker conf where
  conf = VideoPlayerConfig
         { vpPlayPause = push "space",
           vpVolumeUp = push "Ctrl+Up",
           vpVolumeDown = push "Ctrl+Down",
           vpBackNormal = push "Alt+Left",
           vpForwardNormal = push "Alt+Right",
           vpBackBig = push "Ctrl+Left",
           vpForwardBig = push "Ctrl+Right",
           vpBackSmall = push "Shift+Left",
           vpForwardSmall = push "Shift+Right",
           vpToggleFull = push "f",
           vpToggleDVDMenu = push "Shift+m"
         }

thunar :: Binding ActiveWindow NumPadUnlocked
thunar = whenFront (\w -> winInstance w == "Thunar" && winClass w == "Thunar") $ binds $ do
  on NumHome `as` "Home directory" `run` push "Alt+Home"
  on NumPageUp `as` "Parent directory" `run` push "Alt+Up"

thunarMenu :: Text -- ^ a string that should be part of the menu window's title.
           -> Binding ActiveWindow NumPadUnlocked
thunarMenu menu_window_name_part = whenFront frontCondition $ thunar <> ext where
  frontCondition w = menu_window_name_part `isInfixOf` winName w
  ext = binds $ do
    on NumCenter `as` "Run" `run` do
      push "Return"
      cmd' ("sleep 0.3; xdotool search --name '" ++ unpack menu_window_name_part ++ "' windowkill")


data GimpConfig = GimpConfig { gimpSwapColor :: IO ()
                             }

defGimpConfig :: GimpConfig
defGimpConfig = GimpConfig { gimpSwapColor = push "F12" }

-- | Binding for GIMP.
gimp :: GimpConfig -> Binding ActiveWindow NumPadUnlocked
gimp conf = whenFront (\w -> "Gimp" `isInfixOf` winClass w) $ binds $ do
  on NumCenter `as` "ペン" `run` push "p"
  on NumDelete `as` "鉛筆" `run` push "n"
  on NumLeft `as` "スポイト" `run` push "o"
  on NumRight `as` "消しゴム" `run` push "Shift+e"
  on NumHome `as` "矩形選択" `run` push "r"
  on NumUp `as` "色スワップ" `run` gimpSwapColor conf
  on NumPageUp `as` "パス" `run` push "b"
  on NumEnd `as` "やり直し" `run` push "Ctrl+z"
  on NumDown `as` "縮小" `run` push "minus"
  on NumInsert `as` "保存" `run` push "Ctrl+s"
  on NumPageDown `as` "拡大" `run` push "plus"


data FirefoxConfig = FirefoxConfig { ffCancel,
                                     ffLeftTab, ffRightTab, ffCloseTab,
                                     ffToggleBookmarks,
                                     ffLink, ffLinkNewTab,
                                     ffReload,
                                     ffBack, ffForward, ffHome,
                                     ffRestoreTab,
                                     ffFontNormal, ffFontBigger, ffFontSmaller :: IO ()
                                   }

defFirefoxConfig :: FirefoxConfig
defFirefoxConfig = FirefoxConfig { ffCancel = push "Ctrl+g",
                                   ffLeftTab = push "Shift+Ctrl+Tab",
                                   ffRightTab = push "Ctrl+Tab",
                                   ffCloseTab = pushes ["Ctrl+q", "Ctrl+w"],
                                   ffToggleBookmarks = pushes ["Ctrl+q", "Ctrl+b"],
                                   ffLink = pushes ["Ctrl+u", "e"],
                                   ffLinkNewTab = pushes ["Ctrl+u", "Shift+e"],
                                   ffReload = push "F5",
                                   ffBack = push "Shift+b",
                                   ffForward = push "Shift+f",
                                   ffHome = push "Alt+Home",
                                   ffRestoreTab = pushes ["Ctrl+c", "u"],
                                   ffFontNormal = push "Ctrl+0",
                                   ffFontBigger = push "Ctrl+plus",
                                   ffFontSmaller = pushes ["Ctrl+q", "Ctrl+minus"]
                                 }

data FirefoxState = FFBase | FFExt | FFFont | FFLink | FFBookmark deriving (Show,Eq,Ord)

firefox :: FirefoxConfig -> Binding ActiveWindow NumPadUnlocked
firefox conf = whenFront (\w -> winInstance w == "Navigator" && winClass w == "Firefox") impl where
  impl = startFrom FFBase
         $ binds_cancel
         <> ( ifBack (== FFBase) binds_base
              $ ifBack (== FFExt) (binds_all_cancel <> binds_ext)
              $ ifBack (== FFFont) (binds_all_cancel <> binds_font)
              $ ifBack (== FFLink) (binds_all_cancel <> binds_link)
              $ ifBack (== FFBookmark) binds_bookmark
              $ mempty
            )
  act field = liftIO $ field conf
  cancel_act = id `as` "Cancel" `run` (State.put FFBase >> act ffCancel)
  binds_all_cancel = binds' $ do
    forM_ (enumFromTo minBound maxBound) $ \k -> on k cancel_act
  binds_cancel = binds' $ do
    on NumDelete cancel_act
  binds_base = binds' $ do
    on NumLeft `as` "Left tab" `run` act ffLeftTab
    on NumRight `as` "Right tab" `run` act ffRightTab
    on NumEnd `as` "Close tab" `run` act ffCloseTab
    on NumInsert `as` "Bookmark" `run` do
      State.put FFBookmark
      act ffToggleBookmarks
    on NumCenter `as` "Link" `run` do
      State.put FFLink
      act ffLink
    on NumHome `as` "Ext." `run` State.put FFExt
  binds_ext = binds_ext_base <> binds_font
  binds_ext_base = binds' $ do
    on NumHome `as` "Link new tab" `run` do
      State.put FFLink
      act ffLinkNewTab
    advice (before $ State.put FFBase) $ do
      on NumPageUp `as` "Reload" `run` act ffReload
      on NumLeft `as` "Back" `run` act ffBack
      on NumRight `as` "Forward" `run` act ffForward
      on NumPageDown `as` "Home" `run` act ffHome
      on NumEnd `as` "Restore tab" `run` act ffRestoreTab
  binds_font = binds' $ do
    on NumCenter `as` "Normal font" `run` do
      State.put FFBase
      act ffFontNormal
    advice (before $ State.put FFFont) $ do
      on NumUp `as` "Bigger font" `run` act ffFontBigger
      on NumDown `as` "Smaller font" `run` act ffFontSmaller
  binds_link = binds' $ do
    forM_ (enumFromTo minBound maxBound) $ \k -> on k cancel_act
    on NumUp `as` "OK" `run` do
      State.put FFBase
      push "Return"
    on NumLeft `as` "4" `run` push "4"
    on NumCenter `as` "5" `run` push "5"
    on NumRight `as` "6" `run` push "6"
  binds_bookmark = binds' $ do
    on NumEnd `as` "Tab" `run` push "Tab"
    advice (after $ act ffToggleBookmarks) $ do
      forM_ [NumHome, NumInsert, NumDelete] $ \k -> on k cancel_act
      advice (after $ State.put FFBase) $ do
        on NumCenter `as` "Select (new tab)" `run` push "Ctrl+Return"
        on NumInsert `as` "Select (cur tab)" `run` push "Return"

