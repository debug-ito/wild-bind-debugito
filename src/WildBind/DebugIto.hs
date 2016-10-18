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
  ( Binding,
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


data GlobalConfig = GlobalConfig { globalMaximize :: IO (),
                                   globalMenu :: IO ()
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

videoPlayer :: VideoPlayerConfig -> Binding s NumPadUnlocked
videoPlayer v = binds $ do
  on NumHome `as` "Back (L)" `run` (vpBackBig v)
  on NumUp `as` "Vol up" `run` (vpVolumeUp v)
  on NumPageUp `as` "Forward (L)" `run` (vpForwardBig v)
  on NumLeft `as` "Back (M)" `run` (vpBackNormal v)
  on NumCenter `as` "Play/Pause" `run` (vpPlayPause v)
  on NumRight `as` "Forward (M)" `run` (vpForwardNormal v)
  on NumEnd `as` "Back (S)" `run` (vpBackSmall v)
  on NumDown `as` "Vol down" `run` (vpVolumeDown v)
  on NumPageDown `as` "Forward (S)" `run` (vpForwardSmall v)
  on NumInsert `as` "Toggle Full Screen" `run` (vpToggleFull v)


data PlayerMode = NormalPlayer | DVDPlayer deriving (Show, Eq, Ord)

dvdPlayer :: VideoPlayerConfig -> Binding ActiveWindow NumPadUnlocked
dvdPlayer conf = frontCondition $ startFrom DVDPlayer $ ifBack (== NormalPlayer) normal_mode dvd_mode where
  frontCondition = whenFront $ \w -> "_DVD" `isSuffixOf` winClass w
  normal_mode = extend (videoPlayer conf)
                <> ( binds' $ do
                        on NumDelete `as` "DVD Mode" `run` State.put DVDPlayer
                   )
  dvd_mode = binds' $ do
    on NumDelete `as` "Normal Mode" `run` State.put NormalPlayer
    on NumPageDown `as` "Toggle Menu" `run` (liftIO $ vpToggleDVDMenu conf)

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

thunarMenu :: Text -> Binding ActiveWindow NumPadUnlocked
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


data FirefoxConfig = FirefoxConfig -- TODO

data FirefoxState = FFBase | FFExt | FFFont | FFLink | FFBookmark deriving (Show,Eq,Ord)

firefox :: Binding ActiveWindow NumPadUnlocked
firefox = whenFront (\w -> winInstance w == "Navigator" && winClass w == "Firefox") impl where
  impl = startFrom FFBase
         $ binds_cancel
         <> ( ifBack (== FFBase) binds_base
              $ ifBack (== FFExt) (binds_all_cancel <> binds_ext)
              $ ifBack (== FFFont) (binds_all_cancel <> binds_font)
              $ ifBack (== FFLink) (binds_all_cancel <> binds_link)
              $ ifBack (== FFBookmark) binds_bookmark
              $ mempty
            )
  cancel_act = id `as` "Cancel" `run` (State.put FFBase >> push "Ctrl+g")
  binds_all_cancel = binds' $ do
    forM_ (enumFromTo minBound maxBound) $ \k -> on k cancel_act
  binds_cancel = binds' $ do
    on NumDelete cancel_act
  binds_base = binds' $ do
    on NumLeft `as` "Left tab" `run` push "Shift+Ctrl+Tab"
    on NumRight `as` "Right tab" `run` push "Ctrl+Tab"
    on NumEnd `as` "Close tab" `run` pushes ["Ctrl+q", "Ctrl+w"]
    on NumInsert `as` "Bookmark" `run` do
      State.put FFBookmark
      pushes ["Ctrl+q", "Ctrl+b"]
    on NumCenter `as` "Link" `run` do
      State.put FFLink
      pushes ["Ctrl+u", "e"]
    on NumHome `as` "Ext." `run` State.put FFExt
  binds_ext = binds_ext_base <> binds_font
  binds_ext_base = binds' $ do
    on NumHome `as` "Link new tab" `run` do
      State.put FFLink
      pushes ["Ctrl+u", "Shift+e"]
    advice (before $ State.put FFBase) $ do
      on NumPageUp `as` "Reload" `run` push "F5"
      on NumLeft `as` "Back" `run` push "Shift+b"
      on NumRight `as` "Forward" `run` push "Shift+f"
      on NumPageDown `as` "Home" `run` push "Alt+Home"
      on NumEnd `as` "Restore tab" `run` pushes ["Ctrl+c", "u"]
  binds_font = binds' $ do
    on NumCenter `as` "Normal font" `run` do
      State.put FFBase
      push "Ctrl+0"
    advice (before $ State.put FFFont) $ do
      on NumUp `as` "Larger font" `run` push "Ctrl+plus"
      on NumDown `as` "Smaller font" `run` pushes ["Ctrl+q", "Ctrl+minus"]
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
    advice (after $ pushes ["Ctrl+q", "Ctrl+b"]) $ do
      forM_ [NumHome, NumInsert, NumDelete] $ \k -> on k cancel_act
      advice (after $ State.put FFBase) $ do
        on NumCenter `as` "Select (new tab)" `run` push "Ctrl+Return"
        on NumInsert `as` "Select (cur tab)" `run` push "Return"

