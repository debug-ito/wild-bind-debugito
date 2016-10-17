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
         -- * GIMP
         GimpConfig(..),
         defGimpConfig,
         gimp
       ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad.Trans.State as State
import Data.Monoid ((<>))
import Data.Text (isInfixOf, isSuffixOf)
import System.Process (callCommand, spawnCommand)
import WildBind.Input.NumPad (NumPadUnlocked(..))
import WildBind.Binding
  ( Binding,
    binds, on, as, run,
    whenFront,
    startFrom, ifBack, binds', extend
  )
import WildBind.X11 (winClass, winInstance, ActiveWindow)


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

