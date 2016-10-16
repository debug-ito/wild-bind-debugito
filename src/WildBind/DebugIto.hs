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
         -- * Simple Bindings
         base,
         gimp
       ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (isInfixOf)
import System.Process (callCommand, spawnCommand)
import WildBind.Input.NumPad (NumPadUnlocked(..))
import WildBind.Binding
  ( Binding,
    binds, on, as, run,
    whenFront
  )
import WildBind.X11 (winClass, ActiveWindow)


-- | Push a key
push :: MonadIO m => String -> m ()
push k = liftIO $ callCommand ("xdotool key " ++ k)

-- | Push a sequence of keys
pushes :: MonadIO m => [String] -> m ()
pushes [] = return ()
pushes (k:rest) = push k >> pushes rest

-- | Run a comman in background.
cmd' :: MonadIO m => String -> m ()
cmd' = liftIO . void . spawnCommand

-- | Basic, easily overridden bindings
base :: Binding s NumPadUnlocked
base = binds $ do
  on NumCenter `as` "Enter" `run` push "Return"

gimp :: Binding ActiveWindow NumPadUnlocked
gimp = whenFront (\w -> "Gimp" `isInfixOf` winClass w) $ binds $ do
  on NumCenter `as` "ペン" `run` push "p"
  on NumDelete `as` "鉛筆" `run` push "n"
  on NumLeft `as` "スポイト" `run` push "o"
  on NumRight `as` "消しゴム" `run` push "Shift+e"
  on NumHome `as` "矩形選択" `run` push "r"
  on NumUp `as` "色スワップ" `run` push "F12"
  on NumPageUp `as` "パス" `run` push "b"
  on NumEnd `as` "やり直し" `run` push "Ctrl+z"
  on NumDown `as` "縮小" `run` push "minus"
  on NumInsert `as` "保存" `run` push "Ctrl+s"
  on NumPageDown `as` "拡大" `run` push "plus"

