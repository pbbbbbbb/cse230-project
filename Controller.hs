module Controller where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, Next, continue, halt)
import Control.Lens ((%~), (&), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Linear (V2 (V2))


import Game

import Player

import Prelude hiding (Left, Right)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) =
--   continue $ g & paused %~ not
handleEvent g (AppEvent Tick)
  | not (isPaused g) = liftIO (tick g) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') []))
  | not (isPaused g) = continue $ movePlayerSingleStep Up g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') []))
  | not (isPaused g) = continue $ movePlayerSingleStep Dn g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') []))
  | not (isPaused g) = continue $ movePlayerSingleStep Lft g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') []))
  | not (isPaused g) = continue $ movePlayerSingleStep Rt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) =
  continue initGame
handleEvent g _ = continue g
