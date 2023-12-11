module Controller where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, Next, continue, halt)
import Control.Lens ((%~), (&), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Linear (V2 (V2))
import Shaft
  ( Game (..),
    Movement (Down, Left, Right, Up),
    Name (..),
    Tick (..),
    initState,
    movePlayerSingleStep,
    paused,
    step,
  )
import Prelude hiding (Left, Right)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) =
  continue $ g & paused %~ not
handleEvent g (AppEvent Tick)
  | not (g ^. paused) = continue $ step g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') []))
  | not (g ^. paused) = continue $ movePlayerSingleStep Up g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') []))
  | not (g ^. paused) = continue $ movePlayerSingleStep Down g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') []))
  | not (g ^. paused) = continue $ movePlayerSingleStep Left g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') []))
  | not (g ^. paused) = continue $ movePlayerSingleStep Right g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) =
  liftIO initState >>= continue
handleEvent g _ = continue g
