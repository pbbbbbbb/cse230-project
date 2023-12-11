module Controller where

import Shaft (Game (..), Tick(..), Name(..), step, initState, movePlayerSingleStep, Movement (Up, Down, Left, Right))
import Brick ( continue, halt, EventM, BrickEvent(VtyEvent, AppEvent), Next)
import qualified Graphics.Vty as V
import Control.Lens ((^.), (&), (.~), (%~))
import Prelude hiding (Up, Down, Right, Left)
import Linear (V2(V2))
import Control.Monad.IO.Class (liftIO)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                        = continue $ step g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ movePlayerSingleStep Up g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ movePlayerSingleStep Down g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ movePlayerSingleStep Left g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ movePlayerSingleStep Right g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))         = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') []))  = liftIO initState >>= continue
handleEvent g _ = continue g
