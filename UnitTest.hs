module UnitTest where

import Test.QuickCheck
import Player
import Game
import Linear.V2 (V2 (..))
import Control.Lens (makeLenses, (%~), (&), (.~), (^.), _1, _2)

testCreateEnemy :: EnemyPlane -> Bool
testCreateEnemy e = checkCreateEnemy (e ^. coordTurret) (e^.direction)
checkCreateEnemy :: Coord -> Direction -> Bool
checkCreateEnemy coord dir = case dir of
    Up -> ((coord ^. _2) == 0) && ((coord ^. _1) >=  0) && ((coord ^. _1) <=  gridWidth)
    Dn -> ((coord ^. _2) ==  gridHeight)  && ((coord ^. _1) >=  0) && ((coord ^. _1) <=  gridWidth)
    Lft -> ((coord ^. _1) ==  0) && ((coord ^. _2) >= 0) && ((coord ^. _2) <=  gridHeight)
    Rt -> ((coord ^. _1) ==  gridWidth) && ((coord ^. _2) >= 0) && ((coord ^. _2) <=  gridHeight)
    otherwise -> False

-- Check that all enemies in the list is alive and dead ones are removed
prop_enemy_alive :: Game -> Bool
prop_enemy_alive g = foldr (&&) True (map isEnemyAlive (enemies g))
