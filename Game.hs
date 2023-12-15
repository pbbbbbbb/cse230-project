{-# LANGUAGE StrictData #-}
module Game where

import Bullet
import Player
import Control.Monad.State
import GHC.IO.Exception (IOErrorType(TimeExpired))
import Control.Lens (makeLenses, (%~), (&), (.~), (^.), _1, _2)
import Data.Maybe (fromMaybe)

-- im
-- type Time = Int

type Name = ()

data Tick
  = Tick

enemyGenerateRate :: Int
enemyGenerateRate = 100

data Game = Game {
  player        :: PlayerPlane,
  enemies       :: [EnemyPlane],
  playerBullets :: [PlayerBullet],
  enemyBullets  :: [EnemyBullet],
  timer         :: Time,
  paused        :: Bool
} | GameOver deriving (Show)

initGame :: Game
initGame = Game {
  player        = generatePlayer,
  enemies       = [],
  playerBullets = [],
  enemyBullets  = [],
  timer = 0,
  paused = False
}

tick :: Game -> IO Game
tick GameOver = return GameOver
tick g = do
  e' <- updateEnemies g
  return Game {
          player  = updatePlayer g,
          enemies = e',
          playerBullets = updatePlayerBullet g,
          enemyBullets  = updateEnemyBullet g,
          timer   = updateTimer g,
          paused = False
        }

-- tick :: Game -> Game
-- tick = evalState updateGame

-- updateGame :: State Game Game
-- updateGame = do
--   g <- get
--   if isOver g
--     then return GameOver
--     else do
--         e' <- updateEnemies g
--         return Game {
--         player  = updatePlayer g,
--         enemies = e',
--         playerBullets = updatePlayerBullet g,
--         enemyBullets  = updateEnemyBullet g,
--         timer   = updateTimer g
--       }

isOver :: Game -> Bool
isOver g = isPlayerAlive (player g)

updatePlayer :: Game -> PlayerPlane
updatePlayer GameOver = error "Game over, cannot update player."
updatePlayer Game{ player = p, enemies = e, enemyBullets = eb } = checkBulletCrash (checkEnemyCrash p e) eb

checkEnemyCrash :: PlayerPlane -> [EnemyPlane] -> PlayerPlane
checkEnemyCrash p []     = p
checkEnemyCrash p (e:es) =
  if enemyCrash p e
    then checkEnemyCrash (fst (onEnemyCrash p e)) es
    else checkEnemyCrash p es

checkBulletCrash :: PlayerPlane -> [EnemyBullet] -> PlayerPlane
checkBulletCrash p []    = p
checkBulletCrash p (b:bs) =
  if bulletCrash p b
    then checkBulletCrash (fst (onBulletCrash p b)) bs
    else checkBulletCrash p bs

updateEnemies :: Game -> IO [EnemyPlane]
updateEnemies GameOver = error "Game over, cannot update enemies."
updateEnemies Game{ player = p, enemies = e, playerBullets = pb, timer = t }
  = updateEnemyList p e pb t

updateEnemyList :: PlayerPlane -> [EnemyPlane] -> [PlayerBullet] -> Time -> IO [EnemyPlane]
updateEnemyList _ [] _ t
  | t `mod` enemyGenerateRate == 0 = do { e' <- generateEnemy; return [e']}
  | otherwise = return []
updateEnemyList p (e:es) pb t
  | t `mod` enemyGenerateRate == 0 =
      do e'  <- generateEnemy
         es' <- updateEnemyList p es pb t
         return (if isEnemyAlive e && inBoundary e
                    then e' : (updateEnemy p e pb t : es')
                    else e' : es')
  | otherwise = do es' <- updateEnemyList p es pb t
                   return (if isEnemyAlive e && inBoundary e
                              then updateEnemy p e pb t : es'
                              else es')

isEnemyAlive :: EnemyPlane -> Bool
isEnemyAlive e = _killed e

inBoundary :: EnemyPlane -> Bool
inBoundary e = not (outOfBoundary (_coordTurret e))

updateEnemy :: PlayerPlane -> EnemyPlane -> [PlayerBullet] -> Time -> EnemyPlane
updateEnemy p e pb t
  = moveEnemy t (checkPlayerCrash p (checkBulletHit pb e))

checkPlayerCrash :: PlayerPlane -> EnemyPlane -> EnemyPlane
checkPlayerCrash p e = if enemyCrash p e then (snd (onEnemyCrash p e)) else e

checkBulletHit :: [PlayerBullet] -> EnemyPlane -> EnemyPlane
checkBulletHit [] e = e
checkBulletHit (b:bs) e =
  if bulletHit b e
    then checkBulletHit bs (snd (onBulletHit b e))
    else checkBulletHit bs e

updatePlayerBullet :: Game -> [PlayerBullet]
updatePlayerBullet GameOver = error "Game over, cannot update enemy bullets."
updatePlayerBullet Game{ playerBullets = pb, enemies = e } = updatePlayerBulletList pb e

updatePlayerBulletList :: [PlayerBullet] -> [EnemyPlane] -> [PlayerBullet]
updatePlayerBulletList [] _ = []
updatePlayerBulletList (p:ps) es =
  if bulletHitList p es then updatePlayerBulletList ps es else p:(updatePlayerBulletList ps es)

bulletHitList :: PlayerBullet -> [EnemyPlane] -> Bool
bulletHitList p [] = False
bulletHitList p (e:es) = (bulletHit p e) || (bulletHitList p es)

updateEnemyBullet :: Game -> [EnemyBullet]
updateEnemyBullet GameOver = error "Game over, cannot update player bullets."
updateEnemyBullet Game { enemyBullets = eb, player = p }  = updateEnemyBulletList eb p

updateEnemyBulletList :: [EnemyBullet] -> PlayerPlane -> [EnemyBullet]
updateEnemyBulletList [] _ = []
updateEnemyBulletList (e:es) p =
  if bulletCrash p e then updateEnemyBulletList es p else e:(updateEnemyBulletList es p)

updateTimer :: Game -> Time
updateTimer GameOver = error "Game over, cannot update timer."
updateTimer Game { timer = t } = t + 1

movePlayerSingleStep :: Direction -> Game -> Game
movePlayerSingleStep dir game =
  case game of
    GameOver -> GameOver
    Game { player = p, enemies = es, playerBullets = pbs, enemyBullets = ebs, timer = t, paused = ps } ->
      if paused game then game
      else Game { player = movePlayer dir p, enemies = es, playerBullets = pbs, enemyBullets = ebs, timer = t, paused = ps }
