{-# LANGUAGE StrictData #-}
module Game where

import Bullet
import Player
import Control.Monad.State
import GHC.IO.Exception (IOErrorType(TimeExpired))
import Control.Lens (makeLenses, (%~), (&), (.~), (^.), _1, _2)
import Data.Maybe (fromMaybe)

type Name = ()

data Tick
  = Tick

enemyGenerateRate :: Int
enemyGenerateRate = 25

bulletGenerateRate :: Int
bulletGenerateRate = 5

data Game = Game {
  player        :: PlayerPlane,
  enemies       :: [EnemyPlane],
  playerBullets :: [PlayerBullet],
  enemyBullets  :: [EnemyBullet],
  timer         :: Time,
  paused        :: Bool,
  gameOver      :: Bool
} deriving (Show)

initGame :: Game
initGame = Game {
  player        = generatePlayer,
  enemies       = [],
  playerBullets = [],
  enemyBullets  = [],
  timer = 0,
  paused = False,
  gameOver = False
}

tick :: Game -> IO Game
tick g
  | isOver g = return (setGameOver g)
  | otherwise = do
                  e' <- updateEnemies g
                  return Game {
                    player  = updatePlayer g,
                    enemies = e',
                    playerBullets = updatePlayerBullet g,
                    enemyBullets  = updateEnemyBullet g,
                    timer   = updateTimer g,
                    paused = False,
                    gameOver = False
                  }

isPaused :: Game -> Bool
isPaused g = paused g || gameOver g

isOver :: Game -> Bool
isOver g = isPlayerAlive (player g)

setGameOver :: Game -> Game
setGameOver g = Game {
    player = (player g),
    enemies = (enemies g),
    playerBullets = (playerBullets g),
    enemyBullets  = (enemyBullets g),
    timer = (timer g),
    paused = (paused g),
    gameOver = True
  }

setPause :: Game -> Game
setPause g = Game {
    player = (player g),
    enemies = (enemies g),
    playerBullets = (playerBullets g),
    enemyBullets  = (enemyBullets g),
    timer = (timer g),
    paused = not (paused g),
    gameOver = gameOver g
  }

updatePlayer :: Game -> PlayerPlane
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
isEnemyAlive e = (_killed e) && ((_enemyHealth e) > 0)

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
updatePlayerBullet g = map movePlayerBullet (updatePlayerBullet' g)

updatePlayerBullet' :: Game -> [PlayerBullet]
updatePlayerBullet' Game{ player = p, playerBullets = pb, enemies = e, timer = t } =
  (updatePlayerBulletList pb e) ++ (myBulletShoot p t)

updatePlayerBulletList :: [PlayerBullet] -> [EnemyPlane] -> [PlayerBullet]
updatePlayerBulletList [] _ = []
updatePlayerBulletList (p:ps) es =
  if bulletHitList p es then updatePlayerBulletList ps es else p:(updatePlayerBulletList ps es)

bulletHitList :: PlayerBullet -> [EnemyPlane] -> Bool
bulletHitList p [] = False
bulletHitList p (e:es) = (bulletHit p e) || (bulletHitList p es)

updateEnemyBullet :: Game -> [EnemyBullet]
updateEnemyBullet g = map moveEnemyBullet (updateEnemyBullet' g)

updateEnemyBullet' :: Game -> [EnemyBullet]
updateEnemyBullet' Game { enemies = e, enemyBullets = eb, player = p, timer = t }  =
  (updateEnemyBulletList eb p) ++ (enemiesShoot e t)

updateEnemyBulletList :: [EnemyBullet] -> PlayerPlane -> [EnemyBullet]
updateEnemyBulletList [] _ = []
updateEnemyBulletList (e:es) p =
  if bulletCrash p e then updateEnemyBulletList es p else e:(updateEnemyBulletList es p)

enemiesShoot :: [EnemyPlane] -> Time -> [EnemyBullet]
enemiesShoot [] t = []
enemiesShoot (e:es) t = (enemyBulletShoot e t) ++ (enemiesShoot es t)

updateTimer :: Game -> Time
updateTimer Game { timer = t } = t + 1

movePlayerSingleStep :: Direction -> Game -> Game
movePlayerSingleStep dir game =
  case game of
    Game { player = p, enemies = es, playerBullets = pbs, enemyBullets = ebs, timer = t, paused = ps } ->
      if paused game then game
      else Game { player = movePlayer dir p, enemies = es, playerBullets = pbs, enemyBullets = ebs, timer = t, paused = ps, gameOver = False}
