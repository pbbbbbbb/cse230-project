{-# LANGUAGE StrictData #-}
module Game where

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
  mode          :: Mode,
  paused        :: Bool,
  gameOver      :: Bool
} deriving (Show)

data Mode
  = Easy
  | Medium
  | Hard
  | Insane
  deriving (Eq, Show)

initGame :: Game
initGame = Game {
  player        = generatePlayer,
  enemies       = [],
  playerBullets = [],
  enemyBullets  = [],
  timer = 0,
  mode = Easy,
  paused = False,
  gameOver = False
}

-- Tick on each frame
tick :: Game -> IO Game
tick g
  | isOver g = return (setGameOver g)
  | otherwise = do
                  (e', sc) <- updateEnemies g
                  return Game {
                    player  = addScore (updatePlayer g) sc,
                    enemies = e',
                    playerBullets = updatePlayerBullet g,
                    enemyBullets  = updateEnemyBullet g,
                    timer   = updateTimer g,
                    mode    = changeMode g,
                    paused = False,
                    gameOver = False
                  }

-- Modify game state
addScore :: PlayerPlane -> Int -> PlayerPlane
addScore p sc = p & score %~ (+ sc)

changeMode :: Game -> Mode
changeMode g
  | (timer g) >= 600 = Insane
  | (timer g) >= 400 = Hard
  | (timer g) >= 200 = Medium
  | otherwise = Easy

getEnemyGenerateRate :: Mode -> Int
getEnemyGenerateRate Easy = 45
getEnemyGenerateRate Medium = 35
getEnemyGenerateRate Hard = 25
getEnemyGenerateRate Insane = 15

-- Game status
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
    mode = (mode g),
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
    mode = (mode g),
    paused = not (paused g),
    gameOver = gameOver g
  }

-- Player
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

movePlayerSingleStep :: Direction -> Game -> Game
movePlayerSingleStep dir game =
  case game of
    Game { player = p, enemies = es, playerBullets = pbs, enemyBullets = ebs, timer = t, mode = md, paused = ps } ->
      if paused game then game
      else Game { player = movePlayer dir p, enemies = es, playerBullets = pbs, enemyBullets = ebs, timer = t, mode = md, paused = ps, gameOver = False}


-- Enemies
updateEnemies :: Game -> IO ([EnemyPlane], Int)
updateEnemies Game{ player = p, enemies = e, playerBullets = pb, timer = t, mode = md}
  = updateEnemyList (getEnemyGenerateRate md) p e pb t

updateEnemyList :: Int -> PlayerPlane -> [EnemyPlane] -> [PlayerBullet] -> Time -> IO ([EnemyPlane], Int)
updateEnemyList egr _ [] _ t
  | t `mod` egr == 0 = do { e' <- generateEnemy; return ([e'], 0)}
  | otherwise = return ([], 0)
updateEnemyList egr p (e:es) pb t = do
                                      (es', sc) <- updateEnemyList egr p es pb t
                                      return (if isEnemyAlive e && inBoundary e
                                            then (updateEnemy p e pb t : es', sc)
                                            else (es', sc + (if isEnemyAlive e then 0 else (_price e))))

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

-- Player bullet
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

-- Enemy bullet
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

-- Timer
updateTimer :: Game -> Time
updateTimer Game { timer = t } = t + 1
