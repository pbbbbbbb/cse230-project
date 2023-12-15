{-# LANGUAGE StrictData #-}
module Game where

import Bullet
import Enemy
import Player
import Control.Monad.State
import GHC.IO.Exception (IOErrorType(TimeExpired))
-- im
-- type Time = Int

interval :: Int
interval = 100

data Game = Game {
  player        :: Player,
  enemies       :: [Enemy],
  playerBullets :: [Bullet],
  enemyBullets  :: [Bullet],
  timer         :: Time
} | GameOver deriving (Show)

initGame :: Game
initGame = Game {
  player        = initPlayer,
  enemies       = [],
  playerBullets = [],
  enemyBullets  = [],
  timer = 0
}

tick :: Game -> Game
tick = evalState updateGame

updateGame :: State Game Game
updateGame = do
  g <- get
  if isOver g
    then return GameOver
    else return Game {
      player  = updatePlayer g,
      enemies = updateEnemies g,
      playerBullets = updatePlayerBullet g,
      enemyBullets  = updateEnemyBullet g,
      timer   = updateTimer g
    }

isOver :: Game -> Bool
isOver g = isPlayerAlive (player g)

updatePlayer :: Game -> Player
updatePlayer GameOver = error "Game over, cannot update player."
updatePlayer Game{ player = p, enemies = e } = checkCollision p e

checkCollision :: Player -> [Enemy] -> Player
checkCollision = undefined

-- TODO check out of boundary

updateEnemies :: Game -> [Enemy]
updateEnemies GameOver = error "Game over, cannot update enemies."
updateEnemies Game{ enemies = e, timer = t } = updateEnemyList e t

updateEnemyList :: [Enemy] -> Time -> [Enemy]
updateEnemyList [] t     = [initEnemy | t `mod` interval == 0]
updateEnemyList (e:es) t =
  if isEnemyAlive e && inBoundary e
    then updateEnemy e t:updateEnemyList es t
    else updateEnemyList es t

inBoundary :: Enemy -> Bool
inBoundary e = undefined

updateEnemy :: Enemy -> Time -> Enemy
updateEnemy e t = undefined

updatePlayerBullet :: Game -> [Bullet]
updatePlayerBullet GameOver = error "Game over, cannot update enemy bullets."
updatePlayerBullet Game{ playerBullets = pb } = undefined

updateEnemyBullet :: Game -> [Bullet]
updateEnemyBullet GameOver = error "Game over, cannot update player bullets."
updateEnemyBullet Game { enemyBullets = eb }  = undefined

updateTimer :: Game -> Time
updateTimer GameOver = error "Game over, cannot update timer."
updateTimer Game { timer = t } = t + 1
