--module Player where

--data Player = MkPlayer deriving (Show)

--initPlayer :: Player
--initPlayer = undefined

--isPlayerAlive :: Player -> Bool
--isPlayerAlive p = undefined
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Player where
import Data.Monoid ()
import System.Random (Random (..), newStdGen, randomRs)
import Control.Lens (makeLenses, (%~), (&), (.~), (^.), _1, _2)
import Linear.V2 (V2 (..))
import qualified Data.Sequence as SEQ
import Test.QuickCheck

randomInt :: Int -> Int -> IO Int
randomInt lo hi = do
  gen <- newStdGen
  let (value, _) = randomR (lo, hi) gen
  return value

-- import Game (Time)
type Time = Int

type Coord = V2 Int

data PlayerFire = Cannon | Shotgun deriving (Show)
data FireMode = None | SingleDown | SingleShotgun | SingleCorner | SingleCross | SingleFlower | LazerDown | LazerCorner | LazerCross deriving (Show)
data MoveMode = Strike | Move | TurrentMove deriving (Show)
data Direction = Up | Dn | Lft | Rt | UL | UR | DL | DR | Stay deriving (Show)
data EnemyType = Fighter | Bomber | Starship | Turrent deriving (Show)

gridWidth :: Int
gridWidth = 50
gridHeight :: Int
gridHeight = 30
firePool :: [FireMode]
firePool = [None, SingleDown, SingleShotgun, SingleCorner, SingleCross, SingleFlower, LazerDown, LazerCorner, LazerCross]
movePool :: [MoveMode]
movePool = [Strike, Move, TurrentMove]
enemyPool :: [EnemyType]
enemyPool = [Fighter, Bomber, Starship, Turrent]
moveDirPool :: [Direction]
moveDirPool = [Up, Dn, Lft, Rt]

data PlayerPlane = Player {
    _coord :: Coord,
    _score :: Int,
    _playerHealth :: Int,
    _alive :: Bool,
    _playerFireRate :: Int,
    _playerFire :: PlayerFire
} deriving (Show)

data EnemyPlane = Enemy {
    _coords :: [Coord],
    _coordTurret :: Coord,
    _price :: Int,
    _enemyHealth :: Int,
    _killed :: Bool,
    _enemyFireRate :: Int,
    _fireMode :: FireMode,
    _moveMode :: MoveMode,
    _direction :: Direction
} deriving (Show)

data PlayerBullet = MyBullet {
    _coordBullet :: Coord,
    _playerDirection :: Direction,
    _exist :: Bool
} deriving (Show)

data EnemyBullet = EnemyBullet {
    _coordEnemy :: Coord,
    _enemyDirection :: Direction,
    _hit :: Bool
} deriving (Show)
--Generators
--generate... is the final step

makeLenses ''PlayerPlane
makeLenses ''EnemyPlane
makeLenses ''PlayerBullet
makeLenses ''EnemyBullet

generatePlayer :: PlayerPlane
generatePlayer = Player {
    _coord = (V2 (gridWidth `div` 2) 1),
    _score = 0,
    _playerHealth = 10,
    _alive = True,
    _playerFireRate = 1,
    _playerFire = Cannon
  }

generatePlayerCoords :: Coord -> [Coord]
generatePlayerCoords coord = [(coord + (V2 1 0)), (coord + (V2 2 0)), (coord - (V2 1 0)), (coord - (V2 2 0)), (coord + (V2 0 1)), (coord + (V2 0 2)), (coord + (V2 0 3)), (coord + (V2 (-1) (-1))), (coord + (V2 0 (-1))), (coord + (V2 1 (-1)))]

isPlayerAlive :: PlayerPlane -> Bool
isPlayerAlive p = (p ^. playerHealth <= 0) || (not (p ^.alive))

generateEnemy :: IO EnemyPlane
generateEnemy = do
  tp <- randomInt 0 3
  generateEnemy' (enemyPool !! tp)

generateEnemy' :: EnemyType -> IO EnemyPlane
generateEnemy' Turrent = do
  coord1 <- randomInt 0 (gridWidth - 1)
  createEnemy Turrent Dn (V2 coord1 (gridHeight - 1))
generateEnemy' Starship = do
  coord1 <- randomInt 0 (gridWidth - 1)
  createEnemy Starship Dn (V2 coord1 (gridHeight - 1))
generateEnemy' tp = do
  ele <- randomInt 0 3
  --dir <- elements moveDirPool
  let dir = moveDirPool!!ele
  coord <- generateCoord dir
  createEnemy tp dir coord

generateCoord :: Direction -> IO Coord
generateCoord Up = do
  coord1 <- randomInt 0 (gridWidth - 1)
  return (V2 coord1 0)
generateCoord Dn = do
  coord1 <- randomInt 0 (gridWidth - 1)
  return (V2 coord1 (gridHeight - 1))
generateCoord Lft = do
  coord1 <- randomInt 0 (gridHeight - 1)
  return (V2 (gridWidth - 1) coord1)
generateCoord Rt = do
  coord1 <- randomInt 0 (gridHeight - 1)
  return (V2 0 coord1)

createEnemy :: EnemyType -> Direction -> Coord -> IO EnemyPlane
createEnemy Fighter dir coord = do
  id <- randomInt 0 1
  return Enemy {
    _coords = [coord, (coord - (V2 1 0)), (coord + (V2 1 0)), (coord + (V2 0 1)), (coord - (V2 0 1))],
    _coordTurret = coord,
    _price = 100,
    _enemyHealth = 1,
    _killed = True,
    _enemyFireRate = 8,
    _fireMode = (firePool!!id),
    _moveMode = (movePool!!id),
    _direction = dir
  }
createEnemy Bomber dir coord = do
  id <- randomInt 1 2
  return Enemy {
    _coords = [coord, (coord - (V2 1 0)), (coord + (V2 1 0)), (coord + (V2 2 0)), (coord - (V2 2 0)), (coord + (V2 2 1)), (coord + (V2 2 (-1))), (coord + (V2 (-2) 1)), (coord + (V2 (-2) (-1)))],
    _coordTurret = coord,
    _price = 500,
    _enemyHealth = 4,
    _killed = True,
    _enemyFireRate = 8,
    _fireMode = (firePool!!id),
    _moveMode = Move,
    _direction = dir
  }
createEnemy Starship dir coord = do
  id1 <- randomInt 2 8
  id2 <- randomInt 1 2
  return Enemy {
    _coords = (createStarShipCoord coord 1),
    _coordTurret = coord,
    _price = 1500,
    _enemyHealth = 10,
    _killed = True,
    _enemyFireRate = 8,
    _fireMode = (firePool!!id1),
    _moveMode = case dir of
      Dn -> movePool!!id2
      otherwise -> movePool!!1,
    _direction = dir
  }

createEnemy Turrent dir coord = do
  id1 <- randomInt 2 8
  return Enemy {
    _coords = [(coord - (V2 1 0)), (coord + (V2 1 0)), (coord + (V2 0 1)), (coord - (V2 0 1)), (coord - (V2 1 1)), (coord + (V2 1 1)), (coord + (V2 1 (-1))), (coord - (V2 1 (-1)))],
    _coordTurret = coord,
    _price = 200,
    _enemyHealth = 2,
    _killed = True,
    _enemyFireRate = 10,
    _fireMode = (firePool!!id1),
    _moveMode = TurrentMove,
    _direction = Dn
  }

generateCoordsRt :: Coord -> Int -> [Coord]
-- generateCoordsRt _ _ = []
generateCoordsRt coord 0 = []
generateCoordsRt coord n = coord : (generateCoordsRt (coord + (V2 1 0)) (n - 1))
createStarShipCoord :: Coord -> Int -> [Coord]
createStarShipCoord coord 6 = generateCoordsRt coord 5
createStarShipCoord coord 5 = (generateCoordsRt coord 3) ++ (generateCoordsRt (coord + (V2 6 0)) 3) ++ (createStarShipCoord (coord + (V2 2 1)) (5 + 1))
createStarShipCoord coord n = generateCoordsRt coord ((2 * n) - 1) ++ (createStarShipCoord (coord + (V2 (-1) 1)) (n + 1))
--HitBehaviors
--on... is the final step

onEnemyCrash :: PlayerPlane -> EnemyPlane -> (PlayerPlane, EnemyPlane)
onEnemyCrash player enemy = if (enemyCrash player enemy)
  then ((player & playerHealth %~ (+ (- enemy^.enemyHealth))), (enemy & enemyHealth %~ (+ (- enemy^.enemyHealth))))
  else (player, enemy)

onBulletCrash :: PlayerPlane -> EnemyBullet -> (PlayerPlane, EnemyBullet)
onBulletCrash player enemy = if (bulletCrash player enemy)
  then ((player & playerHealth %~ (+ (- 1))), (enemy & hit .~ False))
  else (player, enemy)

onBulletHit :: PlayerBullet -> EnemyPlane -> (PlayerBullet, EnemyPlane)
onBulletHit player enemy = if (bulletHit player enemy)
    then ((player & exist .~ False), (enemy & enemyHealth %~ (+ (- 1))))
    else (player, enemy)

enemyCrash :: PlayerPlane -> EnemyPlane -> Bool
enemyCrash player enemy = (player ^. coord) `elem` (enemy ^. coords)

bulletCrash :: PlayerPlane -> EnemyBullet -> Bool
bulletCrash player enemy = (player^.coord) == (enemy^.coordEnemy)

bulletHit :: PlayerBullet -> EnemyPlane -> Bool
bulletHit player enemy = (player^.coordBullet) `elem` (enemy^.coords)

-- MoveBehaviors
-- move... is the final step
movePlayer :: Direction -> PlayerPlane -> PlayerPlane
movePlayer dir p = if (playerOutOfBoundary (onMove'' dir (p^.coord)))
  then p
  else p & coord %~ (onMove'' dir)

playerOutOfBoundary :: Coord -> Bool
playerOutOfBoundary coord = (coord ^. _2 > gridHeight - 1) || (coord ^. _2 < 0) || (coord ^. _1 > gridWidth - 1) || (coord ^. _1 < 0)

outOfBoundary :: Coord -> Bool
outOfBoundary coord = (coord ^. _2 > gridHeight - 1) || (coord ^. _2 < 0) || (coord ^. _1 > gridWidth - 1) || (coord ^. _1 < 0)

moveEnemy :: Time -> EnemyPlane -> EnemyPlane
moveEnemy t enemy = moveEnemy' t (enemy^.moveMode) (enemy^.direction) enemy

movePlayerBullet :: PlayerBullet -> PlayerBullet
movePlayerBullet p = p & coordBullet %~ (onMove'' (p^.playerDirection))

moveEnemyBullet :: EnemyBullet -> EnemyBullet
moveEnemyBullet e = e & coordEnemy %~ (onMove'' (e^.enemyDirection))

moveEnemy' :: Time -> MoveMode -> Direction -> EnemyPlane -> EnemyPlane
moveEnemy' t Strike dir enemy = if (t`mod` 2 == 0)
  then (enemy & coords %~ (onMove' dir)) & coordTurret %~ (onMove'' dir)
  else enemy
moveEnemy' t Move dir enemy = if (t`mod` 3 == 0)
  then (enemy & coords %~ (onMove' dir)) & coordTurret %~ (onMove'' dir)
  else enemy
moveEnemy' t TurrentMove dir enemy = if (t`mod` 3 == 0)
  then turrentMove (enemy^.coordTurret) dir enemy
  else enemy

turrentMove :: Coord -> Direction -> EnemyPlane -> EnemyPlane
turrentMove (V2 a1 a2) dir enemy = if (a2 > (gridHeight * 4 `div` 5))
  then (enemy & coords %~ (onMove' dir)) & coordTurret %~ (onMove'' dir)
  else enemy

onMove' :: Direction -> [Coord] -> [Coord]
onMove' dir coords = fmap (onMove'' dir) coords

onMove'' :: Direction -> Coord -> Coord
onMove'' Up coord = coord + (V2 0 1)
onMove'' Dn coord = coord + (V2 0 (-1))
onMove'' Lft coord = coord + (V2 (-1) 0)
onMove'' Rt coord = coord + (V2 1 0)
onMove'' UL coord = coord + (V2 (-1) 1)
onMove'' UR coord = coord + (V2 1 1)
onMove'' DL coord = coord + (V2 (-1) (-1))
onMove'' DR coord = coord + (V2 1 (-1))
onMove'' _ coord = coord

--Shooting creation
-- ...Shoot is the final
myBulletShoot :: PlayerPlane -> Time -> [PlayerBullet]
myBulletShoot player t =
  if (t `mod` (player ^. playerFireRate)) == 0
    then myBulletGenerate (player ^. playerFire) (player ^. coord)
    else []

enemyBulletShoot :: EnemyPlane -> Time -> [EnemyBullet]
enemyBulletShoot enemy t = enemyBulletShoot' (enemy ^. fireMode) (enemy ^. coordTurret) (enemy ^. enemyFireRate) t


createEnemyBullet :: Coord -> Direction -> EnemyBullet
createEnemyBullet coord direction = EnemyBullet {
    _coordEnemy = coord,
    _enemyDirection = direction,
    _hit= True
  }

createMyBullet :: Coord -> Direction -> PlayerBullet
createMyBullet coord direction = MyBullet {
    _coordBullet = coord,
    _playerDirection = direction,
    _exist = True
  }


enemyBulletShoot' :: FireMode -> Coord -> Int -> Time -> [EnemyBullet]
enemyBulletShoot' LazerDown turrent rate t = if (t `mod` (2 * rate)) < rate
  then enemyBulletGenerate LazerDown turrent
  else []
enemyBulletShoot' LazerCorner turrent rate t = if (t `mod` (2 * rate)) < rate
    then enemyBulletGenerate LazerCorner turrent
    else []
enemyBulletShoot' LazerCross turrent rate t = if (t `mod` (2 * rate)) < rate
    then enemyBulletGenerate LazerCorner turrent
    else []
enemyBulletShoot' md turrent rate t = if (t `mod` (rate)) == 0
    then enemyBulletGenerate md turrent
    else []

enemyBulletGenerate :: FireMode -> Coord -> [EnemyBullet]
enemyBulletGenerate SingleDown turrent = [createEnemyBullet turrent Dn]
enemyBulletGenerate SingleShotgun turrent = [(createEnemyBullet turrent Dn), (createEnemyBullet turrent DL), (createEnemyBullet turrent DR)]
enemyBulletGenerate SingleCorner turrent = [(createEnemyBullet turrent DL), (createEnemyBullet turrent DR),  (createEnemyBullet turrent UL), (createEnemyBullet turrent UR)]
enemyBulletGenerate SingleCross turrent =  [(createEnemyBullet turrent Dn), (createEnemyBullet turrent Up), (createEnemyBullet turrent Lft), (createEnemyBullet turrent Rt)]
enemyBulletGenerate SingleFlower turrent = [(createEnemyBullet turrent DL), (createEnemyBullet turrent DR),  (createEnemyBullet turrent UL), (createEnemyBullet turrent UR), (createEnemyBullet turrent Dn), (createEnemyBullet turrent Up), (createEnemyBullet turrent Lft), (createEnemyBullet turrent Rt)]
enemyBulletGenerate LazerDown turrent = [createEnemyBullet turrent Dn]
enemyBulletGenerate LazerCorner turrent = [(createEnemyBullet turrent DL), (createEnemyBullet turrent DR),  (createEnemyBullet turrent UL), (createEnemyBullet turrent UR)]
enemyBulletGenerate LazerCross turrent = [(createEnemyBullet turrent Dn), (createEnemyBullet turrent Up), (createEnemyBullet turrent Lft), (createEnemyBullet turrent Rt)]
enemyBulletGenerate _ _ = []

myBulletGenerate :: PlayerFire -> Coord -> [PlayerBullet]
myBulletGenerate Cannon turrent = [createMyBullet turrent Up]
myBulletGenerate Shotgun turrent = [createMyBullet turrent Up, createMyBullet turrent UL, createMyBullet turrent UR]
