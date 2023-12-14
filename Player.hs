--module Player where

--data Player = MkPlayer deriving (Show)

--initPlayer :: Player
--initPlayer = undefined

--isPlayerAlive :: Player -> Bool
--isPlayerAlive p = undefined
module Player where
import Data.Monoid

import Linear.V2
import qualified Data.Sequence as SEQ
import Test.QuickCheck

type Coord = V2 Int

data PlayerFire = Cannon | Shotgun
data FireMode = None | SingleDown | SingleShotgun | SingleCorner | SingleCross | SingleFlower | LazerDown | LazerCorner | LazerCross
data MoveMode = Strike | Move | TurrentMove
data Direction = Up | Dn | Lft | Rt | UL | UR | DL | DR | Stay
data EnemyType = Fighter | Bomber | Starship | Turrent

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
    _health :: Health,
    _alive :: Bool,
    _fireRate :: Int,
    _fireMode :: PlayerFire
} deriving (Show)

data EnemyPlane = Enemy {
    _coord :: [Coord],
    _coordTurret :: Coord,
    _price :: Int,
    _health :: Health,
    _alive :: Bool,
    _fireRate :: Int,
    _fireMode :: FireMode,
    _moveMode :: MoveMode,
    _direction :: Direction
} deriving (Show)

data PlayerBullet = MyBullet {
    _coord :: Coord,
    _direction :: Direction,
    _alive :: Bool
} deriving (Show)

data EnemyBullet = EnemyBullet {
    _coord :: Coord,
    _direction :: Direction,
    _alive :: Bool
} deriving (Show)
--Generators
--generate... is the final step
generatePlayer :: PlayerPlane
generatePlayer = do
  return Player {
    _coord = (V2 (gridWidth `div` 2) 1),
    _score = 0,
    _health = 10,
    _alive = True,
    _fireRate = 3,
    _fireMode = Cannon
  }

generateEnemy :: EnemyType :: EnemyPlane
generateEnemy Turrent = do
  coord1 <- chooseInt(0 , gridWidth)
  createEnemy Turrent Dn (V2 coord1 gridHeight)
generateEnemy Starship = do
  coord1 <- chooseInt (0, gridWidth)
  createEnemy Starship Dn (V2 coord1 gridHeight)
generateEnemy tp = do
  dir <- elements moveDirPool
  coord <- generateCoord dir
  createEnemy tp dir coord

generateCoord :: Direction :: Gen Coord
generateCoord Up = do
  coord1 <- chooseInt (0, gridWidth)
  return (V2 coord1 0)
generateCoord Dn = do
  coord1 <- chooseInt (0, gridWidth)
  return (V2 coord1 gridHeight)
generateCoord Lft = do 
  coord1 <- chooseInt (0, gridHeight)
  return (V2 0 coord1)
generateCoord Rt = do
  coord1 <- chooseInt (0, gridHeight)
  return (V2 gridWidth coord1)

createEnemy :: EnemyType :: Direction :: Coord :: EmemyPlane
createEnemy Fighter dir coord = do
  id <- chooseInt (0, 1)
  return Enemy {
    _coord = [coord, (coord - (V2 1 0)), (coord + (V2 1 0)), (coord + (V2 0 1)), (coord - (V2 0 1))],
    _coordTurret = coord,
    _price = 100,
    _health = 1,
    _alive = True,
    _fireRate = 10,
    _fireMode = (firePool!!id),
    _moveMode = (movePool!!id),
    _direction = dir
  }
createEnemy Bomber dir coord = do
  id <- chooseInt (1, 2)
  return Enemy {
    _coord = [coord, (coord - (V2 0 1)), (coord + (V2 0 1)), (coord - (V2 1 0)), (coord + (V2 0 2)), (coord - (V2 0 2)), (coord + (V2 1 2)), (coord + (V2 (-1) 2)), (coord + (V2 1 (-2))), (coord + (V2 (-1) (-2)))],
    _coordTurret = coord,
    _price = 400,
    _health = 4,
    _alive = True,
    _fireRate = 10,
    _fireMode = (firePool!!id),
    _moveMode = Move,
    _direction = dir
  }
createEnemy Starship dir coord = do
  id1 <- chooseInt (2, 8)
  id2 <- chooseInt (1, 2)
  case dir of
    Dn -> let id3 = id2
    otherwise -> let id3 = 1
  return Enemy {
    _coord = (createStarShipCoord coord 1),
    _coordTurret = coord,
    _price = 1500,
    _health = 15,
    _alive = True,
    _fireRate = 10,
    _fireMode = (firePool!!id1),
    _moveMode = id3,
    _direction = dir
  }

createEnemy Turrent dir coord = do
  id1 <- chooseInt (2, 8)
  return Enemy {
    _coord = [(coord - (V2 1 0)), (coord + (V2 1 0)), (coord + (V2 0 1)), (coord - (V2 0 1)), (coord - (V2 1 1)), (coord + (V2 1 1)), (coord + (V2 1 (-1))), (coord - (V2 1 (-1)))],
    _coordTurret = coord,
    _price = 200,
    _health = 2,
    _alive = True,
    _fireRate = 10,
    _fireMode = (firePool!!id1),
    _moveMode = TurrentMove,
    _direction = Dn
  }

generateCoordsRt :: Coord -> Int -> [Coord]
generateCoordsRt coord 0 = []
generateCorrdsRt coord n = coord:(generateCoordsRt (coord + (V2 0 1)) (n - 1))
createStarShipCoord :: Coord -> Int -> [Coord]
createStarShipCoord coord 6 = generateCoordsRt coord 5
createStarShipCoord coord 5 = (generateCoordsRt coord 3) ++ (generateCoordsRt (coord + (V2 0 6)) 3) ++ (createStarShipCoord (coord + (V2 1 2)) (n + 1))
createStarShipCoord coord n = generateCoordsRt coord ((2 * n) - 1) ++ (createStarShipCoord (coord + (V2 1 -1)) (n + 1))
--HitBehaviors
--on... is the final step

onEnemyCrash :: PlayerPlane -> EnemyPlane -> (PlayerPlane, EnemyPlane)
onEnemyCrash player enemy = if (enemyCrash player enemy)
  then ((player & health %~ (+ (- enemy^.health))), (enemy & health %~ (+ (- enemy^.health))))
  else (player, enemy)

onBulletCrash :: PlayerPlane -> EnemyBullet -> (PlayerPlane, EnemyBullet)
onBulletCrash player enemy = if (bulletCrash player enemy)
  then ((player & health %~ (+ (- 1))), (enemy & alive .~ False))
  else (player, enemy)

onBulletHit :: PlayerBullet -> EnemyPlane -> Bool
onBulletHit player enemy = if (bulletHit player enemy)
    then ((player & alive .~ False), (enemy & health %~ (+ (- 1))))
    else (player, enemy)

enemyCrash :: PlayerPlane -> EnemyPlane -> Bool
enemyCrash player enemy = ((player^.coord) == (enemy^.coord))

bulletCrash :: PlayerPlane -> EnemyBullet -> Bool
bulletCrash player enemy = (player^.coord) 'elem' (enemy^.coord)

bulletHit :: PlayerBullet -> EnemyPlane -> Bool
bulletHit player enemy = (player^.coord) 'elem' (enemy^.coord)

-- MoveBehaviors
-- move... is the final step
movePlayer :: Direction -> PlayerPlane -> PlayerPlane
movePlayer dir p = if (outOfBoundary (onMove'' dir (p^.coord)))
  then p
  else p & coord %~ (onMove'' dir)

moveEnemy :: EnemyPlane -> EnemyPlane
moveEnemy enemy = moveEnemy' (enemy^.moveMode) (enemy^.direction) enemy

moveEnemy' :: MoveMode -> Direction -> EnemyPlane -> EnemyPlane
moveEnemy' Move dir enemy = (enemy & coord %~ (onMove' dir)) & coordTurret %~ (onMove'' dir)
moveEnemy' TurrentMove dir enemy = turrentMove (enemy^.coordTurret) dir enemy

turrentMove :: Coord -> Direction -> EnemyPlane -> EnemyPlane
turrentMove (V2 a1 a2) dir enemy = if (a2 > (gridHeight * 4 / 5))
  then (enemy & coord %~ (onMove' dir)) & coordTurret %~ (onMove'' dir)
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
createEnemyBullet :: Coord -> Direction -> EnemyBullet
createEnemyBullet coord direction = do
  return EnemyBullet {
    _coord = coord,
    _direction = direction,
    _alive = True
  }

createMyBullet :: Coord -> Direction -> MyBullet
createMyBullet coord direction = do
  return MyBullet {
    _coord = coord,
    _direction = direction,
    _alive = True
  }

enemyBulletShoot :: EnemyPlane -> Time -> [EnemyBullet]
enemyBulletShoot enemy t = enemyBulletShoot' (enemy ^.fireMode) (enemy ^.coordTurret) (enemy^.fireRate) t

enemyBulletShoot' :: FireMode -> Coord -> FireRate -> Time -> [EnemyBullet]
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

myBulletShoot :: PlayerPlane -> Time -> [MyBullet]
myBulletShoot player t =
  if (t `mod` (player^.fireRate)) == 0
    then myBulletGenerate (player^.fireMode) (player^.coord)
    else []
myBulletGenerate :: PlayerFire -> Coord -> [MyBullet]
myBulletGenerate Cannon turrent = [createMyBullet turrent Up]
myBulletGenerate Shotgun turrent = [(createMyBullet turrent Up), (createMyBullet turrent UL), (createMyBullet turrent UR)]
