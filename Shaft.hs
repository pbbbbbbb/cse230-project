{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Shaft where

import Data.Monoid

import System.Random (Random(..), randomRs, newStdGen)
import Linear.V2 (V2(..))
import qualified Data.Sequence as SEQ
import Control.Lens (makeLenses, (^.), (.~), (%~), (&), _1, _2)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Prelude hiding (Right, Left, Up, Down)
import Data.Sequence (ViewR(EmptyR, (:>)), viewr, (|>), ViewL (EmptyL, (:<)), viewl, singleton)
import Graphics.Vty.PictureToSpans (isOutOfBounds)
import Data.List (findIndex)
import GHC.IO.Handle.Types (Handle__(haDecoder))

type Name = ()
type Coord = V2 Int
type Score = Int
type Health = Int
type Depth = Int

type Player = [Coord]
data Tick = Tick
data Movement = Up | Down | Left | Right

data Mode = Easy | Medium | Hard deriving (Eq, Show)
data ModeMap = ModeMap
  {
    _easy   :: Modes,
    _medium :: Modes,
    _hard   :: Modes
  }
  deriving (Eq, Show)
type Frequency = [Int]
data Modes = Modes
  {
    _x                    :: [Int],
    _y                    :: [Int]
  }
  deriving (Eq, Show)

data Game = Game {
  _player     :: Player,
  _score      :: Score,
  _health     :: Health,
  _alive      :: Bool,
  _modeMap    :: ModeMap,
  _mode       :: Mode,
  -- _lastPlatformDepth  :: LastDepth,
  _time       :: Int
} deriving (Show)

makeLenses ''Game
makeLenses ''ModeMap
makeLenses ''Modes

gridWidth :: Int
gridWidth = 50
gridHeight :: Int
gridHeight = 30

initPlayer :: Player
initPlayer= [V2 (gridWidth `div` 2) (3)]


initState :: IO Game
initState = do
  mode <- modeMaps
  return Game {
    _player     = initPlayer,
    _score      = 0,
    _health     = 10,
    _alive      = True,
    _modeMap          = mode,
    _mode             = Easy,
    _time   = 0
  }

modeMaps :: IO ModeMap
modeMaps = do
  x    <- randomRs (0, gridWidth) <$> newStdGen
  y    <- randomRs (0, last initPlayer^._2) <$> newStdGen
  return $ ModeMap
    (Modes x y)
    (Modes x y)
    (Modes x y)

getModes :: Game -> Modes
getModes g = case g^.mode of
                Easy -> g^.modeMap.easy
                Medium -> g^.modeMap.medium
                Hard -> g^.modeMap.hard

setModes :: Modes -> Game -> Game
setModes m g = case g^.mode of
                  Easy -> g & modeMap.easy .~ m
                  Medium -> g & modeMap.medium .~ m
                  Hard -> g & modeMap.hard .~ m

modesOfTime :: [Int]
modesOfTime = [200, 50, 0]

modesType :: [Mode]
modesType = [Hard, Medium, Easy]

findMode :: Depth -> Maybe Int
findMode d = findIndex (d >=) modesOfTime

changeMode :: Game -> Game
changeMode g = case findMode (g^.time) of
                  Just x -> g & mode .~ (modesType !! x)
                  Nothing -> g

step  :: Game -> Game
step g = fromMaybe g $ do
  guard $ g^.alive
  return $ fromMaybe (step' g) (checkAlive g)

-- TODO: add more events
step' :: Game -> Game
step' = changeMode . incTime . move

move :: Game -> Game
move = moveEnemies . movePlayerBullets . moveEnemyBullets

incTime :: Game -> Game
incTime g = g & time %~ (+1)

afterMoveSignleStep :: Game -> Game
afterMoveSignleStep g = fromMaybe g $ do
                        guard (not $ isDead g)
                        return g

movePlayerSingleStep :: Movement -> Game -> Game
movePlayerSingleStep Up g    = if shouldUp g && g^.alive then afterMoveSignleStep (movePlayer Up g) else g
movePlayerSingleStep Down g  = if shouldDown g && g^.alive then afterMoveSignleStep (movePlayer Down g) else g
movePlayerSingleStep Left g  = if shouldLeft g && g^.alive then afterMoveSignleStep (movePlayer Left g) else g
movePlayerSingleStep Right g = if shouldRight g && g^.alive then afterMoveSignleStep (movePlayer Right g)  else g

movePlayer :: Movement -> Game -> Game
movePlayer dir g =
  case dir of
    Up  -> if shouldUp g then g & player %~ fmap (+ V2 0 1) else g
    Down -> if shouldDown g then g & player %~ fmap (+ V2 0 (-1)) else g
    Left  -> if shouldLeft g then g & player %~ fmap (+ V2 (-1) 0) else g
    Right -> if shouldRight g then g & player %~ fmap (+ V2 1 0) else g

shouldUp :: Game -> Bool
shouldUp g = shouldUp' [coord^._2 | coord <- g^.player]

shouldUp' :: [Int] -> Bool
shouldUp' xs = (xs /= []) && maximum xs < gridHeight - 1

shouldDown :: Game -> Bool
shouldDown g = shouldDown' [coord^._2 | coord <- g^.player]

shouldDown' :: [Int] -> Bool
shouldDown' xs = (xs /= []) && maximum xs > 0

shouldLeft :: Game -> Bool
shouldLeft g = shouldLeft' [coord^._1 | coord <- g^.player]

shouldLeft' :: [Int] -> Bool
shouldLeft' xs = (xs /= []) && minimum xs > 0

shouldRight :: Game -> Bool
shouldRight g = shouldRight' [coord^._1 | coord <- g^.player]

shouldRight' :: [Int] -> Bool
shouldRight' xs = (xs /= []) && minimum xs < gridWidth - 1

-- TODO
moveEnemies :: Game -> Game
moveEnemies g = g

-- TODO
movePlayerBullets :: Game -> Game
movePlayerBullets g = g

-- TODO
moveEnemyBullets :: Game -> Game
moveEnemyBullets g = g

checkAlive :: Game -> Maybe Game
checkAlive g = do
  guard $ isDead g
  return $ g & alive .~ False

isDead :: Game -> Bool
isDead g = let player' = g^.player
              --  platforms' = g^.platforms
               health' = g^.health
              in ((last player')^._2) < 0 || health' <= 0
