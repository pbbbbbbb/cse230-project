module UI where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    attrName,
    continue,
    customMain,
    emptyWidget,
    fill,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    padAll,
    padBottom,
    padLeft,
    padLeftRight,
    padRight,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import qualified Brick.BChan as BChan
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Controller (handleEvent)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Game
import qualified Graphics.Vty as V
import Lens.Micro (mapped, (^.))
import Linear.V2 (V2 (..))
import Player
  ( Time,
    alive,
    coord,
    coordBullet,
    coordEnemy,
    coords,
    gridHeight,
    gridWidth,
    playerHealth,
    score,
  )

data Cell
  = Empty
  | Player
  | Enemy
  | PlayerBullet
  | EnemyBullet

-- App definition
app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: Game -> [Widget Name]
drawUI g = [C.center (padRight (Pad 2) (drawStats g <+> drawGrid g))]

-- if g ^. paused
--   then [C.center (padRight (Pad 2) (drawStats g <+> drawPauseScreen))]
--   else [C.center (padRight (Pad 2) (drawStats g <+> drawGrid g))]

drawPauseScreen :: Widget Name
drawPauseScreen =
  withBorderStyle BS.unicodeRounded $
    hLimit (gridWidth + 2) $
      vLimit (gridHeight + 2) $
        C.hCenter $
          C.vCenter $
            B.borderWithLabel (str " Aircraft Shooting ") $
              vBox
                [ fill ' ',
                  vBox
                    [C.hCenter $ str "Game Paused", C.hCenter $ str "Press 'p' to resume"],
                  fill ' '
                ]

drawStats :: Game -> Widget Name
drawStats g =
  hLimit
    20
    ( vBox
        [ padTop (Pad 1) (drawScore1 (player g ^. score)),
          -- padTop (Pad 1) (drawScore2 (g ^. score)),
          padTop (Pad 1) (drawHealth1 (player g ^. playerHealth)),
          padTop (Pad 1) (drawTimer (timer g)),
          -- padTop (Pad 1) (drawHealth2 (g ^. health)),
          drawGameOver $ not (player g ^. alive)
        ]
    )

drawTimer :: Time -> Widget Name
drawTimer s =
  withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (str " Timer ") $
      C.hCenter $
        padAll 1 $
          str (show s)

drawScore1 :: Int -> Widget Name
drawScore1 s =
  withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (str " Player 1's Score") $
      C.hCenter $
        padAll 1 $
          str (show s)

-- drawScore2 :: Score -> Widget Name
-- drawScore2 s =
--   withBorderStyle BS.unicodeRounded $
--     B.borderWithLabel (str " Player 2's Score") $
--       C.hCenter $
--         padAll 1 $
--           str (show s)

drawHealth1 :: Int -> Widget Name
drawHealth1 s =
  withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (str " Player 1's Health ") $
      C.hCenter $
        padAll 1 $
          str (show s)

-- drawHealth2 :: Score -> Widget Name
-- drawHealth2 s =
--   withBorderStyle BS.unicodeRounded $
--     B.borderWithLabel (str " Player 2's Health ") $
--       C.hCenter $
--         padAll 1 $
--           str (show s)

-- drawMode :: Mode -> Widget Name
-- drawMode s =
--   withBorderStyle BS.unicodeRounded $
--     B.borderWithLabel (str " Mode ") $
--       C.hCenter $
--         padAll 1 $
--           str (show s)

drawGameOver :: Bool -> Widget Name
drawGameOver True = withAttr gameOverAttr $ C.hCenter $ str "Game Over"
drawGameOver False = emptyWidget

gameInit :: IO Game
gameInit = do
  channel <- BChan.newBChan 10
  forkIO $
    forever $ do
      BChan.writeBChan channel Tick
      threadDelay 400000
  let state = initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  customMain initialVty builder (Just channel) app state

drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (str " Aircraft Shooting ") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [gridHeight - 1, gridHeight - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. gridWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c == (player g ^. coord) = Player
      | any (\b -> c `elem` (b ^. coords)) (enemies g) = Enemy
      | any (\b -> c == (b ^. coordBullet)) (playerBullets g) = PlayerBullet
      | any (\b -> c == (b ^. coordEnemy)) (enemyBullets g) = EnemyBullet
      | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Empty = withAttr emptyAttr space
drawCell Player = withAttr playerAttr playerCell
drawCell Enemy = withAttr enemyAttr enemyCell
drawCell PlayerBullet = withAttr playerBulletAttr playerBulletCell
drawCell EnemyBullet = withAttr enemyBulletAttr enemyBulletCell

playerAttr :: AttrName
playerAttr = attrName "playerAttr"

enemyAttr :: AttrName
enemyAttr = attrName "enemyAttr"

playerBulletAttr :: AttrName
playerBulletAttr = attrName "playerBulletAttr"

enemyBulletAttr :: AttrName
enemyBulletAttr = attrName "enemyBulletAttr"

emptyAttr :: AttrName
emptyAttr = attrName "emptyAttr"

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

space :: Widget Name
space = str " "

playerCell :: Widget Name
playerCell = str "*"

enemyCell :: Widget Name
enemyCell = str "#"

playerBulletCell :: Widget Name
playerBulletCell = str "@"

enemyBulletCell :: Widget Name
enemyBulletCell = str "="

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playerAttr, V.white `on` V.white),
      (enemyAttr, fg V.white `V.withStyle` V.bold),
      (playerBulletAttr, fg V.blue `V.withStyle` V.bold),
      (enemyBulletAttr, fg V.red `V.withStyle` V.bold),
      (gameOverAttr, fg V.red `V.withStyle` V.bold)
    ]
