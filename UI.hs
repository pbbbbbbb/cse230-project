module UI where

import Shaft
  ( Game
  , Mode
  , Name
  , Score
  , Tick(..)
  , alive
  , gridHeight
  , gridWidth
  , health
  , initState
  , mode
  , player
  , score
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
import GHC.IO (unsafePerformIO)
import qualified Graphics.Vty as V
import Lens.Micro ((^.), mapped)
import Linear.V2 (V2(..))

import Brick
  ( App(..)
  , AttrMap
  , AttrName
  , BrickEvent(..)
  , EventM
  , Next
  , Padding(..)
  , Widget
  , (<+>)
  , attrMap
  , attrName
  , continue
  , customMain
  , emptyWidget
  , hBox
  , hLimit
  , halt
  , neverShowCursor
  , padAll
  , padBottom
  , padLeft
  , padRight
  , padTop
  , str
  , vBox
  , withAttr
  , withBorderStyle
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
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

drawUI :: Game -> [Widget Name]
drawUI g = [C.center (padRight (Pad 2) (drawStats g <+> drawGrid g))]

-- TODO: scores and healths for player 1 & 2
drawStats :: Game -> Widget Name
drawStats g =
  hLimit
    20
    (vBox
       [ padTop (Pad 1) (drawMode (g ^. mode))
       , padTop (Pad 1) (drawScore1 (g ^. score))
       , padTop (Pad 1) (drawScore2 (g ^. score))
       , padTop (Pad 1) (drawHealth1 (g ^. health))
       , padTop (Pad 1) (drawHealth2 (g ^. health))
       , drawGameOver (g ^. alive)
       ])

drawScore1 :: Score -> Widget Name
drawScore1 s =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str " Player 1's Score") $
  C.hCenter $ padAll 1 $ str (show s)

drawScore2 :: Score -> Widget Name
drawScore2 s =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str " Player 2's Score") $
  C.hCenter $ padAll 1 $ str (show s)

drawHealth1 :: Score -> Widget Name
drawHealth1 s =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str " Player 1's Health ") $
  C.hCenter $ padAll 1 $ str (show s)

drawHealth2 :: Score -> Widget Name
drawHealth2 s =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str " Player 2's Health ") $
  C.hCenter $ padAll 1 $ str (show s)

drawMode :: Mode -> Widget Name
drawMode s =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str " Mode ") $ C.hCenter $ padAll 1 $ str (show s)

drawGameOver :: Bool -> Widget Name
drawGameOver False = withAttr gameOverAttr $ C.hCenter $ str "Game Over"
drawGameOver _ = emptyWidget

gameInit :: IO Game
gameInit = do
  channel <- BChan.newBChan 10
  forkIO $
    forever $ do
      BChan.writeBChan channel Tick
      threadDelay 400000
  state <- initState
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  customMain initialVty builder (Just channel) app state

-- TODO: draw enemies, player bullets, enemy bullets
drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str " Aircraft Shooting ") $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [gridHeight - 1,gridHeight - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. gridWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` (g ^. player) = Player
      -- | c `elem` (g ^. enemies) = Enemy
      -- | c `elem` (g ^. playerBullets) = PlayerBullet
      -- | c `elem` (g ^. enemyBullets) = EnemyBullet
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
playerBulletCell = str "."

enemyBulletCell :: Widget Name
enemyBulletCell = str "."

plus :: Widget Name
plus = str "+"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playerAttr, V.white `on` V.white)
    , (enemyAttr, fg V.white `V.withStyle` V.bold)
    , (playerBulletAttr, fg V.blue `V.withStyle` V.bold)
    , (enemyBulletAttr, fg V.red `V.withStyle` V.bold)
    , (gameOverAttr, fg V.red `V.withStyle` V.bold)
    ]
