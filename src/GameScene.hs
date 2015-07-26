{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GameScene (
    gameScene
) where

import Game.Scene
import Game.Sprite

import ResultScene

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State
import Graphics.Rendering.OpenGL
import Graphics.Rendering.FTGL
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.UI.GLFW as GLFW
import System.Random

stageWidth :: Int
stageWidth = 640

stageHeight :: Int
stageHeight = 480

data SnakeDirection = Stop
                    | GoUp
                    | GoRight
                    | GoDown
                    | GoLeft
                    deriving (Show, Eq)

data GameSceneState = GameSceneState {
                      _stGridSize    :: GLfloat
                    , _stFeed        :: Sprite
                    , _stWall        :: [Sprite]
                    , _stSnake       :: [Sprite]
                    , _stSnakeLength :: Int
                    , _stDirection   :: SnakeDirection
                    , _stScoreLabel  :: Sprite
                    }

makeLenses ''GameSceneState

gameScene :: Scene GameSceneState ()
gameScene = Scene {
            construct = constructGameScene
          , keyHandler = keyHandlerGameScene
          , stepHandler = stepHandlerGameScene
          , drawHandler = drawHandlerGameScene
          }

constructGameScene :: () -> IO GameSceneState
constructGameScene () = do
    font <- createExtrudeFont "font/FreeSans.ttf"
    setFontFaceSize font 7 7
    setFontDepth font 1.0
    let gridSize    = 20
    let gridSizef   = fromIntegral gridSize
    let gridWidth   = stageWidth `div` gridSize
    let gridHeight  = stageHeight `div` gridSize
    let wallPosList =  [(fromIntegral x, fromIntegral y) | x <- [0, gridWidth - 1], y <- [0..gridHeight - 1]]
                    ++ [(fromIntegral x, fromIntegral y) | y <- [0, gridHeight - 1], x <- [0..gridWidth - 1]]
    snakeX <- fromIntegral <$> randomRIO (1, gridWidth - 2)
    snakeY <- fromIntegral <$> randomRIO (1, gridHeight - 2)
    feedX  <- fromIntegral <$> randomRIO (1, gridWidth - 2)
    feedY  <- fromIntegral <$> randomRIO (1, gridHeight - 2)
    let snakeHead = defaultSquareSprite {
                      _spColor = Color4 0.0 1.0 1.0 1.0
                    , _spPos   = Vertex2 (snakeX * gridSizef) (snakeY * gridSizef)
                    , _spSize  = Vector2 gridSizef gridSizef
                    }
    return $ GameSceneState {
               _stGridSize    = gridSizef
             , _stFeed        = defaultSquareSprite {
                                  _spColor = Color4 1.0 0.5 0.5 1.0
                                , _spPos   = Vertex2 (feedX * gridSizef) (feedY * gridSizef)
                                , _spSize  = Vector2 gridSizef gridSizef
                                }
             , _stWall        = flip map wallPosList $ \(x, y) ->
                                    defaultSquareSprite {
                                      _spColor = Color4 0.2 0.0 0.0 1.0
                                    , _spPos   = Vertex2 (x * gridSizef) (y * gridSizef)
                                    , _spSize  = Vector2 gridSizef gridSizef
                                    }
             , _stSnake       = [snakeHead]
             , _stSnakeLength = 1
             , _stDirection   = Stop
             , _stScoreLabel  = defaultTextSprite {
                                _spText = "Score: 0"
                              , _spColor = Color4 1.0 1.0 1.0 1.0
                              , _spFont  = font
                              , _spPos   = Vertex2 10 5
                              }
             }

keyHandlerGameScene :: GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> StateT GameSceneState IO ()
keyHandlerGameScene key keyState modifierKeys = do
    direction <- use stDirection
    case key of
        GLFW.Key'Up    -> when (direction /= GoDown)  $ stDirection .= GoUp
        GLFW.Key'W     -> when (direction /= GoDown)  $ stDirection .= GoUp
        GLFW.Key'Right -> when (direction /= GoLeft)  $ stDirection .= GoRight
        GLFW.Key'D     -> when (direction /= GoLeft)  $ stDirection .= GoRight
        GLFW.Key'Down  -> when (direction /= GoUp)    $ stDirection .= GoDown
        GLFW.Key'S     -> when (direction /= GoUp)    $ stDirection .= GoDown
        GLFW.Key'Left  -> when (direction /= GoRight) $ stDirection .= GoLeft
        GLFW.Key'A     -> when (direction /= GoRight) $ stDirection .= GoLeft
        _              -> return ()

stepHandlerGameScene :: (forall s' i'. Scene s' i' -> i' -> StateT GameSceneState IO ()) -> Double -> StateT GameSceneState IO ()
stepHandlerGameScene transit dt = do
    gridSizef <- use stGridSize

    -- Move Snake
    snake <- use stSnake
    direction <- use stDirection
    let snakeHead = head snake
    let spawnSnake = case direction of
            GoUp    -> Just $ snakeHead & spPos . y +~ gridSizef
            GoRight -> Just $ snakeHead & spPos . x +~ gridSizef
            GoDown  -> Just $ snakeHead & spPos . y -~ gridSizef
            GoLeft  -> Just $ snakeHead & spPos . x -~ gridSizef
            _       -> Nothing
    snakeLength <- use stSnakeLength
    stSnake .= (take snakeLength $ maybe id (\x -> (x:)) spawnSnake $ snake)

    -- Feed Snake
    snakeHeadPos <- use $ stSnake . to head . spPos
    feedPos      <- use $ stFeed . spPos
    newPos <- if snakeHeadPos /= feedPos
            then return Nothing
            else do
                snake <- use stSnake
                let forbidden = map (view spPos) snake
                let gridSize = floor $ gridSizef :: Int
                let gridWidth  = stageWidth `div` gridSize
                let gridHeight = stageHeight `div` gridSize
                let newFeedPosition = do
                        feedX <- liftIO $ fromIntegral <$> randomRIO (1, gridWidth - 2)
                        feedY <- liftIO $ fromIntegral <$> randomRIO (1, gridHeight - 2)
                        let pos = Vertex2 (feedX * gridSizef) (feedY * gridSizef)
                        if all (pos /=) forbidden
                            then return pos
                            else newFeedPosition
                Just <$> newFeedPosition
    case newPos of
        Nothing  -> return ()
        Just pos -> do
            stFeed . spPos .= pos
            oldLength <- use stSnakeLength
            stSnakeLength .= oldLength + 5

    snake <- use stSnake
    let snakeHead = head snake
    let snakeTail = tail snake
    wall  <- use stWall
    score <- (subtract 1) <$> use stSnakeLength
    if any ((snakeHead^.spPos)==) (map (view spPos) (snakeTail ++ wall))
        then transit resultScene score
        else return ()

    stScoreLabel.spText .= ("Score: " ++ show score)

drawHandlerGameScene :: (Sprite -> IO ()) -> GameSceneState -> IO ()
drawHandlerGameScene draw state = do
    mapM_ draw $ state ^. stWall
    mapM_ draw $ state ^. stSnake
    draw $ state ^. stFeed
    draw $ state ^. stScoreLabel
