{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GameScene (
    gameScene
) where

import GameEngine.Scene
import GameEngine.Sprite
import GameEngine.Sprite.Square
import GameEngine.Sprite.Label
import GameEngine.Sprite.Colored

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Data.Color
import Data.Color.Names
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

data SceneState = SceneState {
                      _stGridSize    :: Int
                    , _stFeed        :: SquareSprite
                    , _stWall        :: [SquareSprite]
                    , _stSnake       :: [SquareSprite]
                    , _stSnakeLength :: Int
                    , _stDirection   :: SnakeDirection
                    , _stScoreLabel  :: LabelSprite
                    }

makeLenses ''SceneState

gameScene :: Scene Int
gameScene = do
    s <- liftIO $ initialSceneState
    makeScene s sceneGen

sceneGen :: SceneGen SceneState Int
sceneGen = SceneGen { keyHandler  = keyHandler'
                    , stepHandler = stepHandler'
                    , drawHandler = drawHandler'
                    }

initialSceneState :: IO SceneState
initialSceneState = do
    freeSans <- loadFont "font/FreeSans.ttf"
    let gridSize    = 20
    let gridWidth   = stageWidth `div` gridSize
    let gridHeight  = stageHeight `div` gridSize
    let wallPosList =  [(x, y) | x <- [0, gridWidth - 1], y <- [0..gridHeight - 1]]
                    ++ [(x, y) | y <- [0, gridHeight - 1], x <- [0..gridWidth - 1]]
    snakeX <- randomRIO (1, gridWidth  - 2)
    snakeY <- randomRIO (1, gridHeight - 2)
    feedX  <- randomRIO (1, gridWidth  - 2)
    feedY  <- randomRIO (1, gridHeight - 2)
    let snakeHead = configureSprite $ do
                        color      .= cyan
                        position.x .= fromIntegral (snakeX * gridSize)
                        position.y .= fromIntegral (snakeY * gridSize)
                        size.x     .= fromIntegral gridSize
                        size.y     .= fromIntegral gridSize
    return $ SceneState {
               _stGridSize    = gridSize
             , _stFeed        = configureSprite $ do
                                    color      .= fromRGB 1.0 0.5 0.5
                                    position.x .= fromIntegral (feedX * gridSize)
                                    position.y .= fromIntegral (feedY * gridSize)
                                    size.x     .= fromIntegral gridSize
                                    size.y     .= fromIntegral gridSize
             , _stWall        = flip map wallPosList $ \(wx, wy) -> configureSprite $ do
                                    color      .= fromRGB 0.2 0.2 0.0
                                    position.x .= fromIntegral (wx * gridSize)
                                    position.y .= fromIntegral (wy * gridSize)
                                    size.x     .= fromIntegral gridSize
                                    size.y     .= fromIntegral gridSize
             , _stSnake       = [snakeHead]
             , _stSnakeLength = 1
             , _stDirection   = Stop
             , _stScoreLabel  = configureSprite $ do
                                    text       .= "Score: 0"
                                    color      .= white
                                    font       .= freeSans
                                    position.x .= 10
                                    position.y .= 5
             }

keyHandler' :: GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> StateT SceneState IO ()
keyHandler' key keyState modifierKeys = do
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

stepHandler' :: Double -> ExceptT Int (StateT SceneState IO) ()
stepHandler' dt = do
    gridSizef <- fromIntegral <$> use stGridSize

    -- Move Snake
    snake <- use stSnake
    direction <- use stDirection
    let snakeHead = head snake
    let spawnSnake = case direction of
            GoUp    -> Just $ snakeHead & position . y +~ gridSizef
            GoRight -> Just $ snakeHead & position . x +~ gridSizef
            GoDown  -> Just $ snakeHead & position . y -~ gridSizef
            GoLeft  -> Just $ snakeHead & position . x -~ gridSizef
            _       -> Nothing
    snakeLength <- use stSnakeLength
    stSnake .= (take snakeLength $ maybe id (\x -> (x:)) spawnSnake $ snake)

    -- Feed Snake
    snakeHeadPos <- use $ stSnake . to head . position
    feedPos      <- use $ stFeed . position
    newPos <- if snakeHeadPos /= feedPos
            then return Nothing
            else do
                snake <- use stSnake
                let forbidden = map (view position) snake
                let gridSize = floor $ gridSizef :: Int
                let gridWidth  = stageWidth `div` gridSize
                let gridHeight = stageHeight `div` gridSize
                let newFeedPosition = do
                        feedX <- liftIO $ fromIntegral <$> randomRIO (1, gridWidth - 2)
                        feedY <- liftIO $ fromIntegral <$> randomRIO (1, gridHeight - 2)
                        let pos = (feedX * gridSizef, feedY * gridSizef)
                        if all (\f -> ((f^.x) /= fst pos) && ((f^.y) /= snd pos)) forbidden
                            then return pos
                            else newFeedPosition
                Just <$> newFeedPosition
    case newPos of
        Nothing  -> return ()
        Just (feedX, feedY) -> do
            stFeed . position.x .= feedX
            stFeed . position.y .= feedY
            oldLength <- use stSnakeLength
            stSnakeLength .= oldLength + 5

    snake <- use stSnake
    let snakeHead = head snake
    let snakeTail = tail snake
    wall  <- use stWall
    score <- (subtract 1) <$> use stSnakeLength
    if any ((snakeHead^.position)==) (map (view position) (snakeTail ++ wall))
        then exitScene score
        else return ()

    stScoreLabel.text .= ("Score: " ++ show score)

drawHandler' :: (Int, Int) -> SceneState -> IO ()
drawHandler' (w, h) state = do
    let draw = drawInWindow w h
    mapM_ draw $ state ^. stWall
    mapM_ draw $ state ^. stSnake
    draw $ state ^. stFeed
    drawInWindow w h $ state ^. stScoreLabel
