{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GameScene (
    gameScene
) where

import GameEngine.Scene
import GameEngine.Sprite
import GameEngine.Sprite.Square
import GameEngine.Sprite.Label

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Graphics.Rendering.OpenGL hiding (position, color)
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

data SceneState = SceneState {
                      _stGridSize    :: GLfloat
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
    freeSans <- createExtrudeFont "font/FreeSans.ttf"
    setFontFaceSize freeSans 7 7
    setFontDepth freeSans 1.0
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
    let snakeHead = configureSprite $ do
                        color    .= Color4 0.0 1.0 1.0 1.0
                        position .= Vector3 (snakeX * gridSizef) (snakeY * gridSizef) 0
                        size     .= Vector3 gridSizef gridSizef 0
    return $ SceneState {
               _stGridSize    = gridSizef
             , _stFeed        = configureSprite $ do
                                    color    .= Color4 1.0 0.5 0.5 1.0
                                    position .= Vector3 (feedX * gridSizef) (feedY * gridSizef) 0
                                    size     .= Vector3 gridSizef gridSizef 0
             , _stWall        = flip map wallPosList $ \(x, y) -> configureSprite $ do
                                    color    .= Color4 0.2 0.0 0.0 1.0
                                    position .= Vector3 (x * gridSizef) (y * gridSizef) 0
                                    size     .= Vector3 gridSizef gridSizef 0
             , _stSnake       = [snakeHead]
             , _stSnakeLength = 1
             , _stDirection   = Stop
             , _stScoreLabel  = configureSprite $ do
                                    text     .= "Score: 0"
                                    color    .= Color4 1.0 1.0 1.0 1.0
                                    font     .= freeSans
                                    position .= Vector3 10 5 0
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
    gridSizef <- use stGridSize

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
                        let pos = Vector3 (feedX * gridSizef) (feedY * gridSizef) 0
                        if all (pos /=) forbidden
                            then return pos
                            else newFeedPosition
                Just <$> newFeedPosition
    case newPos of
        Nothing  -> return ()
        Just pos -> do
            stFeed . position .= pos
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
