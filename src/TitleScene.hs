{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TitleScene (
    titleScene
) where

import GameEngine.Scene
import GameEngine.Sprite
import GameEngine.Sprite.Label
import GameEngine.Sprite.Colored

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Data.Color.Names
import qualified Graphics.UI.GLFW as GLFW
import System.Random

data SceneState = SceneState { _title1        :: LabelSprite
                             , _title2        :: LabelSprite
                             , _enterKeyPressed :: Bool
                             }

makeLenses ''SceneState

titleScene :: Scene ()
titleScene = do
    s <- liftIO $ initialSceneState
    makeScene s sceneGen

initialSceneState :: IO SceneState
initialSceneState = do
    freeSans <- loadFont "font/FreeSans.ttf"
    return $ SceneState {
               _title1 = configureSprite $ do
                             text       .= "Snake Game Haskell"
                             color      .= white
                             font       .= freeSans
                             scale      .= 2
                             position.x .= 120
                             position.y .= 240
             , _title2 = configureSprite $ do
                             text       .= "Press enter"
                             color      .= white
                             font       .= freeSans
                             scale      .= 1.5
                             position.x .= 240
                             position.y .= 180
             , _enterKeyPressed = False
             }

sceneGen :: SceneGen SceneState ()
sceneGen = SceneGen { keyHandler  = keyHandler'
                    , stepHandler = stepHandler'
                    , drawHandler = drawHandler'
                    }

keyHandler' :: GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> StateT SceneState IO ()
keyHandler' key _ _ = case key of
    GLFW.Key'Enter -> enterKeyPressed .= True
    _         -> return ()

stepHandler' :: Double -> ExceptT () (StateT SceneState IO) ()
stepHandler' dt = do
    gameStart <- use enterKeyPressed
    when gameStart $ exitScene ()

drawHandler' :: (Int, Int) -> SceneState -> IO ()
drawHandler' (w, h) state = do
    let draw = drawInWindow w h
    draw $ state ^. title1
    draw $ state ^. title2
