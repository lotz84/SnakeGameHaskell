{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TitleScene (
    titleScene
) where

import GameEngine.Scene
import GameEngine.Sprite

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Graphics.Rendering.OpenGL
import Graphics.Rendering.FTGL
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.UI.GLFW as GLFW
import System.Random

data SceneState = SceneState {
                    _title1        :: Sprite
                  , _title2        :: Sprite
                  , _enterKeyPressed :: Bool
                  }

makeLenses ''SceneState

titleScene :: Scene ()
titleScene = do
    s <- liftIO $ initialSceneState
    makeScene s sceneGen

initialSceneState :: IO SceneState
initialSceneState = do
    font <- createExtrudeFont "font/FreeSans.ttf"
    setFontFaceSize font 7 7
    setFontDepth font 1.0
    return $ SceneState {
               _title1 = defaultTextSprite {
                           _spText  = "Snake Game Haskell"
                         , _spColor = Color4 1.0 1.0 1.0 1.0
                         , _spFont  = font
                         , _spScale = 2.0
                         , _spPos   = Vertex2 120 240
                         }
             , _title2 = defaultTextSprite {
                           _spText  = "Press enter"
                         , _spColor = Color4 1.0 1.0 1.0 1.0
                         , _spFont  = font
                         , _spScale = 1.5
                         , _spPos   = Vertex2 240 180
                         }
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

drawHandler' :: (Sprite -> IO ()) -> SceneState -> IO ()
drawHandler' draw state = do
    draw $ state ^. title1
    draw $ state ^. title2
