{-# LANGUAGE TemplateHaskell #-}

module ResultScene (
    resultScene
) where

import GameEngine.Scene
import GameEngine.Sprite
import GameEngine.Sprite.Label

import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import Graphics.Rendering.OpenGL hiding (color, position, scale)
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.UI.GLFW as GLFW

data SceneState = SceneState {
                         _title1          :: LabelSprite
                       , _title2          :: LabelSprite
                       , _enterKeyPressed :: Bool
                       }

makeLenses '' SceneState

resultScene :: Int -> Scene ()
resultScene score = do
    s <- liftIO $ initialSceneState score
    makeScene s sceneGen

sceneGen :: SceneGen SceneState ()
sceneGen = SceneGen { keyHandler  = keyHandler'
                    , stepHandler = stepHandler'
                    , drawHandler = drawHandler'
                    }

initialSceneState :: Int -> IO SceneState
initialSceneState score = do
    freeSans <- FTGL.createExtrudeFont "font/FreeSans.ttf"
    FTGL.setFontFaceSize freeSans 7 7
    FTGL.setFontDepth freeSans 1.0
    return $ SceneState { _title1 = configureSprite $ do
                                        text     .= ("Score: " ++ show score)
                                        color    .= Color4 1.0 1.0 1.0 1.0
                                        font     .= freeSans
                                        scale    .= 2
                                        position .= Vector3 240 240 0
                        , _title2 = configureSprite $ do
                                        text     .= "Press Enter"
                                        color    .= Color4 1.0 1.0 1.0 1.0
                                        font     .= freeSans
                                        position .= Vector3 260 180 0
                        , _enterKeyPressed = False
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
