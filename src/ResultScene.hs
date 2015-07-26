{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ResultScene (
    resultScene
) where

import Game.Scene
import Game.Sprite

-- import TitleScene

import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.UI.GLFW as GLFW

data SceneState = SceneState {
                         _title1        :: Sprite
                       , _title2        :: Sprite
                       , _enterKeyPressed :: Bool
                       }

makeLenses '' SceneState

resultScene :: Scene SceneState Int
resultScene = Scene {
             construct = construct'
           , keyHandler = keyHandler'
           , stepHandler = stepHandler'
           , drawHandler = drawHandler'
           }

construct' :: Int -> IO SceneState
construct' score = do
    font <- FTGL.createExtrudeFont "font/FreeSans.ttf"
    FTGL.setFontFaceSize font 7 7
    FTGL.setFontDepth font 1.0
    return $ SceneState {
               _title1 = defaultTextSprite {
                           _spText  = "Score: " ++ show score
                         , _spColor = Color4 1.0 1.0 1.0 1.0
                         , _spFont  = font
                         , _spScale = 2.0
                         , _spPos   = Vertex2 240 240
                         }
             , _title2 = defaultTextSprite {
                           _spText  = "Press Enter"
                         , _spColor = Color4 1.0 1.0 1.0 1.0
                         , _spFont  = font
                         , _spScale = 1.0
                         , _spPos   = Vertex2 260 180
                         }
             , _enterKeyPressed = False
             }

keyHandler' :: GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> StateT SceneState IO ()
keyHandler' key _ _ = case key of
    GLFW.Key'Enter -> enterKeyPressed .= True
    _         -> return ()

stepHandler' :: (forall s' i'. Scene s' i' -> i' -> StateT SceneState IO ()) -> Double -> StateT SceneState IO ()
stepHandler' transit dt = do
    gameStart <- use enterKeyPressed
    -- when gameStart $ transit titleScene ()
    return ()

drawHandler' :: (Sprite -> IO ()) -> SceneState -> IO ()
drawHandler' draw state = do
    draw $ state ^. title1
    draw $ state ^. title2