{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TitleScene (
    titleScene
) where

import Game.Scene
import Game.Sprite

import GameScene

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State
import Graphics.Rendering.OpenGL
import Graphics.Rendering.FTGL
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.UI.GLFW as GLFW
import System.Random

data TitleSceneState = TitleSceneState {
                         _title1        :: Sprite
                       , _title2        :: Sprite
                       , _enterKeyPressed :: Bool
                       }

makeLenses '' TitleSceneState

titleScene :: Scene TitleSceneState ()
titleScene = Scene {
             construct = constructTitleScene
           , keyHandler = keyHandlerTitleScene
           , stepHandler = stepHandlerTitleScene
           , drawHandler = drawHandlerTitleScene
           }

constructTitleScene :: () -> IO TitleSceneState
constructTitleScene = const $ do
    font <- createExtrudeFont "font/FreeSans.ttf"
    setFontFaceSize font 7 7
    setFontDepth font 1.0
    return $ TitleSceneState {
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

keyHandlerTitleScene :: GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> StateT TitleSceneState IO ()
keyHandlerTitleScene key _ _ = case key of
    GLFW.Key'Enter -> enterKeyPressed .= True
    _         -> return ()

stepHandlerTitleScene :: (forall s' i'. Scene s' i' -> i' -> StateT TitleSceneState IO ()) -> Double -> StateT TitleSceneState IO ()
stepHandlerTitleScene transit dt = do
    gameStart <- use enterKeyPressed
    when gameStart $ transit gameScene ()

drawHandlerTitleScene :: (Sprite -> IO ()) -> TitleSceneState -> IO ()
drawHandlerTitleScene draw state = do
    draw $ state ^. title1
    draw $ state ^. title2
