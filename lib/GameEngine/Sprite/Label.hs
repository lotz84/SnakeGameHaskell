{-# LANGUAGE TemplateHaskell #-}

module GameEngine.Sprite.Label (
    loadFont
  , LabelSprite
  , text
  , font
  , scale
) where

import GameEngine.Sprite
import GameEngine.Sprite.Colored

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe (fromJust)
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL hiding (position, color, scale)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.FTGL as FTGL

loadFont :: FilePath -> IO FTGL.Font
loadFont path = do
    font <- FTGL.createExtrudeFont path
    FTGL.setFontFaceSize font 7 7
    FTGL.setFontDepth font 1.0
    return font

data LabelSprite = LabelSprite { _text     :: String
                               , _color    :: Color4 GLfloat
                               , _font     :: FTGL.Font
                               , _scale    :: GLfloat
                               , _position :: Vector3 GLfloat
                               , _size     :: Vector3 GLfloat
                               , _velocity :: Vector3 GLfloat
                               }

text :: Lens' LabelSprite String
text = lens _text (\s x -> s {_text = x})

font :: Lens' LabelSprite FTGL.Font
font = lens _font (\s x -> s {_font = x})

scale :: Lens' LabelSprite GLfloat
scale = lens _scale (\s x -> s {_scale = x})

instance Sprite LabelSprite where
    position = lens _position (\s x -> s {_position = x})
    rotation = lens (const (Vector3 0 0 0)) const
    size     = lens _size (\s x -> s {_size = x})
    defaultSprite = LabelSprite { _text     = ""
                                , _font     = nullPtr
                                , _scale    = 1
                                , _color    = Color4 0 0 0 1
                                , _position = Vector3 0 0 0
                                , _size     = Vector3 0 0 0
                                , _velocity = Vector3 0 0 0
                                }
    drawInWindow width height sp = do
        let (Vector3 x y _) = sp^.position
        let s = 0.01 * (sp^.scale)
        let screenWidth  x = (1 / s) * (4 * x / (fromIntegral width)  - 1)
        let screenHeight y = (1 / s) * (4 * y / (fromIntegral height) - 1)
        liftIO . preservingMatrix $ do
            GL.scale s s s
            translate $ Vector3 (screenWidth x) (screenHeight y) 0.0
            traverse_ GL.color $ sp^?color
            FTGL.renderFont (fromJust $ sp^?font) (fromJust $ sp^?text) FTGL.Front

instance Colored LabelSprite where
    color = lens _color (\s x -> s {_color = x})
