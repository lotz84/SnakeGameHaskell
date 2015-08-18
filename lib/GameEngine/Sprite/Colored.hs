module GameEngine.Sprite.Colored (
    Colored(..)
) where

import GameEngine.Sprite

import Control.Lens
import Graphics.Rendering.OpenGL hiding (color)
import Data.Color

instance HasRGB Color4 where
    fromRGB r g b = Color4 r g b 1.0

instance HasAlpha Color4 where
    _Alpha = lens (\(Color4 _ _ _ a) -> a) (\(Color4 r g b _) a -> Color4 r g b a)

class Sprite s => Colored s where
    color :: Lens' s (Color4 GLfloat)
