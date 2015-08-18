module GameEngine.Sprite.Square (
    SquareSprite
) where

import GameEngine.Sprite
import GameEngine.Sprite.Colored

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Graphics.Rendering.OpenGL hiding (position, color)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.FTGL as FTGL

data SquareSprite = SquareSprite { _color    :: Color4 GLfloat
                                 , _position :: Vector3 GLfloat
                                 , _size     :: Vector3 GLfloat
                                 , _velocity :: Vector3 GLfloat
                                 }

instance Sprite SquareSprite where
    position = lens _position (\s x -> s {_position = x})
    rotation = lens (const (Vector3 0 0 0)) const
    size     = lens _size (\s x -> s {_size = x})
    defaultSprite = SquareSprite { _color    = Color4 0 0 0 1
                                 , _position = Vector3 0 0 0
                                 , _size     = Vector3 0 0 0
                                 , _velocity = Vector3 0 0 0
                                 }
    drawInWindow width height sp = do
        let (Vector3 x y _) = sp^.position
        let (Vector3 w h _) = sp^.size
        let screenWidth  x  = 4 * x / (fromIntegral width)  - 1
        let screenHeight y  = 4 * y / (fromIntegral height) - 1
        liftIO . preservingMatrix $ do
            traverse_ GL.color $ sp^?color
            renderPrimitive Quads $ do
                v (screenWidth x)     (screenHeight (y+h)) 0
                v (screenWidth x)     (screenHeight y)     0
                v (screenWidth (x+w)) (screenHeight y)     0
                v (screenWidth (x+w)) (screenHeight (y+h)) 0
        where
        v x y z = vertex (Vertex3 x y z :: Vertex3 GLfloat)

instance Colored SquareSprite where
    color = lens _color (\s x -> s {_color = x})
