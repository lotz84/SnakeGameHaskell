module GameEngine.Sprite.Texture (
    loadGLTextureFromFile
  , TextureSprite
  , texture
) where

import GameEngine.Sprite

import Control.Lens
import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL hiding (position, texture)
import Graphics.GLUtil (readTexture, texture2DWrap)

loadGLTextureFromFile :: FilePath -> IO TextureObject
loadGLTextureFromFile f = do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Mirrored, ClampToEdge)
    return t

data TextureSprite = TextureSprite { _texture  :: TextureObject
                                   , _position :: Vector3 GLfloat
                                   , _size     :: Vector3 GLfloat
                                   , _velocity :: Vector3 GLfloat
                                   }

texture :: Lens' TextureSprite TextureObject
texture = lens _texture (\s x -> s {_texture = x})

instance Sprite TextureSprite where
    position = lens _position (\s x -> s {_position = x})
    rotation = lens (const (Vector3 0 0 0)) const
    size     = lens _size (\s x -> s {_size = x})
    defaultSprite = TextureSprite { _texture  = TextureObject 0
                                  , _position = Vector3 0 0 0
                                  , _size     = Vector3 0 0 0
                                  , _velocity = Vector3 0 0 0
                                  }
    drawInWindow width height sp = do
        let (Vector3 x y _) = sp^.position
        let (Vector3 w h _) = sp^.size
        let screenWidth x   = 4 * x / (fromIntegral width)  - 1
        let screenHeight y  = 4 * y / (fromIntegral height) - 1
        liftIO . preservingMatrix $ do
            textureBinding Texture2D $= (sp^?texture)
            renderPrimitive Quads $ do
                n 0 0 1
                t 0 0 >> v (screenWidth x)     (screenHeight (y+h)) 0
                t 0 1 >> v (screenWidth x)     (screenHeight y)     0
                t 1 1 >> v (screenWidth (x+w)) (screenHeight y)     0
                t 1 0 >> v (screenWidth (x+w)) (screenHeight (y+h)) 0
        where
        v x y z = vertex (Vertex3 x y z :: Vertex3 GLfloat)
        n x y z = normal (Normal3 x y z :: Normal3 GLfloat)
        t u v   = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)
