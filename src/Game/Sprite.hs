{-# LANGUAGE TemplateHaskell #-}

module Game.Sprite (
    loadGLTextureFromFile,
    Sprite(..),
    defaultTextSprite,
    defaultSquareSprite,
    defaultTextureSprite,
    drawInWindow, move, hitTest,
    spText, spColor, spScale, spTex,
    spPos, spSize, spSpeed,
    x, y
) where

import Control.Lens
import Control.Monad.State (execState)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe (isJust, fromJust)
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.FTGL as FTGL
import Graphics.GLUtil (readTexture, texture2DWrap)

loadGLTextureFromFile :: FilePath -> IO TextureObject
loadGLTextureFromFile f = do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Mirrored, ClampToEdge)
    return t

data Sprite = TextSprite {
                _spText  :: String
              , _spColor :: Color4 GLfloat
              , _spFont  :: FTGL.Font
              , _spScale :: GLfloat
              , _spPos   :: Vertex2 GLfloat
              , _spSize  :: Vector2 GLfloat
              , _spSpeed :: Vector2 GLfloat
              }
            | SquareSprite {
                _spColor :: Color4 GLfloat
              , _spScale :: GLfloat
              , _spPos   :: Vertex2 GLfloat
              , _spSize  :: Vector2 GLfloat
              , _spSpeed :: Vector2 GLfloat
              }
            | TextureSprite {
                _spTex   :: TextureObject
              , _spScale :: GLfloat
              , _spPos   :: Vertex2 GLfloat
              , _spSize  :: Vector2 GLfloat
              , _spSpeed :: Vector2 GLfloat
              }

makeLenses ''Sprite
makePrisms ''Sprite

defaultTextSprite :: Sprite
defaultTextSprite = TextSprite {
                    _spText   = ""
                  , _spFont   = nullPtr
                  , _spColor  = Color4 0.0 0.0 0.0 1.0
                  , _spScale  = 1.0
                  , _spPos    = Vertex2 0 0
                  , _spSize   = Vector2 0 0
                  , _spSpeed  = Vector2 0 0
                  }

defaultSquareSprite :: Sprite
defaultSquareSprite = SquareSprite {
                      _spColor = Color4 0.0 0.0 0.0 1.0
                    , _spScale = 1.0
                    , _spPos    = Vertex2 0 0
                    , _spSize   = Vector2 0 0
                    , _spSpeed  = Vector2 0 0
                    }

defaultTextureSprite :: Sprite
defaultTextureSprite = TextureSprite {
                       _spTex = TextureObject 0
                     , _spScale = 1.0
                     , _spPos    = Vertex2 0 0
                     , _spSize   = Vector2 0 0
                     , _spSpeed  = Vector2 0 0
                     }

drawInWindow :: MonadIO m => Int -> Int -> Sprite -> m ()
drawInWindow width height sp | sp^?_TextSprite & isJust = do
                                  let (Vertex2 x y) = sp^.spPos
                                  let s = 0.01 * (sp^.spScale)
                                  let screenWidth x  = (1/s) * (4*x/(fromIntegral width)-1)
                                  let screenHeight y = (1/s) * (4*y/(fromIntegral height)-1)
                                  liftIO . preservingMatrix $ do
                                      scale s s s
                                      translate $ Vector3 (screenWidth x) (screenHeight y) 0.0
                                      traverse_ color $ sp^?spColor
                                      FTGL.renderFont (fromJust $ sp^?spFont) (fromJust $ sp^?spText) FTGL.Front
                             | sp^?_SquareSprite & isJust = do
                                  let (Vertex2 x y) = sp^.spPos
                                  let (Vector2 w h) = sp^.spSize
                                  let s = sp^.spScale
                                  let screenWidth x  = (1/s) * (4*x/(fromIntegral width)-1)
                                  let screenHeight y = (1/s) * (4*y/(fromIntegral height)-1)
                                  liftIO . preservingMatrix $ do
                                      scale s s s
                                      traverse_ color $ sp^?spColor
                                      renderPrimitive Quads $ do
                                          v (screenWidth x)     (screenHeight (y+h)) 0
                                          v (screenWidth x)     (screenHeight y)     0
                                          v (screenWidth (x+w)) (screenHeight y)     0
                                          v (screenWidth (x+w)) (screenHeight (y+h)) 0
                             | sp^?_TextureSprite & isJust = do
                                  let (Vertex2 x y) = sp^.spPos
                                  let (Vector2 w h) = sp^.spSize
                                  let s = sp^.spScale
                                  let screenWidth x  = (1/s) * (4*x/(fromIntegral width)-1)
                                  let screenHeight y = (1/s) * (4*y/(fromIntegral height)-1)
                                  liftIO . preservingMatrix $ do
                                      scale s s s
                                      textureBinding Texture2D $= (sp^?spTex)
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

move :: Sprite -> Sprite
move sp = (`execState` sp) $ do
    speedX <- use (spSpeed.x)
    speedY <- use (spSpeed.y)
    spPos.x += speedX
    spPos.y += speedY

hitTest :: Sprite -> Sprite -> Bool
hitTest s1 s2 = let Vertex2 xmin1 ymin1 = s1^.spPos
                    Vertex2 xmin2 ymin2 = s2^.spPos
                    Vector2 w1 h1 = s1^.spSize
                    Vector2 w2 h2 = s2^.spSize
                    xmax1 = xmin1 + w1
                    ymax1 = ymin1 + h1
                    xmax2 = xmin2 + w2
                    ymax2 = ymin2 + h2
                in not $ (xmin1 > xmax2) || (ymin1 > ymax2) || (xmax1 < xmin2) || (ymax1 < ymin2)

class HasX v where
    x :: Lens (v a) (v a) a a

instance HasX Vertex2 where
    x = lens (\(Vertex2 x _) -> x) (\(Vertex2 _ y) x' -> Vertex2 x' y)

instance HasX Vector2 where
    x = lens (\(Vector2 x _) -> x) (\(Vector2 _ y) x' -> Vector2 x' y)

class HasY v where
    y :: Lens (v a) (v a) a a

instance HasY Vertex2 where
    y = lens (\(Vertex2 _ y) -> y) (\(Vertex2 x _) y' -> Vertex2 x y')

instance HasY Vector2 where
    y = lens (\(Vector2 _ y) -> y) (\(Vector2 x _) y' -> Vector2 x y')
