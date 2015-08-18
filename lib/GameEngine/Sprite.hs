module GameEngine.Sprite (
    Sprite(..)
  , Physical(..)
  , configureSprite
  , x, y, z
) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Graphics.Rendering.OpenGL

class Sprite s where
    position      :: Lens' s (Vector3 GLfloat)
    rotation      :: Lens' s (Vector3 GLfloat)
    size          :: Lens' s (Vector3 GLfloat)
    defaultSprite :: s
    drawInWindow  :: MonadIO m => Int -> Int -> s -> m ()

configureSprite :: Sprite s => State s () -> s
configureSprite state = execState state defaultSprite

class Sprite s => Physical s where
    velocity  :: Lens' s (Vector3 GLfloat)
    move      :: s -> s
    collision :: Physical p => s -> p -> Bool

class HasX v where
    x :: Lens' (v a) a

instance HasX Vertex2 where
    x = lens (\(Vertex2 x _) -> x) (\(Vertex2 _ y) x -> Vertex2 x y)

instance HasX Vector2 where
    x = lens (\(Vector2 x _) -> x) (\(Vector2 _ y) x -> Vector2 x y)

instance HasX Vertex3 where
    x = lens (\(Vertex3 x _ _) -> x) (\(Vertex3 _ y z) x -> Vertex3 x y z)

instance HasX Vector3 where
    x = lens (\(Vector3 x _ _) -> x) (\(Vector3 _ y z) x -> Vector3 x y z)


class HasY v where
    y :: Lens' (v a) a

instance HasY Vertex2 where
    y = lens (\(Vertex2 _ y) -> y) (\(Vertex2 x _) y -> Vertex2 x y)

instance HasY Vector2 where
    y = lens (\(Vector2 _ y) -> y) (\(Vector2 x _) y -> Vector2 x y)

instance HasY Vertex3 where
    y = lens (\(Vertex3 _ y _) -> y) (\(Vertex3 x _ z) y -> Vertex3 x y z)

instance HasY Vector3 where
    y = lens (\(Vector3 _ y _) -> y) (\(Vector3 x _ z) y -> Vector3 x y z)


class HasZ v where
    z :: Lens' (v a) a

instance HasZ Vertex3 where
    z = lens (\(Vertex3 _ _ z) -> z) (\(Vertex3 x y _) z -> Vertex3 x y z)

instance HasZ Vector3 where
    z = lens (\(Vector3 _ _ z) -> z) (\(Vector3 x y _) z -> Vector3 x y z)
