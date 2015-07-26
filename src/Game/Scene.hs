{-# LANGUAGE RankNTypes #-}

module Game.Scene (
    Scene(..)
) where

import Game.Sprite

import Control.Monad.State
import Data.Functor.Contravariant
import qualified Graphics.UI.GLFW as GLFW

data Scene s i = Scene { construct   :: i -> IO s
                       , keyHandler  :: GLFW.Key
                                     -> GLFW.KeyState
                                     -> GLFW.ModifierKeys
                                     -> StateT s IO ()
                       , stepHandler :: (forall s' i'. Scene s' i' -> i' -> StateT s IO ())
                                     -> Double -- the time between steps
                                     -> StateT s IO ()
                       , drawHandler :: (Sprite -> IO ())
                                     -> s
                                     -> IO ()
                       }

instance Contravariant (Scene s) where
    contramap f s = s { construct = (construct s) . f}
