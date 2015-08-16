module GameEngine (
    play
) where

import GameEngine.Scene

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Data.IORef
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess, exitFailure)
import System.IO

play :: Int        -- initial width of the window
     -> Int        -- initial height of the window
     -> String     -- title of the window
     -> Scene ()
     -> IO ()
play width height title scene = do
    let errorCallback _ description = hPutStrLn stderr description
    GLFW.setErrorCallback (Just errorCallback)
    successfulInit <- GLFW.init
    if not successfulInit
        then exitFailure
        else do
            mw <- GLFW.createWindow width height title Nothing Nothing
            case mw of
                Nothing -> GLFW.terminate >> exitFailure
                Just window -> do
                    -- OpenGL initialization
                    GLFW.makeContextCurrent mw
                    clearColor $= Color4 0.0 0.0 0.0 1.0
                    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
                    blend $= Enabled
                    texture Texture2D $= Enabled

                    -- Main Loop
                    let fps = 10
                    time  <- maybe 0 id <$> GLFW.getTime
                    let gameInfo = GameInfo { _fps             = fps
                                            , _lastTime        = time
                                            , _lastDisplayTime = time
                                            }
                    flip runReaderT window . flip evalStateT gameInfo . runExceptT $ scene

                    GLFW.destroyWindow window
                    GLFW.terminate
                    exitSuccess
