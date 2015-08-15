{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Scene (
    play
  , SceneGen(..)
  , Scene
  , exitScene
  , makeScene
  , GameInfo(..)
) where

import Game.Sprite

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

data SceneGen s a = SceneGen { keyHandler  :: GLFW.Key
                                           -> GLFW.KeyState
                                           -> GLFW.ModifierKeys
                                           -> StateT s IO ()
                             , stepHandler :: Double -- the time between steps
                                           -> ExceptT a (StateT s IO) ()
                             , drawHandler :: (Sprite -> IO ())
                                           -> s
                                           -> IO ()
                             }

data GameInfo = GameInfo { _fps             :: Double
                         , _lastTime        :: Double
                         , _lastDisplayTime :: Double
                         }
makeLenses ''GameInfo

data GameException = GameWindowShouldClose

type Scene = ExceptT GameException (StateT GameInfo (ReaderT GLFW.Window IO))

exitScene :: a -> ExceptT a (StateT s IO) ()
exitScene = throwError

makeScene :: s -> SceneGen s a -> Scene a
makeScene gameState sceneGen = do
    window   <- ask
    stateRef <- liftIO $ newIORef gameState
    liftIO $ GLFW.setKeyCallback window . Just $ \_ key _ keyState modifierKeys -> do
        s  <- readIORef stateRef
        s' <- flip execStateT s $ keyHandler sceneGen key keyState modifierKeys
        writeIORef stateRef s'
    fix $ \loop -> do
        nowFps             <- use fps
        nowLastTime        <- use lastTime
        nowLastDisplayTime <- use lastDisplayTime

        shouldClose <- liftIO $ GLFW.windowShouldClose window
        if shouldClose
            then throwError GameWindowShouldClose
            else do
                after <- liftIO $ maybe 0 id <$> GLFW.getTime
                lastTime .= after
                if (after - nowLastDisplayTime) < 1/nowFps
                    then loop
                    else do
                        lastDisplayTime .= after
                        (width, height) <- liftIO $ GLFW.getFramebufferSize window
                        liftIO $ do
                            viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
                            clear [ColorBuffer, DepthBuffer]

                            matrixMode $= Projection
                            loadIdentity
                            lookAt (Vertex3 0 0 1) (Vertex3 0 0 0) (Vector3 0 1 0)

                        liftIO $ matrixMode $= Modelview 0
                        s       <- liftIO $ readIORef stateRef
                        (e, s') <- liftIO $ flip runStateT s . runExceptT $ stepHandler sceneGen (after - nowLastTime)
                        case e of
                            Left result -> return result
                            Right _ -> do
                                liftIO $ do
                                    drawHandler sceneGen (drawInWindow width height) s'
                                    writeIORef stateRef s'
                                    GLFW.swapBuffers window
                                    GLFW.pollEvents
                                loop

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
