{-# LANGUAGE RankNTypes #-}

module Game.Utils (
    play
) where

import Game.Scene
import Game.Sprite

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Cont
import Data.IORef
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess, exitFailure)
import System.IO

play :: Int        -- initial width of the window
     -> Int        -- initial height of the window
     -> String     -- title of the window
     -> Scene s () -- initial scene
     -> IO ()
play width height title initialScene = do
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
                    ref   <- registScene window initialScene ()
                    mainLoop fps window time time initialScene ref

                    GLFW.destroyWindow window
                    GLFW.terminate
                    exitSuccess

registScene :: GLFW.Window -> Scene s i -> i -> IO (IORef s)
registScene window scene init = do
    state <- construct scene init
    ref   <- newIORef state
    GLFW.setKeyCallback window . Just $ \_ key _ keyState modifierKeys -> do
        state  <- readIORef ref
        state' <- flip execStateT state $ keyHandler scene key keyState modifierKeys
        writeIORef ref state'
    return ref

mainLoop :: Double      -- fps
         -> GLFW.Window -- OpenGL Window
         -> Double      -- last loop activated time
         -> Double      -- last display updated time
         -> Scene s i   -- scene at the loop
         -> IORef s     -- state
         -> IO ()
mainLoop fps window beforeLoop beforeDrawing scene ref = do
    shouldClose <- GLFW.windowShouldClose window
    when (not shouldClose) $ do
        after <- maybe 0 id <$> GLFW.getTime
        if (after - beforeDrawing) < 1/fps
            then mainLoop fps window after beforeDrawing scene ref
            else do
                (width, height) <- GLFW.getFramebufferSize window
                viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
                clear [ColorBuffer, DepthBuffer]

                matrixMode $= Projection
                loadIdentity
                lookAt (Vertex3 0 0 1) (Vertex3 0 0 0) (Vector3 0 1 0)

                let transit scene init = do
                        ref <- liftIO $ registScene window scene init
                        liftIO $ mainLoop fps window after after scene ref

                matrixMode $= Modelview 0
                state  <- readIORef ref
                state' <- flip execStateT state $ stepHandler scene transit (after - beforeLoop)
                drawHandler scene (drawInWindow width height) state'
                writeIORef ref state'

                GLFW.swapBuffers window
                GLFW.pollEvents
                mainLoop fps window after after scene ref
