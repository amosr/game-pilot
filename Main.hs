module Main where

import qualified Graphics.UI.GLFW   as GLFW
import Graphics.Rendering.OpenGL

import qualified GenerateTunnel     as GT
import qualified Tunnel             as T

import qualified State              as S
import qualified RenderState        as RS

import qualified AudioLoad          as ALoad
import qualified AudioPlay          as APlay

-- import qualified Render as R

-- everything from here starts with gl or GL
-- import Graphics.Rendering.OpenGL.Raw
-- import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Control.Monad ( forever )
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )

-- import Data.IORef


initGL :: GLFW.Window -> IO ()
initGL win = do
  depthFunc  $= Just Less
  shadeModel $= Smooth
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

  fogMode  $= Exp 0.01
  fogColor $= Color4 0.0 0.0 0.0 0.5

  GLFW.setTime 0


resizeScene :: GLFW.WindowSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  let w = fromIntegral width
      h = fromIntegral height
      hr= fromIntegral width / fromIntegral height

  viewport   $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  perspective 45 hr 0.1 500

  matrixMode $= Modelview 0
  loadIdentity
  -- translate (Vector3
  -- flush

drawScene :: S.State -> GLFW.Window -> IO ()
drawScene s win = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity

  S.update win s
  RS.draw s

  flush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: S.State -> GLFW.KeyCallback 
keyPressed _ win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
-- keyPressed s win GLFW.Key'Up     _ GLFW.KeyState'Pressed _ = modifyIORef (S._sSpeed s) (     (+) 1)
-- keyPressed s win GLFW.Key'Down   _ GLFW.KeyState'Pressed _ = modifyIORef (S._sSpeed s) (flip (-) 1)
keyPressed _ _   _               _ _                     _ = return ()

main :: IO ()
main = do
     True <- GLFW.init

     let segsPerSecond = 30

     [filename] <- getArgs

     Just music <- ALoad.load filename segsPerSecond
     putStrLn (show $ ALoad._aDuration music)
     putStrLn (show $ ALoad._aInfo music)

     audio <- APlay.init music


     GLFW.defaultWindowHints
     -- get a fullscreen window using the primary monitor
     monitor  <- GLFW.getPrimaryMonitor
     Just win <- GLFW.createWindow 1024 768 "game-pilot" monitor Nothing

     let tun = GT.mkTunnel music
     state    <- S.init win tun segsPerSecond

     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback win (Just $ drawScene state)
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just $ keyPressed state)
     -- register window close handler
     GLFW.setWindowCloseCallback win (Just shutdown)

     GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

     initGL win
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene state win
       GLFW.swapBuffers win
       APlay.update audio
