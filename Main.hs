module Main where
-- Glue everything together

import qualified Graphics.UI.GLFW   as GLFW
import Graphics.Rendering.OpenGL

import qualified GenerateTunnel     as GT

import qualified State              as S
import qualified RenderState        as RS

import qualified AudioLoad          as ALoad
import qualified AudioPlay          as APlay

import Control.Applicative
import Control.Monad ( forever )
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )


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
keyPressed _ _   _               _ _                     _ = return ()

data Args
 = Args
 { _aFilename   :: String
 , _aFullscreen :: Bool
 , _aMonitor    :: Maybe Int
 , _aMonitorShow:: Bool
 }
argsOfFn :: String -> Args
argsOfFn fn
 = Args
 { _aFilename       = fn
 , _aFullscreen     = False
 , _aMonitor        = Nothing
 , _aMonitorShow    = False
 }

parseArgs :: [String] -> Maybe Args
parseArgs []
 = Nothing
parseArgs [fn]
 = Just $ argsOfFn fn
parseArgs ("-f":xs)
 | Just a <- parseArgs xs
 = Just $ a { _aFullscreen = True }

-- Hack: '-m show' doesn't require filename etc because it won't use it
parseArgs ("-m":"show":_)
 = Just $ (argsOfFn "") { _aMonitorShow = True }

parseArgs ("-m":num:xs)
 | Just a <- parseArgs xs
 = Just $ a { _aMonitor = Just $ read num }

parseArgs _
 = Nothing

usage :: String
usage
 = unlines
 [ "game-pilot:"
 , "silly little 'game' that lets you fly through music"
 , "usage:"
 , "  game-pilot [-f] [-m (num|show)] filename"
 , "where"
 , "-f       : fullscreen"
 , "-m num   : select monitor number for fullscreen (implies -f)"
 , "-m show  : show list of available monitors (will not run)"
 , "filename : flac, ogg, or whatever to view. mp3 is not supported."
 ]

main :: IO ()
main = do
     True <- GLFW.init

     args <- getArgs
     -- TODO print usage
     case parseArgs args of
      Nothing
       -> putStr usage
      Just (Args { _aMonitorShow = True })
       -> showMonitorList
      Just args'
       -> play args'

showMonitorList :: IO ()
showMonitorList
 = do   mons <- GLFW.getMonitors
        case mons of
         Nothing
          ->    putStrLn "No monitors found..."
         Just ms
          -> do putStrLn "Monitors:"
                mapM_ showMon (ms `zip` [0::Int ..])
 where
  showMon (m,i)
   = do name <- GLFW.getMonitorName m
        putStrLn $ concat [show i, ": ", maybe "<unknown name>" id name]

play :: Args -> IO ()
play args
 = do
     let segsPerSecond = 30
         filename = _aFilename args

     Just music <- ALoad.load filename segsPerSecond
     putStrLn (show $ ALoad._aDuration music)
     putStrLn (show $ ALoad._aInfo music)

     audio <- APlay.initialise music


     GLFW.defaultWindowHints
     -- get a fullscreen window using the primary monitor
     monitor  <- case (_aFullscreen args, _aMonitor args) of
                  (True, Nothing)
                   -> GLFW.getPrimaryMonitor
                  (_,    Just i)
                   -> (fmap (!!i)) <$> GLFW.getMonitors
                  _
                   -> return Nothing
     Just win <- GLFW.createWindow 1024 768 "game-pilot" monitor Nothing

     let tun = GT.mkTunnel music
     state    <- S.initialise win tun segsPerSecond

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
