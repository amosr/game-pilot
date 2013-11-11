module State where

import qualified Tunnel                             as T
import qualified Data.Vect.Double                   as V
import qualified Data.Vect.Double.Util.Quaternion   as Q

import qualified Graphics.UI.GLFW                   as GLFW

import Data.IORef
import Control.Monad


data State
 = State
 { _sTunnel     :: IORef T.Tunnel
 , _sFrames     :: IORef Int
 , _sTime       :: IORef Double
 , _sFrameTime  :: IORef Double
 , _sOrg        :: IORef V.Vec3
 , _sRot        :: IORef Q.UnitQuaternion
 , _sCursorPos  :: IORef (Double, Double)
 , _sSegmentsPerSec :: Double
 }

init :: GLFW.Window -> T.Tunnel -> Int -> IO State
init win tun segsPerSecond
 = do   tun'    <- newIORef   tun
        frames' <- newIORef   0
        time'   <- newIORef   0
        frametime' <- newIORef   0
        org'    <- newIORef $ V.Vec3 0 0 0
        rot'    <- newIORef   Q.unitU

        pos     <- GLFW.getCursorPos win
        pos'    <- newIORef   pos

        return State
               { _sTunnel    = tun'
               , _sFrames    = frames'
               , _sTime      = time'
               , _sFrameTime = frametime'
               , _sOrg       = org'
               , _sRot       = rot'
               , _sCursorPos = pos'
               , _sSegmentsPerSec = fromIntegral segsPerSecond
               }


update :: GLFW.Window -> State -> IO ()
update win s
 = do   modifyIORef (_sFrames s) (+1)
        frames <- readIORef $ _sFrames s

        oldtime<- readIORef $ _sTime s
        time'  <- GLFW.getTime
        case time' of
         Just t  -> do
            writeIORef (_sTime s) t
            writeIORef (_sFrameTime s) (t - oldtime)
         Nothing -> return ()

        frametime <- readIORef $ _sFrameTime s

        oldpos <- readIORef $ _sCursorPos s
        newpos <- GLFW.getCursorPos win

        writeIORef (_sCursorPos s) newpos

        rot <- readIORef $ _sRot s

        let rot' = rotate oldpos newpos rot

        writeIORef (_sRot s) rot'

        let speed = frametime * _sSegmentsPerSec s * T.tunnelSegmentSize

        -- Check if we can peel a segment off the tunnel
        tun <- readIORef $ _sTunnel s

        let motion  = Q.actU rot' (V.Vec3 0 0 speed)
        -- Hack motion - we actually always want to go forward at 'speed', to keep with music
        let motion' = V.mkVec3 (V._1 motion, V._2 motion, speed)

        org <- readIORef $ _sOrg s
        let org' = org V.&+ motion'
        let org'' = maybe org' id (T.collideSphere tun org' 2)
        writeIORef (_sOrg    s) org''

        -- duh, this is just getting Z
        let proj    = org'' V.&. V.Vec3 0 0 1
        -- putStrLn $ show (org', proj)

        when (proj > T.tunnelSegmentSize * 2) $ do -- (frames `mod` 11 == 0) $ do
            let (seg, tun') = T.peel tun
            writeIORef (_sTunnel s) tun'

            let rot'' = Q.normalizeU $ Q.multU (T._sRot seg) rot'
            writeIORef (_sRot s) rot''
            writeIORef (_sOrg s) (org'' V.&- Q.actU (T._sRot seg) T.tunnelSegmentSizeV)

 where
  rotate (ox,oy) (nx,ny) q
   = let xq = Q.rotU (V.Vec3 0 1 0) ((ox - nx) / 200)
         yq = Q.rotU (V.Vec3 1 0 0) ((oy - ny) / 200)
     in q `Q.multU` xq `Q.multU` yq

