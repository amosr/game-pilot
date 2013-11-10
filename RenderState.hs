module RenderState where

import qualified Data.Vector.Unboxed                as U
import qualified Data.Vect.Double                   as V
import qualified Data.Vect.Double.Util.Quaternion   as Q
import qualified Data.Vect.Double.OpenGL            as VGL

import qualified Graphics.Rendering.OpenGL          as GL
import qualified Graphics.Rendering.OpenGL.Raw      as GLR

import qualified Tunnel                             as T
import qualified RenderTunnel                       as RT
import qualified State                              as S
import qualified RenderSky                          as RSky

import           Data.IORef

{-# INLINE draw #-}
draw :: S.State
     -> IO ()
draw s
 = do   org <- readIORef $ S._sOrg s
        rot <- readIORef $ S._sRot s
        time<- readIORef $ S._sTime s

        VGL.multMatrix  $ Q.leftOrthoU rot


        tun    <- readIORef $ S._sTunnel s
        let (r,g,b) = T.bgColour tun
        let col     = GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) 0.5

        GL.clearColor GL.$= col
        GL.clear [GL.ColorBuffer]
        -- Draw sky box
        RSky.draw time
        
        -- Clear depth buffer, as sky box should be behind everything
        GL.clear [GL.DepthBuffer]

        GL.fogColor   GL.$= col
        GLR.glEnable GLR.gl_FOG


        VGL.glTranslate   org

        RT.draw tun

        GLR.glDisable GLR.gl_FOG


