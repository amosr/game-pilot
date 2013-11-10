module RenderSky where

import qualified Data.Vect.Double                   as V
import qualified Data.Vect.Double.Util.Quaternion   as Q
import qualified Data.Vect.Double.OpenGL            as VGL

import qualified Graphics.Rendering.OpenGL          as GL

{-# INLINE draw #-}
draw :: Double
     -> IO ()
draw time
 = do   GL.pointSize GL.$= 10
        GL.renderPrimitive GL.Points $ mapM_ draw_line [0..300]
 where
  draw_line i
   = do color (0.5, 0.5, 0.5)
        vert  i

  vert i
   = let i' = fromIntegral i
         x  = sin (time / 90  + sin (i' / 13)) * pi
         y  = sin (time / 130 + sin (i' / 27)) * pi
         z  = sin (time / 270 + sin (i' / 52)) * pi
         q  = Q.rotU V.vec3X x `Q.multU` Q.rotU V.vec3Y y `Q.multU` Q.rotU V.vec3Z z
     in  GL.vertex $ Q.actU q V.vec3Z

  color (r,g,b)
   = GL.color $ GL.Color3 (VGL.glflt r) (VGL.glflt g) (VGL.glflt b)


