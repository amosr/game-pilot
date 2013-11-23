module RenderTunnel where
-- Draw segments of tunnel

import qualified Data.Vector.Unboxed                as U
import qualified Data.Vect.Double                   as V
import qualified Data.Vect.Double.Util.Quaternion   as Q
import qualified Data.Vect.Double.OpenGL            as VGL

import qualified Graphics.Rendering.OpenGL          as GL

import           Tunnel

{-# INLINE draw #-}
draw :: Tunnel
     -> IO ()
draw tun
 = do   let segs = lodSegmentsOfTunnel tun
        GL.preservingMatrix $ do
            mapM_ draw_seg  $ segs `zip` tail segs
 where
  
  sines = _tSines tun

  draw_seg ((_, seg1), (dist, seg2))
   = do let q     = _sRot seg2
            dist' = fromIntegral dist
            width = _sWidth seg1
        draw_strips (_sVerts seg1) (_sVerts seg2) q dist' width
        VGL.multMatrix  $ Q.leftOrthoU q
        VGL.glTranslate $ V.Vec3 0 0 (-fromIntegral dist * tunnelSegmentSize)

  draw_strips as bs q dist width
   = GL.renderPrimitive GL.TriangleStrip $ do
        U.mapM_ (draw_verts q dist width) $ U.zip3 as bs sines
        -- Back to the start to close it off
        draw_verts q dist width (as U.! 0, bs U.! 0, sines U.! 0)

  draw_verts q dist width ((ad,ac),(bd,bc),(sine,cosine))
   = do color ac
        GL.vertex (V.Vec3 (sine*ad) (cosine*ad) 0)

        let bv  = V.Vec3 (sine*bd) (cosine*bd) (dist * width)
            bv' = Q.actU q bv
        color bc
        GL.vertex bv'

  color (r,g,b)
   = GL.color $ GL.Color3 (VGL.glflt r) (VGL.glflt g) (VGL.glflt b)

