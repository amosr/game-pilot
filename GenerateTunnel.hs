module GenerateTunnel where

import qualified Data.Vector.Storable             as SV
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vect.Double                 as V
import qualified Data.Vect.Double.Util.Quaternion as Q

import qualified Tunnel                           as T
import qualified AudioLoad                        as A

import qualified FFT                              as FFT

import Data.Int

mkTunnel :: A.Audio -> T.Tunnel
mkTunnel audio
 = T.tunnel $ segs (0 :: Double) (A._aBuffs audio)
 where
  segs i []
   =     T.Segment Q.unitU (dataFor i 0) (widthFor i) (bgFor i) : segs (i+1) []
  segs i (s:ss)
   = let (peak, maxFreq, v1, v2, v3, v4) = analyse s
         n' = 1 / (((peak + v1) / 5) + 0.400)
     in  T.Segment (rotFor i v2 v3 v4 ((v2 + v3 + v4) / 60)) (dataFor i n') (widthFor i) (bgFor i) : segs (i+1) ss

  rotFor i nx ny nz n
   | nx == 0 && ny == 0 && nz == 0
   = Q.unitU
   | otherwise
   = Q.rotU (V.Vec3 nx ny nz) n

  dataFor i n
   = U.generate dataLength (dataGen n i)

  dataGen n i j
   = let j' = fromIntegral j :: Double
         dist = sin (sin (sin (i/400) * 3) * 3 + sin (j' / 7) * 2) * (sin (j' / 50) + cos (i / 50) + sin (i * j' / 30)) + 7 * n
     in   (max dist 2.0, (sin (i / 300) + sin (j' / 3), cos (i / 500) + sin (j' / 5), sin (i / 700 + 1) + sin (j' / 7)))

  widthFor i
   = (sin (sin (i / 900) * 200) * 0.49 + 0.51)

  bgFor i
   = (cos (i / 3000) * 0.3, sin (i / 5000) * 0.3, cos (i / 7000 + 1) * 0.3)

  dataLength = 100


analyse :: SV.Vector Int16 -> (Double,Double,Double,Double,Double,Double)
analyse s
   = let lenI = logBase 2 $ fromIntegral $ SV.length s :: Double
         len' = 2 ^ truncate lenI
         lenD = fromIntegral len' :: Double
         s'   = SV.unsafeTake len' s
         fft  = FFT.rfft s'

         mag v= SV.sum v / 500
         n    = mag fft

         peak = n / 5
         maxFreq = fromIntegral (SV.maxIndex fft) / lenD

         vS   = len' `div` 8
         v1   = mag $ SV.unsafeSlice (vS*0) vS fft
         v2   = mag $ SV.unsafeSlice (vS*1) vS fft
         v3   = mag $ SV.unsafeSlice (vS*2) vS fft
         v4   = mag $ SV.unsafeSlice (vS*3) vS fft
     in  (peak, maxFreq, v1, v2, v3, v4)
