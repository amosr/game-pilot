module GenerateTunnel where

import qualified Data.Vector.Storable             as SV
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vect.Double                 as V
import qualified Data.Vect.Double.Util.Quaternion as Q

import qualified Tunnel                           as T
import qualified AudioLoad                        as A

import Debug.Trace

import Data.Int

mkTunnel :: A.Audio -> T.Tunnel
mkTunnel audio
 = T.tunnel $ segs (0 :: Double) (A._aBuffs audio)
 where
  segs i []
   =     T.Segment (rotFor i) (dataFor i 0) (widthFor i) (bgFor i) : segs (i+1) []
  segs i (s:ss)
   = let n  = SV.maximum s
         m  = SV.minimum s
         n' = 1 / (fromIntegral (abs (n - m)) / 30000 + 0.500)
     in  T.Segment (rotFor i) (dataFor i n') (widthFor i) (bgFor i) : segs (i+1) ss

  rotFor i
   = Q.rotU (V.Vec3 (sin (i / 300)) (cos (i / 500)) (sin (i / 700 + 1))) 0.01

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

