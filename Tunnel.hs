module Tunnel where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vect.Double    as V
import qualified Data.Vect.Double.Util.Quaternion as Q

data Segment
 = Segment
 { _sRot        :: !Q.UnitQuaternion
   -- Note: all segments in tunnel must have same amount of data
 , _sVerts      :: !(U.Vector Data)
 , _sWidth      :: !Double
 , _sBgColour   :: !(Double,Double,Double)
 }

type Data = (Double, (Double,Double,Double))
{-
data Data
 = Data
 { _dist        :: Double
 , _colour      :: (Double,Double,Double)
 }
-}

data Tunnel
 = Tunnel
 { _tSegments   :: [Segment]
 , _tSines      :: !(U.Vector (Double,Double))
 }

lodSegmentsOfTunnel :: Tunnel -> [(Int,Segment)]
lodSegmentsOfTunnel tun
 = go 0 100 1 200 (_tSegments tun)
 where
  go n till skip end segs
   | n >= end
   = []

  go n till skip end segs
   | n >= till
   = go n (till*2) (skip*2) end segs

  go n till skip end (s:ss)
   = let rest = go (n+1) till skip end ss
     in  if   n `mod` skip == 0
         then (skip, s) : rest
         else             rest

  go _ till skip end []
   = []
{-
 = take 200
 $ zip (cycle [1]) (_tSegments tunnel)
 -}

defaultTunnel :: Tunnel
defaultTunnel
 = tunnel $ segs (0 :: Double)
 where
  segs i
   = Segment (rotFor i) (dataFor i) (widthFor i) (bgFor i) : segs (i+1)

  rotFor i
   = Q.rotU (V.Vec3 (sin (i / 300)) (cos (i / 500)) (sin (i / 700 + 1))) 0.01

  dataFor i
   = U.generate dataLength (dataGen i)

  dataGen i j
   = let j' = fromIntegral j :: Double
     in   (sin (sin (sin (i/400) * 3) * 3 + sin (j' / 7) * 2) * (sin (j' / 50) + cos (i / 50) + sin (i * j' / 30)) + 7, (sin (i / 300) + sin (j' / 3), cos (i / 500) + sin (j' / 5), sin (i / 700 + 1) + sin (j' / 7)))

  widthFor i
   = (sin (sin (i / 900) * 200) * 0.49 + 0.51)

  bgFor i
   = (cos (i / 3000) * 0.3, sin (i / 5000) * 0.3, cos (i / 7000 + 1) * 0.3)

  dataLength = 100

tunnel :: [Segment] -> Tunnel
tunnel ss@(s:_)
 = Tunnel
   { _tSegments = ss
   , _tSines    = U.generate len gen }
 where
  gen i
   = let i'   = fromIntegral i   :: Double
         len' = fromIntegral len :: Double
     in  (sin (i' / len' * 2 * pi), cos (i' / len' * 2 * pi))

  len
   = U.length $ _sVerts s

tunnel [] = error "Can't make an empty tunnel!"


peel :: Tunnel -> (Segment, Tunnel)
peel tun
 = case _tSegments tun of
    []   -> error "Can't peel an empty tunnel"
    s:ss -> ( s
            , tun { _tSegments = ss })


collideSphere
    :: Tunnel
    -> V.Vec3   -- ^ position of sphere
    -> Double   -- ^ radius
    -> Maybe V.Vec3

-- TODO hacky
collideSphere tun org rad
 = go org rad V.zero $ _tSegments tun
 where
  go org rad fup [] = Nothing
  go org rad fup (s:ss)
   | V._3 org < 0
   = Nothing
   | V._3 org < tunnelSegmentSize
      -- This bit's hacky, but should work if terrain is smooth enough and relatively 'open'
      -- 1. Find angle from centre of segment to sphere
      -- 2. Use angle to look up segment's data
      -- 3. Check if sphere collides with segment at that point
      --
      -- Should also interpolate between the data

      -- Vector with Z zero'd out
   = let orgZ  = V.Vec3 0 0 (V._3 org)
         orgZ0 = org V.&- orgZ
         orgZ0N= V.normalize orgZ0
         (x,y) = (V._1 orgZ0N, V._2 orgZ0N)
      -- Org's distance to centre
         orgD  = V.norm orgZ0

      -- Angle between the centre of segment
         ang   | x <= 0    = acos y
               | otherwise = acos y + pi
      -- Convert to an index
         verts = _sVerts s
         nverts= U.length verts
         idx   | isNaN ang
               = 0
               | otherwise
               = truncate $ ang * fromIntegral nverts / (2 * pi)
      -- Look up vertex's distance from centre
         c     = fst $ verts U.! idx

     in  if   orgD > (c - rad)
         then Just (orgZ0N V.&* (c - rad) V.&+ orgZ V.&+ fup)
         else Nothing
   | otherwise
   = let trans = Q.actU (_sRot s) tunnelSegmentSizeV
         fup'  = fup V.&+ trans
         org'  = org  V.&- trans
     in  go org' rad fup' ss


tunnelSegmentSize :: Double
tunnelSegmentSize = 3

tunnelSegmentSizeV :: V.Vec3
tunnelSegmentSizeV = V.Vec3 0 0 tunnelSegmentSize


bgColour :: Tunnel -> (Double,Double,Double)
bgColour tun 
 = case _tSegments tun of
    []    -> (0,0,0)
    (s:_) -> _sBgColour s
