module FFT where

import qualified Data.Vector.Storable             as SV
import qualified Data.Vector.Storable.Mutable     as SVM

import qualified Tunnel                           as T
import qualified AudioLoad                        as A

import Control.Monad.ST

import Data.Complex
import Foreign.Storable.Complex ()

import Data.Int


-- | Length of input vector must be 2^n for some n. This is not checked.
fft :: SV.Vector Int16 -> SV.Vector (Complex Double)
fft vec
 = SV.create $ do
    let len = SV.length vec

    -- We need somewhere to store output, and a temporary buf
    out <- SVM.new len
    buf <- SVM.new len

    go_divide 0 len 1 out buf

    return out

 where
  go_divide s n step out buf

   | step >= n
   = do let val = fromIntegral (vec SV.! s) / 32767 :+ 0
        -- Fill both, probably not necessary
        SVM.write out s val
        SVM.write buf s val

   | otherwise
   = do -- Get subcomputations to write output to 'buf', with 'out' as buffer
        go_divide (s       )  n          (step * 2) buf out
        go_divide (s + step)  n          (step * 2) buf out
        -- Write from 'buf' to 'out', fixing with twiddles
        go_fix 0   s          n           step      out buf


  go_fix i s n step out buf
   | i >= n
   = return ()
   | otherwise
   = do buf1 <- SVM.read buf (s + i)
        buf2 <- SVM.read buf (s + i + step)
        let t = exp' i n * buf2

        SVM.write out (s + (i       `div` 2)) (buf1 + t)
        SVM.write out (s + ((i + n) `div` 2)) (buf1 - t)

        go_fix (i + step*2) s n step out buf


  exp' k n = cis $ -1 * pi * (fromIntegral k) / (fromIntegral n)


rfft :: SV.Vector Int16 -> SV.Vector Double
rfft vec
 = SV.map magnitude
 $ fft vec

