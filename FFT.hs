module FFT
    ( fft
    , rfft )
    where
-- Perform fast Fourier transform over some real data

import qualified Data.Vector.Storable             as SV
import qualified Data.Vector.Storable.Mutable     as SVM

-- Complex numbers and storing them in vectors
import Data.Complex
import Foreign.Storable.Complex ()

import Data.Int


-- | Simple Cooley-Tukey fast Fourier transform
-- Input is real, but output is complex
-- Length of input vector must be 2^n for some n. This is not checked.
--
-- Allocates output and intermediate buffer of size n
--
-- Even though we want real output, we sadly need to calculate complexes before taking their magnitude.
--
fft :: SV.Vector Int16 -> SV.Vector (Complex Double)
fft vec
 = SV.create $ do
    -- We need somewhere to store output, and a temporary buf
    out <- SVM.new len
    buf <- SVM.new len

    -- Start dividing from 0 with stride 1
    go_divide 0 1 out buf

    return out

 where
  len = SV.length vec

  -- Recursively divide and process each half
  go_divide s step out buf
   -- If the step/stride is larger than the total input length,
   -- we've reached the bottom and there's only one in this chunk
   | step >= len
   = do -- Read from input vector and convert to complex
        let val = fromIntegral (vec SV.! s) / 32767 :+ 0
        -- Fill both, but probably only necessary to fill the out
        SVM.write out s val
        SVM.write buf s val

   | otherwise
   = do -- Get subcomputations to write output to 'buf', with 'out' as buffer
        -- Double the stride so each will process half the elements
        go_divide (s       )  (step * 2) buf out
        go_divide (s + step)  (step * 2) buf out
        -- Fix up the data in 'buf' and write it to 'out', fixing with twiddles
        go_fix 0   s           step      out buf

  -- Fix up after the divide is done
  go_fix i s step out buf
   -- Loop is done
   | i >= len
   = return ()

   | otherwise
   = do -- Read evens
        buf1 <- SVM.read buf (s + i)
        -- Read odd
        buf2 <- SVM.read buf (s + i + step)

        -- sin and cos for current element excluding offset(s)
        let t = exp' i len * buf2

        -- Write it out
        SVM.write out (s + (i         `div` 2)) (buf1 + t)
        SVM.write out (s + ((i + len) `div` 2)) (buf1 - t)

        -- Progress
        go_fix (i + step*2) s step out buf


  exp' k n = cis $ -1 * pi * (fromIntegral k) / (fromIntegral n)


-- | Real output of FFT.
-- Take magnitude of complex outputs.
rfft :: SV.Vector Int16 -> SV.Vector Double
rfft vec
 = SV.map magnitude
 $ fft vec

