module AudioLoad
    ( Audio(..)
    , load )
    where
-- Load audio from a file
--
-- Uses libsndfile to decode flac and ogg
-- mp3 not supported, sadly.
--
-- Uses unsafePerformIO to return a lazy list of chunks

import qualified Data.Vector.Storable             as SV

import qualified Sound.File.Sndfile               as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV

import           System.IO.Unsafe (unsafePerformIO)
import           Data.Int         (Int16)

data Audio
 = Audio
   -- | Information such as sample rate, duration
   { _aInfo     :: !Snd.Info
   -- | Duration in seconds
   , _aDuration :: !Double
   -- | Lazy list of chunks
   , _aBuffs    :: [SV.Vector Int16]
   }

-- | load audio from a file
-- Splits audio into @segsPerSec@ chunks
--
-- TODO catch exceptions and return Nothing
load :: FilePath -> Int -> IO (Maybe Audio)
load file segsPerSec
 = do   hnd            <- Snd.openFile file Snd.ReadMode Snd.defaultInfo

        let info        = Snd.hInfo hnd
            dur         = Snd.duration info
            chunksize   = Snd.samplerate info `div` segsPerSec
            frames      = Snd.frames info
            -- Start reading from file
            buff        = mkBuff hnd chunksize frames 0

        return $ Just $ Audio info dur buff
 where
  mkBuff hnd chunksize end i
   | i >= end
   = []
   | otherwise
   = unsafePerformIO $ do
        -- Seek to the exact point we need.
        -- This means we can safely unsafe, as it becomes order independent
        _   <- Snd.hSeek hnd Snd.AbsoluteSeek i
        -- Attempt to read the buffer from the file
        buf <- Snd.hGetBuffer hnd chunksize
        return $ case buf of
         -- Nothing means we've reached the end. Unexpectedly early, but no matter.
         Nothing
          -> []
         -- Convert buf to vector and return a lazy thunk for the rest of the audio
         Just buf'
          -> SndV.fromBuffer buf' : mkBuff hnd chunksize end (i+chunksize)

