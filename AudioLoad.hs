module AudioLoad where

import qualified Data.Vector.Storable             as SV

import qualified Sound.File.Sndfile               as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV

import           System.IO.Unsafe (unsafePerformIO)
import           Data.Int         (Int16)

data Audio
 = Audio
   { _aInfo     :: !Snd.Info
   , _aDuration :: !Double
   , _aBuffs    :: [SV.Vector Int16]
   -- bitrate etc
   }

load :: FilePath -> Int -> IO (Maybe Audio)
load file segsPerSec
 = do   hnd            <- Snd.openFile file Snd.ReadMode Snd.defaultInfo

        let info        = Snd.hInfo hnd
            dur         = Snd.duration info
            chunksize   = Snd.samplerate info `div` segsPerSec
            frames      = Snd.frames info
            buff        = mkBuff hnd chunksize frames 0

        return $ Just $ Audio info dur buff
 where
  mkBuff hnd chunksize end i
   | i >= end
   = []
   | otherwise
   = unsafePerformIO $ do
        Snd.hSeek hnd Snd.AbsoluteSeek i
        buf <- Snd.hGetBuffer hnd chunksize
        return $ case buf of
         Nothing
          -> []
         Just buf'
          -> SndV.fromBuffer buf' : mkBuff hnd chunksize end (i+chunksize)

