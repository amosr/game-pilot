module AudioPlay where

import qualified Data.Vector.Storable             as SV
import qualified Sound.OpenAL                     as AL

import qualified AudioLoad                        as A

import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef

import           Data.Int         (Int16)


data AudioState
 = AudioState
 { _asSource    :: AL.Source
 , _asData      :: IORef [SV.Vector Int16]
 , _asProcessed :: IORef Int
 , _asBuffs     :: IORef [AL.Buffer]
 }

init :: A.Audio -> IO AudioState
init audio
 = do   Just device          <- AL.openDevice    Nothing
        Just context         <- AL.createContext device []
        AL.currentContext AL.$= Just context
        [src]                <- AL.genObjectNames 1

        let numBuffers        = 100
        buffs                <- AL.genObjectNames numBuffers

        let datas             = A._aBuffs audio
        -- Fill the first ten or so
        let (now,rest)        = splitAt numBuffers datas

        mapM_ fillBuffer (buffs `zip` now)

        AL.queueBuffers src buffs
        AL.play [src]

        rest'                <- newIORef rest
        proc'                <- newIORef 0
        buffs'               <- newIORef $ cycle buffs

        return $ AudioState
               { _asSource      = src
               , _asData        = rest'
               , _asProcessed   = proc'
               , _asBuffs       = buffs'
               }


fillBuffer :: (AL.Buffer, SV.Vector Int16) -> IO ()
fillBuffer (buf, dat)
 = do   ptr <- SV.unsafeWith dat (return . id)

        let rgn     = AL.MemoryRegion ptr (fromIntegral $ SV.length dat * 2)
            bufdata = AL.BufferData   rgn AL.Stereo16 44100
        
        AL.bufferData buf AL.$= bufdata

        return ()


update :: AudioState -> IO ()
update state
 = do   let src   = _asSource state
        oldproc  <- readIORef $ _asProcessed state
        newproc  <- AL.get $ AL.buffersProcessed src
        let newproc' = fromIntegral newproc

        writeIORef (_asProcessed state) newproc'

        buffs <- readIORef $ _asBuffs state
        datas <- readIORef $ _asData state

        let enqueue      = newproc' -- - oldproc
            (now,rest)   = splitAt enqueue (datas `zip` buffs)
            (nowD,nowB)  = unzip now
            (restD,restB)= unzip rest

        putStrLn $ "enqueuing more: " ++ show enqueue

        AL.unqueueBuffers src nowB

        mapM_ fillBuffer (nowB `zip` nowD)

        AL.queueBuffers src nowB
        -- AL.play [src]

        writeIORef (_asData  state) restD
        writeIORef (_asBuffs state) restB


