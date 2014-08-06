module AudioPlay
    ( initialise
    , update 
    , AudioState )
    where
-- Use OpenAL to stream and play some music

import qualified Data.Vector.Storable             as SV
import qualified Sound.OpenAL                     as AL

-- Just for the Audio datatype
import qualified AudioLoad                        as A

import           Data.IORef

import           Data.Int         (Int16)


-- | Keep track of the state of the currently playing music
data AudioState
 = AudioState
 -- | The OpenAL "sound source" we're playing to
 { _asSource    :: AL.Source
 -- | Audio data
 , _asData      :: IORef [SV.Vector Int16]
 -- | Infinite list of OpenAL buffers to queue up with audio data
 , _asBuffs     :: IORef [AL.Buffer]
 -- Actually, these two might be nicer as
 --     IORef [(SV.Vector Int16, AL.Buffer)]
 }


-- | Initialise OpenAL and start playing given stream
-- Returned AudioState must then be periodically called with 'update' to keep filling queue
initialise :: A.Audio -> IO AudioState
initialise audio
 = do   -- Start OpenAL
        Just device          <- AL.openDevice    Nothing
        Just context         <- AL.createContext device []
        AL.currentContext AL.$= Just context

        -- Generate source
        [src]                <- AL.genObjectNames 1

        -- Generate a bunch of buffers.
        -- Since 'update' is called every frame we don't really need this many
        let numBuffers        = 100
        buffs                <- AL.genObjectNames numBuffers

        let datas             = A._aBuffs audio
        -- Get the data to initially fill the buffers
        let (now,rest)        = splitAt numBuffers datas

        -- Fill them with data
        mapM_ fillBuffer (buffs `zip` now)

        -- Enqueue them and start playing
        AL.queueBuffers src buffs
        AL.play [src]

        rest'                <- newIORef rest
        buffs'               <- newIORef $ cycle buffs

        return $ AudioState
               { _asSource      = src
               , _asData        = rest'
               , _asBuffs       = buffs'
               }


-- | Keep streaming, enqueue new data as buffers are finished
update :: AudioState -> IO ()
update state
 = do   let src          = _asSource state

        buffs           <- readIORef $ _asBuffs state
        datas           <- readIORef $ _asData state

        -- Find out how many buffers have been played
        processed       <- AL.get $ AL.buffersProcessed src
        let enqueue      = fromIntegral processed
            -- Get buffers and audio data to enqueue
            (now,rest)   = splitAt enqueue (buffs `zip` datas)
            (nowB, _)    = unzip now
            (restB,restD)= unzip rest

        -- Remove from queue, then fill them with new data and re-add them
        _ <- AL.unqueueBuffers src processed
        mapM_ fillBuffer now
        AL.queueBuffers src nowB

        -- Update state
        writeIORef (_asBuffs state) restB
        writeIORef (_asData  state) restD


-- | Fill given buffer with audio data
fillBuffer :: (AL.Buffer, SV.Vector Int16) -> IO ()
fillBuffer (buf, dat)
 = do   -- Get the pointer of the vector's data
        -- This is unsafe because it might be GC'd.
        -- I'm going to do it anyway.
        ptr <- SV.unsafeWith dat (return . id)

        -- TODO hardcoded audio format
        let rgn     = AL.MemoryRegion ptr (fromIntegral $ SV.length dat * 2)
            bufdata = AL.BufferData   rgn AL.Stereo16 44100
        
        AL.bufferData buf AL.$= bufdata

        return ()

