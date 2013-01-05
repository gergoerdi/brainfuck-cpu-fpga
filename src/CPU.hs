{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module CPU
       ( CPUOut(..)
       , CPUIn(..)
       , CPUDebug(..)
       , cpu
       ) where

import Language.KansasLava
import Utils
import Data.Sized.Matrix
import Data.Sized.Arith (X0_, X1_)
import Data.Sized.Unsigned as Unsigned
import Data.Char

data CPUIn c = CPUIn{ cpuProgD :: Signal c U8
                    , cpuButton :: Signal c Bool
                    , cpuInput :: Signal c U8
                    }

data CPUOut c = CPUOut{ cpuProgA :: Signal c U8
                      , cpuNeedInput :: Signal c Bool
                      , cpuOutput :: Signal c (Enabled U8)
                      }

data CPUDebug c = CPUDebug{ cpuPC :: Signal c U8
                          , cpuExec :: Signal c Bool
                          , cpuHalt :: Signal c Bool
                          , cpuWaitIn :: Signal c Bool
                          , cpuWaitOut :: Signal c Bool
                          }

data CPUState = Start
              | Fetch
              | WaitRAM
              | Exec
              | SkipFwd
              | Rewind
              | WaitIn
              | WaitOut
              | Halt
              deriving (Show, Eq, Enum, Bounded)

instance Rep CPUState where
    type W CPUState = X4
    newtype X CPUState = XCPUState{ unXCPUState :: Maybe CPUState }

    unX = unXCPUState
    optX = XCPUState
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe X9
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x
      where
        x :: X X9
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness X9)

type X32768 = X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (
              X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))))))))))

cpu :: forall c sig. (Clock c, sig ~ Signal c)
    => CPUIn c -> (CPUDebug c, CPUOut c)
cpu CPUIn{..} = runRTL $ do
    -- Program counter
    pc <- newReg (0 :: U8)

    -- The opcode currently getting executed
    op <- newReg (0 :: U8)

    -- Depth counter, for skip-ahead/rewind
    dc <- newReg (0 :: U8)

    -- Pointer to RAM
    pointer <- newReg (0 :: Unsigned X15)
    let addr = coerce toAddr (reg pointer)
          where
            toAddr :: Unsigned X15 -> X32768
            toAddr = fromIntegral . toInteger

    -- New value to write into RAM[addr]
    cellNew <- newReg (0 :: Unsigned X8)

    -- Write Enabled
    we <- newReg False

    -- RAM
    let ram = writeMemory $ packEnabled (reg we) $ pack (addr, reg cellNew)
        cell = syncRead ram addr

    -- CPU state
    s <- newReg Start
    let isState x = reg s .==. pureS x

    let dbg = CPUDebug{ cpuPC = reg pc
                      , cpuExec = isState Exec
                      , cpuHalt = isState Halt
                      , cpuWaitIn = isState WaitIn
                      , cpuWaitOut = isState WaitOut
                      }

        out = CPUOut{ cpuProgA = var pc
                    , cpuNeedInput = isState WaitIn
                    , cpuOutput = packEnabled (isState WaitOut) cell
                    }
    let next = do
            pc := reg pc + 1
            s := pureS Fetch

    switch (reg s)
      [ Start ==> do
             s := pureS Fetch
      , Fetch ==> do
             we := low
             op := cpuProgD
             s := pureS WaitRAM
      , WaitRAM ==> do
             s := pureS Exec
      , Exec ==> switch (reg op)
          [ ch '+' ==> do
                 we := high
                 cellNew := cell + 1
                 next
          , ch '-' ==> do
                 we := high
                 cellNew := cell - 1
                 next
          , ch '>' ==> do
                 pointer := reg pointer + 1
                 next
          , ch '<' ==> do
                 pointer := reg pointer - 1
                 next
          , ch '[' ==> do
                 CASE [ IF (cell .==. pureS 0) $ do
                             dc := pureS 0
                             s := pureS SkipFwd
                      , OTHERWISE next
                      ]
          , ch ']' ==> do
                 dc := pureS 0
                 -- TODO: check if we could exit the loop
                 s := pureS Rewind
          , ch '.' ==> do
                 s := pureS WaitOut
          , ch ',' ==> do
                 s := pureS WaitIn
          , ch '\0' ==> do
                 s := pureS Halt
          , oTHERWISE next
          ]
      , WaitIn ==> do
             WHEN cpuButton $ do
                 we := high
                 cellNew := cpuInput
                 next
      , WaitOut ==> WHEN cpuButton next
      , Halt ==> do
          s := pureS Halt
      ]

    return (dbg, out)
  where
    ch = fromIntegral . ord :: Char -> U8
