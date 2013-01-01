{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CPU
       ( CPUDebugInfo(..)
       , cpu
       ) where

import Language.KansasLava
import Utils
import Data.Sized.Matrix
import Data.Sized.Arith (X0_, X1_)
import Data.Sized.Unsigned as Unsigned
import Data.Char

data CPUDebugInfo c = CPUDebugInfo{ cpuPC :: Signal c U8
                                  , cpuExec :: Signal c Bool
                                  , cpuWaitIn :: Signal c Bool
                                  , cpuWaitOut :: Signal c Bool
                                  }

data CPUState = Fetch
              | WaitRAM
              | Exec
              | WaitIn
              | WaitOut
              deriving (Show, Eq, Enum, Bounded)

instance Rep CPUState where
    type W CPUState = X3
    newtype X CPUState = XCPUState{ unXCPUState :: Maybe CPUState }

    unX = unXCPUState
    optX = XCPUState
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe X5
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x4
      where
        x4 :: X X5
        x4 = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness X5)

type X32768 = X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (
              X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))))))))))

cpu :: forall c sig. (Clock c, sig ~ Signal c)
    => (sig U8 -> sig U8)
    -> (sig Bool, sig U8)
    -> (CPUDebugInfo c, (sig Bool, sig (Enabled U8)))
cpu progROM (button, input) = runRTL $ do
    -- Program counter
    pc <- newReg (0 :: U8)

    -- The opcode currently getting executed
    op <- newReg (0 :: U8)


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
    s <- newReg Fetch

    let dbg = CPUDebugInfo{ cpuPC = reg pc
                          , cpuExec = isState Exec
                          , cpuWaitIn = isState WaitIn
                          , cpuWaitOut = isState WaitOut
                          }
          where
            isState x = reg s .==. pureS x

    let next = do
            pc := reg pc + 1
            s := pureS Fetch

    switch s
      [ Fetch ==> do
             we := low
             op := progROM (reg pc)
             s := pureS WaitRAM
      , WaitRAM ==> do
             s := pureS Exec
      , Exec ==> switch op
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
          , ch '.' ==> do
                 s := pureS WaitOut
          , ch ',' ==> do
                 s := pureS WaitIn
          , ch '\0' ==> return ()
          , oTHERWISE next
          ]
      , WaitIn ==> do
             WHEN button $ do
                 we := high
                 cellNew := input
                 next
      , WaitOut ==> WHEN button next
      ]

    let requestInput = reg s .==. pureS WaitIn
        outputReady = reg s .==. pureS WaitOut
        output = packEnabled outputReady cell
    return (dbg, (requestInput, output))
  where
    ch = fromIntegral . ord :: Char -> U8
