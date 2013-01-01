{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
import Language.KansasLava
import Utils
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.LogicStart
import Hardware.KansasLava.SevenSegment
import Data.Sized.Matrix
import Data.Sized.Arith (X0_, X1_)
import Data.Sized.Unsigned as Unsigned

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

import System.FilePath
import System.Directory
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
    newtype X CPUState = CPUStateX{ unCPUStateX :: Maybe CPUState }

    unX = unCPUStateX
    optX = CPUStateX
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
    pc <- newReg (0 :: U8)
    op <- newReg (0 :: U8)

    pointer <- newReg (0 :: Unsigned X15)
    let addr = coerce toAddr (reg pointer)
          where
            toAddr :: Unsigned X15 -> X32768
            toAddr = fromIntegral . toInteger

    cellNew <- newReg (0 :: Unsigned X8)
    we <- newReg False
    let ram = writeMemory $ packEnabled (reg we) $ pack (addr, reg cellNew)
        cell = syncRead ram addr

    s <- newReg Fetch
    let ch = fromIntegral . ord :: Char -> U8

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

indexList :: (Size n) => [a] -> Unsigned n -> Maybe a
indexList [] _ = Nothing
indexList (x:_) 0 = Just x
indexList (_:xs) n = indexList xs (n - 1)

testBench :: (LogicStart fabric) => String -> fabric ()
testBench prog = do
    button <- do
        Buttons{..} <- buttons
        let (_, button, _) = debounce (Witness :: Witness X16) buttonLeft
        return button
    input <- toUnsigned `liftM` switches

    let progROM = funMap (Just . fromIntegral . toInteger . ord . fromMaybe '\0' . indexList prog)

    let (dbg, (requestInput, output)) = cpu progROM (button, input)
        (outputE, outputD) = unpackEnabled output

        display = mux requestInput (outputD, input)
        (displayHi, displayLo) = both decode . splitByte $ display
        displayE = outputE .||. requestInput

        (pcHi, pcLo) = both decode . splitByte $ cpuPC dbg

    let ssE = matrix [ high, high, displayE, displayE ]
        ssD = matrix [ pcHi, pcLo, displayHi, displayLo ]
    sseg $ driveSS ssE ssD
    leds $ matrix $ replicate 5 low ++ [ cpuExec dbg, cpuWaitIn dbg, cpuWaitOut dbg ]
  where
    decode :: (sig ~ Signal c) => sig (Unsigned X4) -> Matrix X7 (sig Bool)
    decode = unpack . funMap (Just . decodeHexSS)

main :: IO ()
main = do
    kleg <- reifyFabric $ do
        board_init
        testBench "+++.++.>.+++.,++."
        --         0123456789abcdef0

    createDirectoryIfMissing True outPath
    writeVhdlPrelude $ outVHDL "lava-prelude"
    writeVhdlCircuit modName (outVHDL modName) kleg
    writeUCF (outPath </> modName <.> "ucf") kleg
  where
    modName = "IOTest"
    outPath = ".." </> "ise" </> "src"
    outVHDL name = outPath </> name <.> "vhdl"
