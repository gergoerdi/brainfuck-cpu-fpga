{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
import Language.KansasLava
import Utils
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.LogicStart
import Hardware.KansasLava.SevenSegment
import Data.Sized.Matrix
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
              | Exec
              | WaitIn
              | WaitOut
              deriving (Show, Eq, Enum, Bounded)

instance Rep CPUState where
    type W CPUState = X2
    newtype X CPUState = CPUStateX{ unCPUStateX :: Maybe CPUState }

    unX = unCPUStateX
    optX = CPUStateX
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe X4
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x4
      where
        x4 :: X X4
        x4 = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness X4)

cpu :: forall c sig. (Clock c, sig ~ Signal c)
    => (sig U8 -> sig U8)
    -> (sig Bool, sig U8)
    -> (CPUDebugInfo c, (sig Bool, sig (Enabled U8)))
cpu prog (button, input) = runRTL $ do
    pc <- newReg (0 :: U8)
    op <- newReg (0 :: U8)

    pointer <- newReg (0 :: Unsigned X8)
    let addr = coerce (fromIntegral . toInteger :: Unsigned X8 -> X256) (reg pointer)
    cellNew <- newReg (0 :: Unsigned X8)
    we <- newReg False
    let ram = writeMemory $ packEnabled (reg we) $ pack (addr, reg cellNew)
        cell = asyncRead ram addr

    s <- newReg Fetch
    let isState x = reg s .==. pureS x
        isFetch = isState Fetch
        isExec = isState Exec
        isWaitIn = isState WaitIn
        isWaitOut = isState WaitOut

    let isOp c = reg op .==. pureS (fromIntegral . ord $ c)
        isInc = isOp '+'
        isDec = isOp '-'
        isIncPtr = isOp '>'
        isDecPtr = isOp '<'
        isPrint = isOp '.'
        isRead = isOp ','
        isHalt = isOp '\0'

    let dbg = CPUDebugInfo{ cpuPC = reg pc
                          , cpuExec = isExec
                          , cpuWaitIn = isWaitIn
                          , cpuWaitOut = isWaitOut
                          }

    let next = do
            pc := reg pc + 1
            s := pureS Fetch
    CASE [ IF isFetch $ do
                we := low
                op := prog (reg pc)
                s := pureS Exec
         , IF isExec $ do
                CASE [ IF isInc $ do
                            we := high
                            cellNew := cell + 1
                            next
                     , IF isDec $ do
                            we := high
                            cellNew := cell - 1
                            next
                     , IF isIncPtr $ do
                            pointer := reg pointer + 1
                            next
                     , IF isDecPtr $ do
                            pointer := reg pointer - 1
                            next
                     , IF isPrint $ do
                            s := pureS WaitOut
                     , IF isRead $ do
                            s := pureS WaitIn
                     , IF isHalt $ return ()
                     , OTHERWISE next
                     ]
         , IF isWaitIn $ do
                WHEN button $ do
                    we := high
                    cellNew := input
                    next
         , IF isWaitOut $ do
                WHEN button next
         ]

    let requestInput = isWaitIn
        output = packEnabled isWaitOut cell
    return (dbg, (requestInput, output))

indexList :: (Size n) => [a] -> Unsigned n -> Maybe a
indexList [] _ = Nothing
indexList (x:_) 0 = Just x
indexList (x:xs) n = indexList xs (n - 1)

testBench :: (LogicStart fabric) => String -> fabric ()
testBench prog = do
    button <- do
        Buttons{..} <- buttons
        let (_, right, _) = debounce (Witness :: Witness X16) buttonRight
        return right
    input <- toUnsigned `liftM` switches

    let progROM = funMap (Just . fromIntegral . toInteger . ord . fromMaybe '\0' . indexList prog)

    let (dbg, (requestInput, output)) = cpu progROM (button, input)
        (outputE, outputD) = unpackEnabled output

        display = mux requestInput (outputD, input)
        (displayHi, displayLo) = both decode . splitByte $ display

        (pcHi, pcLo) = both decode . splitByte $ cpuPC dbg

    sseg $ driveSS_ $ matrix
      [ Just pcHi
      , Just pcLo
      , Just displayHi
      , Just displayLo
      ]
    leds $ matrix $ replicate 5 low ++ [ cpuExec dbg, cpuWaitIn dbg, cpuWaitOut dbg ]
  where
    decode :: (sig ~ Signal c) => sig (Unsigned X4) -> Matrix X7 (sig Bool)
    decode = unpack . funMap (Just . decodeHexSS)

main :: IO ()
main = do
    kleg <- reifyFabric $ do
        board_init
        testBench "++.++.>.+++.,++."

    createDirectoryIfMissing True outPath
    writeVhdlPrelude $ outVHDL "lava-prelude"
    writeVhdlCircuit modName (outVHDL modName) kleg
    writeUCF (outPath </> modName <.> "ucf") kleg
  where
    modName = "IOTest"
    outPath = ".." </> "ise" </> "src"
    outVHDL name = outPath </> name <.> "vhdl"
