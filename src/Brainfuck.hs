{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
import Language.KansasLava
import Utils
import CPU
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.LogicStart
import Hardware.KansasLava.SevenSegment
import Data.Sized.Matrix
import Data.Sized.Unsigned as Unsigned

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

import System.FilePath
import System.Directory
import System.Environment (getArgs)
import Data.Char

indexList :: (Size n) => [a] -> Unsigned n -> Maybe a
indexList [] _ = Nothing
indexList (x:_) 0 = Just x
indexList (_:xs) n = indexList xs (n - 1)

ssI :: (sig ~ Signal c) => Matrix X7 (sig Bool)
ssI = matrix [  low,  low,  low,  low, high,  low,  low ]

ssO :: (sig ~ Signal c) => Matrix X7 (sig Bool)
ssO = matrix [  low,  low, high, high, high,  low, high ]

testBench :: (LogicStart fabric) => String -> fabric ()
testBench prog = do
    button <- do
        Buttons{..} <- buttons
        let (_, button, _) = debounce (Witness :: Witness X16) buttonLeft
        return button
    input <- toUnsigned `liftM` switches

    let toOpcode = fromIntegral . toInteger . ord . fromMaybe '\0' . indexList prog
        cpuIn = CPUIn{ cpuProgD = rom cpuProgA (Just . toOpcode)
                     , cpuButton = button
                     , cpuInput = input
                     }
        (dbg, CPUOut{..}) = cpu cpuIn

    let (outputE, outputD) = unpackEnabled cpuOutput

        digit = mux cpuNeedInput (outputD, input)
        (digitHi, digitLo) = both decode . splitByte $ digit
        digitE = outputE .||. cpuNeedInput

        i, o :: Seq (Matrix X7 Bool)
        i = pack ssI
        o = pack ssO

        io :: Matrix X7 (Seq Bool)
        io = unpack $ mux cpuNeedInput (o, i)

    let ssE = matrix [ low, digitE, digitE, digitE ]
        zero = matrix $ replicate 7 low
        ssD = matrix [ zero, io, digitHi, digitLo ]
    sseg $ driveSS ssE ssD

    let dbgFlags = [ cpuExec dbg
                   , cpuHalt dbg
                   , cpuWaitIn dbg
                   , cpuWaitOut dbg
                   , digitE
                   ]
    leds $ matrix $ reverse $ dbgFlags ++ replicate 3 low
  where
    decode :: (sig ~ Signal c) => sig (Unsigned X4) -> Matrix X7 (sig Bool)
    decode = unpack . funMap (Just . decodeHexSS)

helloWorld :: String
helloWorld = unlines
    [ ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]"
    , ">++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++"
    , ".------.--------.[-]>++++++++[<++++>- ]<+.[-]++++++++++."
    ]

emitBench :: String -> IO ()
emitBench prog = do
    kleg <- reifyFabric $ do
        board_init
        testBench prog

    createDirectoryIfMissing True outPath
    writeVhdlPrelude $ outVHDL "lava-prelude"
    writeVhdlCircuit modName (outVHDL modName) kleg
    writeUCF (outPath </> modName <.> "ucf") kleg
  where
    modName = "Brainfuck"
    outPath = ".." </> "ise" </> "src"
    outVHDL name = outPath </> name <.> "vhdl"

main :: IO ()
main = do
    args <- getArgs
    prog <- case args of
        [filename] -> readFile filename
        _ -> return helloWorld

    emitBench prog
