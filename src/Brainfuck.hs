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
import Data.Char

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

    let toOpcode = fromIntegral . toInteger . ord . fromMaybe '\0' . indexList prog
        cpuIn = CPUIn{ cpuProgD = rom cpuProgA (Just . toOpcode)
                     , cpuButton = button
                     , cpuInput = input
                     }
        (dbg, CPUOut{..}) = cpu cpuIn

    let (outputE, outputD) = unpackEnabled cpuOutput

        display = mux cpuNeedInput (outputD, input)
        (displayHi, displayLo) = both decode . splitByte $ display
        displayE = outputE .||. cpuNeedInput

        (pcHi, pcLo) = both decode . splitByte $ cpuPC dbg

    let ssE = matrix [ high, high, displayE, displayE ]
        ssD = matrix [ pcHi, pcLo, displayHi, displayLo ]
    sseg $ driveSS ssE ssD

    let dbgFlags = [ cpuExec dbg
                   , cpuHalt dbg
                   , cpuWaitIn dbg
                   , cpuWaitOut dbg
                   , displayE
                   ]
    leds $ matrix $ reverse $ dbgFlags ++ replicate 3 low
  where
    decode :: (sig ~ Signal c) => sig (Unsigned X4) -> Matrix X7 (sig Bool)
    decode = unpack . funMap (Just . decodeHexSS)

main :: IO ()
main = do
    kleg <- reifyFabric $ do
        board_init
        testBench "++++[->.++<]"
        --         0123456789abcdef0

    createDirectoryIfMissing True outPath
    writeVhdlPrelude $ outVHDL "lava-prelude"
    writeVhdlCircuit modName (outVHDL modName) kleg
    writeUCF (outPath </> modName <.> "ucf") kleg
  where
    modName = "Brainfuck"
    outPath = ".." </> "ise" </> "src"
    outVHDL name = outPath </> name <.> "vhdl"
