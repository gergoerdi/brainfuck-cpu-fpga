{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
import Language.KansasLava
import Utils
import CPU
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.LogicStart
import Hardware.KansasLava.SevenSegment
import Data.Sized.Matrix as Matrix
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

ssI :: Matrix X7 Bool
ssI = matrix [ False, False, False, False,  True, False, False ]

ssO :: Matrix X7 Bool
ssO = matrix [ False, False,  True,  True,  True, False,  True ]

ssH :: Matrix X7 Bool
ssH = matrix [ False, False,  True, False,  True,  True,  True ]

ssA :: Matrix X7 Bool
ssA = matrix [  True,  True,  True, False,  True,  True,  True ]

ssL :: Matrix X7 Bool
ssL = matrix [ False, False, False,  True,  True,  True, False ]

ssT :: Matrix X7 Bool
ssT = matrix [ False, False, False,  True,  True,  True,  True ]

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
        (CPUDebug{..}, CPUOut{..}) = cpu cpuIn

    let (outputE, outputD) = unpackEnabled cpuOutput

        digit = mux cpuNeedInput (outputD, input)
        (digitHi, digitLo) = both decode . splitByte $ digit
        digitE = outputE .||. cpuNeedInput

        io :: Matrix X7 (Seq Bool)
        io = unpack $ mux cpuNeedInput (o, i)
          where
            i = pack $ fmap pureS ssI :: Seq (Matrix X7 Bool)
            o = pack $ fmap pureS ssO :: Seq (Matrix X7 Bool)

    let haltD :: Matrix X4 (Matrix X7 (Seq Bool))
        haltD = matrix $ map (fmap pureS) [ssH, ssA, ssL, ssT]
        haltE = matrix [high, high, high, high]

        noDigit :: Matrix X7 (Seq Bool)
        noDigit = matrix $ replicate 7 undefinedS

        ioD :: Matrix X4 (Matrix X7 (Seq Bool))
        ioD = matrix [noDigit, io, digitHi, digitLo]
        ioE = matrix [low, digitE, digitE, digitE]

        outD :: Matrix X4 (Matrix X7 (Seq Bool))
        outD = Matrix.zipWith (muxMatrix2 cpuHalt) ioD haltD

        outE :: Matrix X4 (Seq Bool)
        outE = muxMatrix2 cpuHalt ioE haltE

    sseg $ driveSS outE outD

    let dbgFlags = [ cpuExec
                   , cpuHalt
                   , cpuWaitIn
                   , cpuWaitOut
                   , digitE
                   ]
    leds $ matrix $ reverse $ dbgFlags ++ replicate 3 low
  where
    decode :: (sig ~ Signal c) => sig (Unsigned X4) -> Matrix X7 (sig Bool)
    decode = unpack . funMap (Just . decodeHexSS)

muxMatrix2 :: (sig ~ Signal c, Size n, Rep a) => sig Bool -> Matrix n (sig a) -> Matrix n (sig a) -> Matrix n (sig a)
muxMatrix2 b = Matrix.zipWith (curry $ mux b)

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
