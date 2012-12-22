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

import System.FilePath
import System.Directory

ram :: forall c sig dw. (Clock c, sig ~ Signal c, Size dw)
    => (sig Bool, sig Bool)
    -> (sig Bool, sig Bool)
    -> (sig (Unsigned X4), sig (Unsigned dw))
ram (left, right) (up, down) = runRTL $ do
    pointer <- newReg (0 :: Unsigned X4)
    let addr = coerce (fromIntegral . toInteger :: Unsigned X4 -> X16) (reg pointer)
        addr' = coerce (fromIntegral . toInteger :: Unsigned X4 -> X16) (var pointer)
    cellNew <- newReg (0 :: Unsigned dw)

    we <- newReg False
    let ram = writeMemory $ packEnabled (reg we) $ pack (addr, reg cellNew)

    CASE [ IF left $ do
                pointer := reg pointer - 1
                cellNew := asyncRead ram addr'
                we := low
         , IF right $ do
                pointer := reg pointer + 1
                cellNew := asyncRead ram addr'
                we := low
         , IF up $ do
                cellNew := reg cellNew + 1
                we := high
         , IF down $ do
                cellNew := reg cellNew - 1
                we := high
         , OTHERWISE $ do
                we := low
         ]

    return (reg pointer, reg cellNew)

testBench :: (LogicStart fabric) => fabric ()
testBench = do
    Buttons{..} <- buttons
    let (_, up, _) = debounce (Witness :: Witness X16) buttonUp
        (_, down, _) = debounce (Witness :: Witness X16) buttonDown
        (_, left, _) = debounce (Witness :: Witness X16) buttonLeft
        (_, right, _) = debounce (Witness :: Witness X16) buttonRight

    let (addr, dat) = ram (left, right) (up, down)

    let (datDispHi, datDispLo) = both decode $ splitByte dat
        addrDisp = decode addr

    sseg $ driveSSM $ matrix
      [ Just addrDisp
      , Nothing
      , Just datDispHi
      , Just datDispLo
      ]
  where
    decode :: (sig ~ Signal c) => sig (Unsigned X4) -> Matrix X7 (sig Bool)
    decode = unpack . funMap (Just . decodeHexSS)

main :: IO ()
main = do
    kleg <- reifyFabric $ do
        board_init
        testBench

    createDirectoryIfMissing True outPath
    writeVhdlPrelude $ outVHDL "lava-prelude"
    writeVhdlCircuit modName (outVHDL modName) kleg
    writeUCF (outPath </> modName <.> "ucf") kleg
  where
    modName = "IOTest"
    outPath = ".." </> "ise" </> "src"
    outVHDL name = outPath </> name <.> "vhdl"
