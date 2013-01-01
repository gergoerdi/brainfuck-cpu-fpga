{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Utils
       ( debounce
       , toUnsigned, fromUnsigned
       , splitByte
       , both
       , switch, (==>), oTHERWISE
       ) where

import Language.KansasLava
import Data.Sized.Matrix
import Data.Sized.Unsigned as Unsigned

import Control.Arrow

both :: (Arrow arr) => arr a b -> arr (a, a) (b, b)
both f = f *** f

splitByte :: (sig ~ Signal c) => sig (Unsigned X8) -> (sig (Unsigned X4), sig (Unsigned X4))
splitByte sig = (hi, lo)
  where
    mtx = fromUnsigned sig
    hi = toUnsigned . flip cropAt 4 $ mtx
    lo = toUnsigned . flip cropAt 0 $ mtx

debounce :: forall c sig n. (Clock c, sig ~ Signal c, Size n)
          => Witness n -> sig Bool -> (sig Bool, sig Bool, sig Bool)
debounce _ button = runRTL $ do
    -- Based on http://www.fpga4fun.com/Debouncer2.html
    counter <- newReg (0 :: Unsigned n)
    let counter_max = reg counter .==. maxBound
    toggle <- newReg False

    let idle = reg toggle ./=. button
        down = bitNot (reg toggle) .&&. bitNot idle .&&. counter_max
        up = reg toggle .&&. bitNot idle .&&. counter_max

    CASE [ IF idle $ do
                counter := 0
         , OTHERWISE $ do
                counter := reg counter + 1
                WHEN counter_max $ do
                    toggle := bitNot (reg toggle)
         ]

    return (up, down, reg toggle)

toUnsigned :: (sig ~ Signal c, Size ix) => Matrix ix (sig Bool) -> sig (Unsigned ix)
toUnsigned = coerce Unsigned.fromMatrix . pack

fromUnsigned :: (sig ~ Signal c, Size ix) => sig (Unsigned ix) -> Matrix ix (sig Bool)
fromUnsigned = unpack . coerce Unsigned.toMatrix

switch :: (Eq a, Rep a) => Reg s c a -> [(Maybe a, RTL s c ())] -> RTL s c ()
switch r = CASE . map (uncurry $ maybe OTHERWISE toIF)
  where
    toIF x rtl = IF (reg r .==. pureS x) rtl

(==>) :: (Eq a, Rep a) => a -> RTL s c () -> (Maybe a, RTL s c ())
x ==> rtl = (Just x, rtl)

oTHERWISE :: (Eq a, Rep a) => RTL s c () -> (Maybe a, RTL s c ())
oTHERWISE rtl = (Nothing, rtl)
