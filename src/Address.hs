{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Address (EXP, toAddr) where

import Data.Sized.Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Arith

type family EXP a
type instance EXP X0 = X1
type instance EXP (X0_ a) = MUL (EXP a) (EXP a)
type instance EXP (X1_ a) = APP0 (MUL (EXP a) (EXP a))

toAddr :: (Size n, Num m, m ~ EXP n) => Unsigned n -> m
toAddr = fromIntegral . toInteger
