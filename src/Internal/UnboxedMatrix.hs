{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE StandaloneDeriving     #-}

-----------------------------------------------------------------------------
-- | Unboxed matrix types.
-----------------------------------------------------------------------------

module Internal.UnboxedMatrix where
    
import           Prelude (Int, Float, Double, Eq)
import qualified Prelude as P

import qualified Data.Array.IArray  as IA

import           Control.Monad.ST   (runST)
import           Data.Array.IArray  (IArray)
import           Data.Array.Unboxed (UArray)

import           Internal.Matrix

-- | Generic matrix type via an unboxed array (UArray).
data UMatrix a = UMatrix !Int !Int (UArray I a)
deriving instance (IArray UArray a, Eq a) => Eq (UMatrix a)

instance (IArray UArray a) => MatrixRepr UMatrix a where
    fromRows  = fromRowsG' UMatrix
    fromAssoc = fromAssocG' UMatrix
    fromFun   = fromFunG' UMatrix
    bounds (UMatrix m n _) = (m,n)
    
instance (IArray UArray a) => MatrixAccess UMatrix a where
    at (UMatrix _ _ ary) = (ary IA.!)
    
instance (IArray UArray a) => MatrixArrayRepr UMatrix a where
    type ArrayReprType UMatrix = UArray
    extractArray (UMatrix _ _ ary) = ary
    fromArray ary = UMatrix m n ary
        where
            (_,(m,n)) = IA.bounds ary

{-
-- it seems to be impossible to have a constraint
-- that makes this (general overlappable) instance
-- work ... :(
--
instance {-# OVERLAPPABLE #-} (Eq a, Num a, IArray UArray a, ElimArray (ArrayReprType UMatrix) a s) --"forall s." ?!?
    => MatrixEchelon (UMatrix a) where
    echelonInfo mat = runST (echelonInfoG' P.const mat)
-}
instance MatrixEchelon (UMatrix Float) where
    echelonInfo mat = runST (echelonInfoG' P.const mat)
instance MatrixEchelon (UMatrix Double) where
    echelonInfo mat = runST (echelonInfoG' P.const mat)
-- (...)

instance MatrixEchelon (UMatrix Int) where
    echelonInfo mat = runST (echelonInfoG' P.div mat)
-- (...)
