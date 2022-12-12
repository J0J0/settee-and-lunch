{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-----------------------------------------------------------------------------
-- | Uniform look for matrix types via a type family.
-- 
-- The idea is to use the 'Matrix' type (family) throughout instead of
-- concrete matrix types: it is an open type family and prepopulated with
-- instances for common types (Int, Float, Double -> unboxed; Integer, Ratio,
-- Complex -> boxed), but can be amended by other instances if necessary.
-----------------------------------------------------------------------------

module Internal.MatrixFamily where
    
import Data.Complex (Complex)
import Data.Ratio   (Ratio)

import Data.Kind (Type)

import Internal.Matrix
import Internal.UnboxedMatrix
    
-- | The type family that selects the appropriate matrix type
-- for a coefficient type.
-- 
-- For any type, 'GMatrix' can be used.
-- 
-- For types with a @UArray@ instance (see @array@ package),
-- 'UMatrix' can be used.
-- This is the default for 'Int', 'Float' and 'Double'.
type family MatrixT a :: Type -> Type
type Matrix a = (MatrixT a) a

type instance MatrixT Integer     = GMatrix
type instance MatrixT (Ratio a)   = GMatrix
type instance MatrixT (Complex a) = GMatrix

type instance MatrixT Int    = UMatrix
type instance MatrixT Float  = UMatrix
type instance MatrixT Double = UMatrix

type MatrixReprFor    a = MatrixRepr   (MatrixT a) a
type MatrixAccessFor  a = MatrixAccess (MatrixT a) a
type MatrixEchelonFor a = MatrixEchelon (Matrix a)
