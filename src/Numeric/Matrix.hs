-----------------------------------------------------------------------------
-- This module exports the external interface to the library.
-----------------------------------------------------------------------------

-- |
-- Module      : Numeric.Matrix
-- Description : Matrix operations over any Haskell type
-- 
-- This package provides matrix types and operations in pure Haskell,
-- with the minimal dependencies @base@ and @array@.
-- 
-- Type classes govern the interface that we offer and instances for
-- two generic matrix types are provided:
-- 'GMatrix' can be used with any Haskell type, and
-- 'UMatrix' works for types that are supported by @UArray@ from the
-- @array@ package.
-- 
-- A type family makes it possible to uniformly use @'Matrix' a@
-- in applications, whether @a = Int@ (using 'UMatrix') or
-- @a = Integer@ (using 'GMatrix'), etc.
-- 
-- Currently, the only operations that are implemented are matrix
-- multiplication ('mult') and a variant of Gaussian elimination
-- that turns a matrix into (possibly non-reduced) row echelon form
-- (see 'MatrixEchelon').

module Numeric.Matrix
    (
    -- * Matrix representation and access
        MatrixRepr(..)
    ,   MatrixAccess(..)
    ,   GMatrix
    ,   UMatrix
    -- ** Generic show function for any 'MatrixRepr'
    ,   showMatrix
    -- * The 'Matrix' type family
    ,   Matrix
    ,   MatrixT
    -- ** Convenience type synonoms
    ,   MatrixReprFor
    ,   MatrixAccessFor
    ,   MatrixEchelonFor
    -- * Matrix multiplication
    ,   mult
    ,   genericMult
    -- * Row echelon form
    ,   MatrixEchelon(..)
    ,   EchelonInfo(..)
    ) where

import Internal.Matrix
import Internal.MatrixFamily
import Internal.Show
import Internal.UnboxedMatrix
