{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-----------------------------------------------------------------------------
-- |
-- This module defines the interface and operations for our matrix types.
-- Note that we provide a most general matrix type 'GMatrix' that can take
-- any Haskell type for its coefficients, but our interface also supports
-- more efficient unboxed matrices (see 'UMatrix') for types that have a
-- corresponding 'UArray' instance.
-----------------------------------------------------------------------------

module Internal.Matrix where

import           Prelude (Int, ($), (.), Show, Eq(..), Num(..), subtract, const)
import qualified Prelude as P

import           Control.Monad (forM_, (>>=), return, when, guard)

import           Data.Ix (Ix)
import qualified Data.List as L
import           Data.Maybe (Maybe(..))
import           Data.Foldable (concat, sum)
import           Data.Functor (fmap)
import           Data.STRef

import qualified Data.Array.IArray  as IA
import qualified Data.Array.MArray  as MA
import qualified Data.Array.Unsafe  as UnsafeA

import           Control.Monad.ST   (ST, runST)
import           Data.Array.IArray  (IArray)
import           Data.Array.MArray  (MArray)
import           Data.Array.ST      (STArray, STUArray)
import           Data.Array.Unboxed (UArray)

import           Data.Kind (Type)

import           Internal.Util (firstM, (<&>))


-- | An instance @'MatrixRepr' mat a@ specifies that we
-- can build matrices of type @mat a@ which should have 
-- coefficients in @a@.
-- 
-- We use the usual matrix conventions: index pairs
-- @(Int,Int)@ are given in the order @(row, column)@,
-- and indexing starts with @1@.
class MatrixRepr mat a where
    -- | This function is only required to return a valid @mat a@
    -- on structurally correct input. In particular, the instances
    -- provided by this module throw an error or return a bogus matrix
    -- on any empty lists or rows of different length.
    fromRows  :: [[a]] -> mat a
    fromFun   :: (Int, Int) -> (Int -> Int -> a) -> mat a
    fromAssoc :: (Int, Int) -> [(Int,Int,a)] -> mat a
    
    toRows    :: mat a -> [[a]]
    
    bounds    :: mat a
              -> (Int,Int)  -- ^ (row count, column count)
    
    default toRows :: (MatrixAccess mat a) => mat a -> [[a]]
    toRows mat = [ [mat `at` (i,j) | j <- [1..n]] | i <- [1..m]]
        where
            (m,n) = bounds mat

-- | A 'MatrixAccess' additionally provides random access to its elements.
class (MatrixRepr mat a) => MatrixAccess mat a where
    at :: mat a -> (Int, Int) -> a

type Array  = IA.Array

type family Mutable (ar :: Type -> Type -> Type) :: Type -> Type -> Type -> Type
type instance Mutable IA.Array = STArray
type instance Mutable UArray   = STUArray

type I = (Int,Int)
-- | Generic matrix type via a boxed array;
-- can be used with /any/ haskell type.
data GMatrix a = GMatrix !Int !Int (Array I a)
    deriving (Eq)


fromRowsG' :: (IArray ar a)
           => (Int -> Int -> ar I a -> mat a)  -- ^ mat constructor
           -> [[a]]
           -> mat a
fromRowsG' mkMat []   = mkMat 0 0 $ IA.array ((1,1),(0,0)) []
fromRowsG' mkMat rows = mkMat m n $ IA.listArray ((1,1),(m,n)) elems
    where
        m     = L.length rows
        n     = L.length (L.head rows)
        elems = concat rows

fromAssocG' :: (IArray ar a)
            => (Int -> Int -> ar I a -> mat a)  -- ^ mat constructor
            -> (Int,Int)
            -> [(Int,Int,a)]
            -> mat a
fromAssocG' mkMat (m,n) l = mkMat m n $ IA.array ((1,1),(m,n)) l'
    where
        l' = fmap (\ (i,j,a) -> ((i,j), a)) l

fromFunG' :: (IArray ar a)
          => (Int -> Int -> ar I a -> mat a)  -- ^ mat constructor
          -> (Int, Int)
          -> (Int -> Int -> a)
          -> mat a
fromFunG' mkMat (m,n) f = mkMat m n $ IA.array ((1,1), (m,n)) $
    [ ((i, j), f i j) | i <- [1..m], j <- [1..n] ]

instance MatrixRepr GMatrix a where
    fromRows  = fromRowsG' GMatrix
    fromAssoc = fromAssocG' GMatrix
    fromFun   = fromFunG' GMatrix
    
    bounds (GMatrix m n _) = (m,n)
    
instance MatrixAccess GMatrix a where
    at (GMatrix _ _ ary) = (ary IA.!)


-- | Matrix multiplication that allows mixed matrix types,
-- but is too polymorphic (hence inconvenient) in practice.
-- 
-- Returns @Nothing@ in case of a dimension mismatch.
genericMult :: (MatrixAccess mat1 a, MatrixAccess mat2 a, MatrixRepr mat3 a, Num a)
            => mat1 a -> mat2 a -> Maybe (mat3 a)
genericMult m1 m2 = do
    let (m,p) = bounds m1
        (q,n) = bounds m2
    guard $ p == q
    return $ fromFun (m,n) $ \ i j ->
        sum [(m1 `at` (i,k)) * (m2 `at` (k,j)) | k <- [1..p]]

-- | Matrix multiplication.
-- Specialization of 'genericMult'.
mult :: (MatrixAccess mat a, Num a)
     => mat a -> mat a -> Maybe (mat a)
mult = genericMult


-- | A 'MatrixArrayRepr' is based on an array type that satisfies the
-- 'Data.Array.IArray' interface (see @array@ package).
class (MatrixRepr mat a) => MatrixArrayRepr mat a where
    type ArrayReprType mat :: Type -> Type -> Type
    extractArray :: mat a -> ArrayReprType mat I a
    fromArray    :: ArrayReprType mat I a -> mat a
    
instance MatrixArrayRepr GMatrix a where
    type ArrayReprType GMatrix = Array
    extractArray (GMatrix _ _ ary) = ary
    fromArray ary = GMatrix m n ary
        where
            (_,(m,n)) = IA.bounds ary


-- | A 'MatrixEchelon' can be transformed to row echelon form (non-reduced).
class MatrixEchelon ma where
    -- | Compute the (possibly non-reduced) row echelon form of a matrix.
    echelonize       :: ma -> ma
    -- | Compute the rank of a matrix.
    rank             :: ma -> Int
    -- | Compute the indices of the pivot columns (first column has index 1).
    pivotCols        :: ma -> [Int]
    -- | Compute the invertible matrix that transforms a matrix
    -- to row echelon form.
    -- For exact coefficient types, we have
    --
    -- @
    --   'echelonTransform' m `'mult'` m == Just ('echelonize' m)
    -- @
    -- 
    -- (if there is an @Eq@ instance for @ma@).
    echelonTransform :: ma -> ma
    
    -- | Return all of the above in a bundled 'EchelonInfo'.
    -- 
    -- (Note, that the default implementation for the other functions
    -- are projections after using this function.)
    echelonInfo :: ma -> EchelonInfo ma
    
    echelonize       = echGetMat       . echelonInfo
    rank             = echGetRank      . echelonInfo
    pivotCols        = echGetPivotCols . echelonInfo
    echelonTransform = echGetTransform . echelonInfo
    
data EchelonInfo ma =
    EchelonInfo { echGetMat       :: ma
                , echGetRank      :: Int
                , echGetPivotCols :: [Int]
                , echGetTransform :: ma
                }
    deriving (Show)
    
type ElimArray ar a s = (IArray ar a, MArray ((Mutable ar) s) a (ST s))

-- | Wraper around 'elimination' that handles (un)packing of underlying arrays.
echelonInfoG' :: (Eq a, Num a, MatrixArrayRepr mat a, ElimArray (ArrayReprType mat) a s)
              => (a -> a -> a)  -- ^ optional division (see 'elimination' below)
              -> mat a -> ST s (EchelonInfo (mat a))
echelonInfoG' optDiv mat = do
    rawInfo <- elimination optDiv (bounds mat) (extractArray mat)
    
    let rawAry'  = echGetMat rawInfo
        rawTrans = echGetTransform rawInfo

    return $ 
        rawInfo { echGetMat       = fromArray rawAry'
                , echGetTransform = fromArray rawTrans
                }

echelonInfoG :: (Eq a, Num a)
             => GMatrix a -> EchelonInfo (GMatrix a)
echelonInfoG mat = runST (echelonInfoG' const mat)
-- ↑ can't eliminate 'mat' variable because of type checking,
--   see http://stackoverflow.com/a/9469942

instance {-# OVERLAPPABLE #-} (Eq a, Num a) => MatrixEchelon (GMatrix a) where
    echelonInfo = echelonInfoG

{-
-- we'd really like to have this general (overlapping) instance,
-- but apparently being a subclass doesn't count as "more specific"
-- (https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances).
--
instance (P.Integral i) => MatrixEchelon (GMatrix i) where
    echelonInfo = echelonInfoG' getAryG GMatrix P.div
-}
instance MatrixEchelon (GMatrix Int) where
    echelonInfo mat = runST (echelonInfoG' P.div mat)
instance MatrixEchelon (GMatrix P.Integer) where
    echelonInfo mat = runST (echelonInfoG' P.div mat)


thaw :: (IArray ar a, Ix i, MArray ((Mutable ar) s) a (ST s))
     => ar i a -> ST s ((Mutable ar) s i a)
thaw = MA.thaw

-- | Work horse for the row echelon reduction.
-- 
-- The first argument can be used to divide by the previous pivot element in row
-- operations, e.g. 'Prelude.div' for 'P.Integral' types. Otherwise, simply pass
-- 'Prelude.const', which will behave as a no-op.
-- 
-- The second argument can be used to restrict the function's effect to a specified
-- "view port" of the given array: the elimination algorithm will only treat the
-- rectangular portion of the array that starts with column 1, up to the given maximum,
-- and similarly for rows.
-- This can be useful if the input array consists of an augmented matrix.
elimination :: (Eq a, Num a, ElimArray ar a s)
            => (a -> a -> a)  -- optional division function – or 'const'.
            -> (Int,Int)      -- view port, @(max row, max column)@
            -> ar I a -> ST s (EchelonInfo (ar I a))
elimination optDiv (m,n) ary = do
    let (_,(m',n')) = IA.bounds ary
        
    aa <- thaw ary
    zz <- MA.newArray ((1,1),(m',m')) 0
    
    let (a, a_set)  = matrixGetterSetter aa
        (_, z_set)  = matrixGetterSetter zz
    
    forM_ [1..m'] $ \ i -> z_set i i 1
    
    r_ptr       <- newSTRef 1  -- current row
    pivCols_ptr <- newSTRef [] -- pivot columns
    lastPiv_ptr <- newSTRef 1  -- cache last pivot element
    
    forM_ [1..n] $ \ l -> do
        r <- readSTRef r_ptr
        
        firstM (\ i -> a i l <&> (/= 0)) [r..m] >>= \case
            Nothing -> return ()
            Just k  -> do
                modifySTRef pivCols_ptr (l:)
                
                last_pivot <- readSTRef lastPiv_ptr
                pivot      <- a k l
                        
                when (k /= r) $ do
                    let rowSwap = performRowSwap r k
                    rowSwap aa [l..n']
                    rowSwap zz [1..m]
                
                forM_ [r+1..m] $ \ i -> do
                    factor <- a i l
                    a_set i l 0
                    let rowOp = performRowOp optDiv i r pivot factor last_pivot
                    rowOp aa [l+1..n'] 
                    rowOp zz [1..m]   
                
                modifySTRef' r_ptr (+1)
                writeSTRef lastPiv_ptr pivot
    
    ary'  <- UnsafeA.unsafeFreeze aa
    trans <- UnsafeA.unsafeFreeze zz
    rank_ <- readSTRef r_ptr <&> (subtract 1)
    pivs  <- readSTRef pivCols_ptr
    
    return $ EchelonInfo { echGetMat       = ary'
                         , echGetRank      = rank_
                         , echGetPivotCols = pivs
                         , echGetTransform = trans
                         }


{-# INLINE performRowOp #-}
performRowOp :: (Num a, MArray mar a m)
             => (a -> a -> a)
             -> Int -> Int
             -> a -> a -> a
             -> mar I a
             -> [Int]
             -> m ()
performRowOp optDiv i k piv fac last_piv mat range = do
    let (m, m_set) = matrixGetterSetter mat
    forM_ range $ \ j -> do
        mij <- m i j
        mkj <- m k j
        m_set i j $ (piv * mij - fac * mkj) `optDiv` last_piv
        
{-# INLINE performRowSwap #-}
performRowSwap :: (MArray mar a m)
               => Int -> Int
               -> mar I a
               -> [Int]
               -> m ()
performRowSwap r k mat range = do
    let (m, m_set) = matrixGetterSetter mat
    forM_ range $ \ j -> do
        mrj <- m r j
        mkj <- m k j
        m_set r j $ mkj
        m_set k j $ mrj


matrixGetter :: (MArray mar a m) => mar I a -> Int -> Int -> m a
matrixGetter mat = \ i j -> MA.readArray mat (i,j)

matrixSetter :: (MArray mar a m) => mar I a -> Int -> Int -> a -> m ()
matrixSetter mat = \ i j x -> MA.writeArray mat (i,j) x

matrixGetterSetter :: (MArray mar a m)
                   => mar I a -> (Int -> Int -> m a, Int -> Int -> a -> m ())
matrixGetterSetter mat = (matrixGetter mat, matrixSetter mat)
