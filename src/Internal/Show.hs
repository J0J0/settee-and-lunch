{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- | Here we define a 'Show' instance for our matrix types.
-----------------------------------------------------------------------------

module Internal.Show where
    
import Data.List (intercalate)
import Data.Semigroup (Max(..))

import Data.Array.IArray  (IArray)
import Data.Array.Unboxed (UArray)

import Internal.Matrix
import Internal.UnboxedMatrix

showMatrix :: (MatrixRepr mat a, Show a)
           => mat a -> String
showMatrix mat = intercalate "\n" [intercalate "  " r | r <- rows']
    where
        rows  = fmap (fmap show) (toRows mat)
        len   = getMax $ foldMap (foldMap (Max . length)) rows
        pad s = replicate (len - length s) ' ' ++ s
        rows' = fmap (fmap pad) rows
        

instance (Show a) => Show (GMatrix a) where
    show = showMatrix

instance (Show a, IArray UArray a) => Show (UMatrix a) where
    show = showMatrix
