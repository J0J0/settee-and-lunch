-----------------------------------------------------------------------------
-- | Just some utility stuff ...
-----------------------------------------------------------------------------

module Internal.Util (
        (<&>)
    ,   firstM
    ) where
        
infixl 4 <&>
-- | An infix synonym for @flip fmap@.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | (Taken from the monad-loops package:)
-- return the first value from a list, if any, satisfying the given predicate.
firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM p (x:xs) = do
        q <- p x
        if q
                then return (Just x)
                else firstM p xs
