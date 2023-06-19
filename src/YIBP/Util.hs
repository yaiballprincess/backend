module YIBP.Util (whenLeft, whenNothing) where


whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _        _ = pure ()
{-# INLINE whenLeft #-}

whenNothing :: Applicative m => Maybe a -> m a -> m a
whenNothing (Just x) _ = pure x
whenNothing Nothing a = a