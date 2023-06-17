module YIBP.Util (whenLeft) where


whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _        _ = pure ()
{-# INLINE whenLeft #-}