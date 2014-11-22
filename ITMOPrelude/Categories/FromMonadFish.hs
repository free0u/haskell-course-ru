{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadFish where
import ITMOPrelude.Categories.MonadFish

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin
un = un

-- делаем из нас
instance MonadFish m => Monad m where
    return = returnFish
    f >>= g = ((\_ -> f) >=> g) f

instance MonadFish m => Functor m where
  fmap f ma = ma >>= (return . f)

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id


--class MonadFish m where
--    returnFish :: a -> m a
--    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

--class Monad m where
--    return :: a -> m a
--    (>>=) :: m a -> (a -> m b) -> m b

