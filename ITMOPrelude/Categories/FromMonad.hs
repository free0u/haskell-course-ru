{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonad where
import ITMOPrelude.Categories

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем из нас
un = un

instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \a -> f a >>= g

instance Monad m => Functor m where
  fmap f x = x >>= (return . f)

instance Monad m => MonadJoin m where
  returnJoin = return
  join = (>>=id)


--class Functor m => MonadJoin m where
--    returnJoin :: a -> m a
--    join :: m (m a) -> m a
