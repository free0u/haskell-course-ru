{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadJoin where
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Primitive (($))

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

-- делаем из нас

un = un

instance MonadJoin m => Monad m where
  return = returnJoin
  ma >>= f = join $ fmap f ma

instance MonadJoin m => MonadFish m where
  returnFish = returnJoin
  f >=> g = \x -> join $ fmap g $ f x 
