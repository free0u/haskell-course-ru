{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда
instance Category (->) where
  -- id :: a -> a
  id x = x
  -- (.) :: (b -> c) -> (a -> b) -> (a -> c)
  f . g = \x -> f $ g x

instance Functor (Either a) where
  fmap f (Left x) = Left x
  fmap f (Right x) = Right $ f x

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just $ f x

instance Functor List where
  fmap = map

instance Monad Maybe where
  return x = Just x
  (Nothing) >>= f = Nothing
  (Just x) >>= f  =  f x

instance Monad List where
  return x = Cons x Nil
  m >>= f  = concatMap f m

--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return x = State (\s -> (s, x))
    -- >>= :: State s a -> (a -> State s b) -> State s b
    comp >>= f = State $ \st ->
      let (st', value) = runState comp st
          comp' = f value
      in runState comp' st'


