{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module ITMOPrelude.Algebra where

import Prelude (Show)

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
--import ITMOPrelude.Tree

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

-- Инстансы писать сюда

-- monoid by Sum
newtype Sum a = Sum { getSum :: a }	
	
instance Monoid (Sum Nat) where 
  mempty = Sum natZero
  mappend a b = Sum $ getSum a +. getSum b

instance Monoid (Sum Int) where
  mempty = Sum intZero
  mappend a b = Sum $ getSum a .+. getSum b

instance Monoid (Sum Rat) where
  mempty = Sum $ Rat intZero natOne
  mappend a b = Sum $ getSum a %+ getSum b


-- monoid by Product
newtype Product a = Product { getProduct :: a }

instance Monoid (Product Nat) where
  mempty = Product natOne
  mappend a b = Product $ getProduct a *. getProduct b

instance Monoid (Product Int) where
  mempty = Product intOne
  mappend a b = Product $ getProduct a .*. getProduct b

instance Monoid (Product Rat) where
  mempty = Product $ Rat intOne natOne
  mappend a b = Product $ getProduct a %* getProduct b


-- Bool monoids
newtype All = All { getAll :: Bool }
newtype Any = Any { getAny :: Bool }

instance Monoid All where
  mempty = All $ True
  mappend (All False) _ = All False
  mappend (All True)  x = x

instance Monoid Any where
  mempty = Any $ False
  mappend (Any True)  _ = Any True
  mappend (Any False) x = x

-- others monoids
instance Monoid Unit where
  mempty = Unit
  mappend _ _ = Unit

instance Monoid (Maybe a) where
  mempty = Nothing
  mappend (Just x) _ = Just x
  mappend Nothing y = y

instance Monoid Tri where
  mempty = EQ
  mappend EQ y = y
  mappend x _ = x
  
instance Monoid (List a) where
  mempty = Nil
  mappend a b = a ++ b


instance Group (Sum Int) where
  ginv = Sum . intNeg . getSum

instance Group (Sum Rat) where
  ginv = Sum . ratNeg . getSum

instance Group (Product Rat) where
  ginv = Product . ratInv . getProduct
