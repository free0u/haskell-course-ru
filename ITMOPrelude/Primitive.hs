{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1
nat2 = natOne +. natOne
nat3 = nat2 +. natOne
nat4 = nat3 +. natOne
nat5 = nat4 +. natOne
nat6 = nat5 +. natOne
nat7 = nat6 +. natOne
nat8 = nat7 +. natOne
nat9 = nat8 +. natOne

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq m n = case natCmp m n of
  EQ -> True
  _ -> False

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt m n = case natCmp m n of
  LT -> True
  _ -> False

natGt :: Nat -> Nat -> Bool
natGt m n = case natCmp m n of
  GT -> True
  _ -> False


infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
m -. Zero = m
n -. (Succ m) = case (n -. m) of
  Zero -> Zero
  Succ x -> x

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod _ Zero = undefined
natDivMod n m = case natCmp n m of
  LT -> Pair natZero n
  _ -> let p = natDivMod (n -. m) m in
    Pair (Succ $ fst p) $ snd p

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd a Zero = a
gcd a b = gcd b $ natMod a b

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero   = Pos natZero   -- 0
intOne    = Pos natOne     -- 1
intNegOne = Neg natZero -- -1
int2 = Pos $ Succ $ Succ Zero
int3 = Pos $ Succ $ Succ $ Succ Zero
int4 = Pos $ Succ $ Succ $ Succ $ Succ Zero
intn2 = Neg $ Succ Zero
intn3 = Neg $ Succ $ Succ Zero
intn4 = Neg $ Succ $ Succ $ Succ Zero

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = Pos Zero
intNeg (Pos (Succ x)) = Neg x
intNeg (Neg x) = Pos $ Succ x

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Neg _) (Pos _) = LT
intCmp (Pos _) (Neg _) = GT
intCmp (Pos m) (Pos n) = m `natCmp` n
intCmp (Neg m) (Neg n) = n `natCmp` m

intEq :: Int -> Int -> Bool
intEq m n = case m `intCmp` n of
  EQ -> True
  _ -> False

intLt :: Int -> Int -> Bool
intLt m n = case m `intCmp` n of
  LT -> True
  _ -> False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Pos n) .+. (Pos m) = Pos $ n +. m
(Neg n) .+. (Neg m) = Neg $ Succ $ n +. m
(Neg m) .+. (Pos Zero) = Neg m
(Neg Zero) .+. (Pos (Succ n)) = Pos n
(Neg (Succ m)) .+. (Pos (Succ n)) = (Neg m) .+. (Pos n)
m .+. n = n .+. m

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Pos m) .*. (Pos n) = Pos $ m *. n
m .*. (Neg n) = intNeg $ m .*. (intNeg $ Neg n)
m .*. n = n .*. m
-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show)

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Pos a) b) = Rat (Pos b) a
ratInv x = ratNeg $ ratInv $ ratNeg x

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat x y) (Rat a b) =
  let y' = Pos y
      b' = Pos b in
  (x .*. b') `intCmp` (a .*. y')

ratEq :: Rat -> Rat -> Bool
ratEq m n = case m `ratCmp` n of
  EQ -> True
  _ -> False

ratLt :: Rat -> Rat -> Bool
ratLt m n = case m `ratCmp` n of
  LT -> True
  _ -> False

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat x y) %+ (Rat a b) = Rat p q where
  p = (Pos b) .*. x .+. (Pos y) .*. a
  q = b *. y

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat x y) %* (Rat a b) = Rat p q where
  p = x .*. a
  q = y *. b

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c 
f . g = \ x -> f (g x)

infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
