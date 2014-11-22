-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library
module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like
    ) where

--------------------------------------------------------------------------------
-- Определения

-- Тело этого определения можно заменить на всё, что захочется
data ParseError = ParseError String
                deriving (Show) -- лишь бы show был

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }

instance Monad (Monstupar s) where
    return a = Monstupar $ \s -> Right (s, a)
    -- (>>=) :: m a -> (a -> m b) -> m b
    -- Monstupar s a -> (a -> Monstupad s b) -> Monstupar s b
    ma >>= f = Monstupar $ \inp -> case runParser ma inp of
      Left e -> Left e
      Right (inp', a) -> runParser (f a) inp' 
      

--------------------------------------------------------------------------------
-- Примитивные парсеры.
-- Имена и сигнатуры функций менять нельзя, тела можно

-- Всё хорошо
ok :: Monstupar s ()
ok = Monstupar $ \s -> Right (s , ())

-- Не должно парситься парсером p
isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \s -> case runParser p s of
    Left e -> Right (s , ())
    Right _ -> Left $ ParseError "Expected fail"

-- Конец ввода
eof :: Monstupar s ()
eof = Monstupar $ \s -> case s of
    [] -> Right (s , ())
    _  -> Left $ ParseError "Expected EOF"

infixr 2 <|>
-- Сначала первый парсер, если он фейлится, то второй
(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
a <|> b = Monstupar $ \s -> case runParser a s of
  Right (s', a) -> Right (s', a)
  Left _ -> runParser b s

-- В голове ввода сейчас нечто, удовлетворяющее p
like :: (s -> Bool) -> Monstupar s s
like p = Monstupar $ \s -> case s of
  [] -> Left $ ParseError "Unexpected EOF in like"
  (x:xs) -> if p x then
              Right (xs, x)
            else
              Left $ ParseError "Predicate fail in like"

-- Сюда можно добавлять ещё какие-то примитивные парсеры
-- если они понадобятся

