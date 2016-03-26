module Parsing(primary) where

import Control.Applicative
import Data.Char
import Data.Tuple

newtype Parser result = Parser { runParser :: String -> [(String, result)] }


succeed :: r -> Parser r
succeed v = Parser $ \stream -> [(stream, v)]

instance Functor Parser where
    fmap f (Parser pattern) = Parser $ (fmap . fmap . fmap) f pattern


instance Applicative Parser where
    pure result = succeed result
    Parser pattern_map <*> Parser pattern = Parser $ \s -> [(u, f a) | (t, f) <- pattern_map s, (u, a) <- pattern t]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    []   -> []
    a:as
        | p a -> [(as, a)]
        | otherwise -> []

char :: Char -> Parser Char
char = satisfy . (==)

alpha = satisfy isAlpha
digit = satisfy isDigit
space = satisfy isSpace
desirable = satisfy isDesirable
{-
modulus = Parser $ \s -> case s of
    [] -> []
    x@(a:as)
        | (take 3 s == "mod") -> [(drop 3 s, take 3 s)]
        | otherwise -> []
divide = Parser $ \s -> case s of
    [] -> []
    x@(a:as)
        | (take 3 s == "div") -> [(drop 3 s, take 3 s)]
        | otherwise -> []
-}
isDesirable '+' = True
isDesirable '-' = True
isDesirable '*' = True
isDesirable '^' = True
isDesirable '!' = True
isDesirable '|' = True
isDesirable '/' = True
isDesirable '%' = True
isDesirable _ = False

charList :: String -> Parser Char
charList = satisfy . (flip elem)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs


instance Alternative Parser where
    empty = Parser $ const []
    Parser pattern1 <|> Parser pattern2 = Parser $ liftA2 (++) pattern1 pattern2

end :: Parser ()
end = Parser $ \stream -> [(stream, ()) | null stream]

just :: Parser r -> Parser r
just pattern = const <$> pattern <*> end

(.>) :: Parser r1 -> Parser r2 -> Parser r2
parser1 .> parser2 = fmap (flip const) parser1 <*> parser2

(<.) :: Parser r1 -> Parser r2 -> Parser r1
parser1 <. parser2 = fmap const parser1 <*> parser2

(<?>) :: (r -> Bool) -> Parser r -> Parser r
predicate <?> (Parser parser) = Parser $ \s -> [(t, r) | (t, r) <- parser s, predicate r]

number = (fmap (:) digit) <*> (number <|> succeed [])
--my operations
operator =  (fmap (:) desirable) <*> (operator <|> succeed [])

numPar = ((:) <$> ( char '(')) .>  ( ((:[]) <$> (char '-')) <|> succeed[] ) .> number  <. (fmap (:[]) (satisfy (==')') ) )

numberOrOperator = number <|> operator <|> numPar
spaces = someSpaces <|> succeed []
someSpaces = space .> spaces
numbersWithSpaces = (fmap (:)  ((someSpaces .> number) <|> (number <. someSpaces)) ) <*> ( numbersWithSpaces <|> succeed [])

parseAll = (fmap (:)  ( numberOrOperator <|> (someSpaces .> numberOrOperator) <|> (numberOrOperator <. someSpaces)) ) <*> ( parseAll <|> succeed [])

primary :: String -> Int
primary s = calculate ([],snd $ head (runParser parseAll s))

calculate ::([Int],[[Char]]) -> Int
calculate (stack,ls) = case ls of
    [] -> stack!!0
    a:as
        | a == "+" -> calculate ((stack!!0 + stack!!1):(drop 2 stack),as)
        | a == "-" -> calculate ( (stack!!1 - stack!!0):(drop 2 stack),as)
        | a == "*" -> calculate ( (stack!!0 * stack!!1):(drop 2 stack),as)
        | a == "/" -> calculate ( (div (stack!!1) (stack!!0)):(drop 2 stack),as)
        | a == "^" -> calculate ( (stack!!1 ^ stack!!0):(drop 2 stack),as)
        | a == "!" -> calculate ( ( product [1.. (stack!!0)] ):(drop 1 stack),as)
        | a == "%" -> calculate ( (mod (stack!!1)  (stack!!0)):(drop 2 stack),as)
        | a == "|" -> calculate ( (abs$ stack!!0):(drop 1 stack),as)
        | all isDigit a -> calculate ( (read a :: Int):stack,as)
        | otherwise -> stack!!0


{-
data ArithExpr = Number{value::Integer }|Plus{v1::ArithExpr,v2 :: ArithExpr}|Mult{v1:: ArithExpr,v2::ArithExpr}|Div{v1::ArithExpr,v2::ArithExpr}|Mod{v1::ArithExpr,v2::ArithExpr}
    deriving(Show,Eq)
eval:: ArithExpr -> Int
eval (Number value) = value
eval (Plus v1 v2) = (+) (eval v1) (eval v2)
eval (Mult v1 v2) = (*) (eval v1) (eval v2)
eval (Div v1 v2) =  quot (eval v1) (eval v2)
eval (Mod v1 v2) = (mod) (eval v1) (eval v2)
-}
