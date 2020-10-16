module Main where

import Lib
import Data.Char
import Data.Maybe
import Control.Applicative

main :: IO ()
main = do
    x <- getLine
    let a = fmap (parseString takeToken) $ words x
    print a

takeToken :: Parser Token
takeToken = (operand <$> chars "+-") <|> (Num <$> takeInt) 

data Token = 
    Sum | 
    Sub |
    Num Int deriving (Show, Eq)

takeInt :: Parser Int
takeInt = read <$> (takeP isDigit)

operand '+' = Sum
operand '-' = Sub