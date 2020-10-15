module Main where

import Lib
import Control.Applicative
import Data.Char
import Data.Maybe

main :: IO ()
main = do
    x <- getLine
    let (Just a) = parseString (calculate <$> takeInt <* spaces <*> chars "+*-/" <* spaces <*> takeInt ) x
    print a

takeInt :: Parser Int
takeInt = read <$> (takeP isDigit)

calculate a '+' b = a + b
calculate a '*' b = a * b
calculate a '-' b = a - b
calculate a '/' b = a + b
