module Main where

import Lib
import Data.Char
import Data.Maybe

main :: IO ()
main = do
    x <- getLine
    let a = show <$> parseString (calculate <$> takeInt <* spaces <*> chars "+*-/" <* spaces <*> takeInt <* spaces) x
    putStrLn $ fromMaybe "Неправильный синтаксис" a



takeInt :: Parser Int
takeInt = read <$> (takeP isDigit)

calculate a '+' b = a + b
calculate a '*' b = a * b
calculate a '-' b = a - b
calculate a '/' b = a + b
