module Lib
    ( Parser,
      runParser,
      parseString,
      charP,
      char,
      chars,
      digit,
      string,
      takeP,
      anything,
      spaces) where
import Data.Char
import Data.List
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> [(String, a)] }

instance Functor Parser where
    fmap f = Parser . (fmap . fmap . fmap) f . runParser 

instance Applicative Parser where
    pure x = Parser $ \s -> [(s, x)]
    (Parser p1) <*> (Parser p2) = Parser f where 
        f str = [ (str2, f x) | (str1, f) <- p1 str, 
                                (str2, x) <- p2 str1 ]
                                
instance Alternative Parser where
    empty = Parser $ const []
    (Parser px) <|> (Parser py) = Parser (\s -> px s ++ py s)

parseString ::  Parser a -> String -> Maybe a
parseString (Parser p) str = 
    case p str of [("", val)] -> Just val
                  _           -> Nothing

charP :: (Char -> Bool) -> Parser Char
charP p = Parser f where
    f (ch:chs) | p ch = [(chs, ch)]
               | otherwise = []

char :: Char -> Parser Char
char ch = charP $ \ch' -> ch == ch'

chars :: [Char] -> Parser Char
chars chs = charP $ \ch' -> elem ch' chs

digit :: Parser Char
digit = charP isDigit

string :: String -> Parser String
string str = Parser f where
    f str' = if str `isPrefixOf` str'
             then [(str' \\ str, str)]
             else []

takeP :: (Char -> Bool) -> Parser String
takeP p = Parser f where
    f str = [(str \\ pre,pre)] where 
        pre = takeWhile p str

spaces :: Parser String
spaces = takeP (\s -> s == ' ') 

anything :: Parser String
anything = Parser $ \str -> [("",str)]

