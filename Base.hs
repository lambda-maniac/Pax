module Pax.Base ( module Pax.Parser
                , module Pax.Constants
                , tokenized
                , character
                , parsers
                , string
                , spaces
                , space
                , oneof
                , cast
                , eof
                , (<&>) )
where

import Pax.Parser
import Pax.Constants

character :: Char -> Parser Char
character c = Parser $ parser'
    where
        parser' (x:xs) = if x == c then Just (x, xs) else Nothing
        parser' [    ] = Nothing

eof :: Parser String
eof = Parser $ parser'
    where
        parser' [ ] = Just ("", "")
        parser'  _  = Nothing

space :: Parser Char
space = character ' '

spaces :: Parser String
spaces = many space

tokenized :: Parser a -> Parser a
tokenized parser' = spaces *> parser'

cast :: Parser a -> Parser [a]
cast = ((: []) <$>)

parsers :: String -> [Parser Char]
parsers = map character

string :: String -> Parser String
string = sequence . parsers

oneof :: String -> Parser Char
oneof = foldr (<|>) empty . parsers

{- This kind of looks like the implementation of `sequence` for lists. -}
infixl 4 <&>
(<&>) :: Parser String -> Parser String -> Parser String
(<&>) p q = (<>) <$> p <*> q
