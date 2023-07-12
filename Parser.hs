module Pax.Parser ( Parser (..)
                  , module Control.Applicative )
where

import Control.Applicative

data Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap fn parser = Parser $ \input ->
        case parse parser input of
            Just (value, rest) -> return (fn value, rest)
            Nothing            -> Nothing

instance Applicative Parser where
    pure :: a -> Parser a
    pure value = Parser $ \input -> Just (value, input)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) fparser xparser = Parser $ \input ->
        case parse fparser input of
            Just (fn, rest) -> parse (fn <$> xparser) rest
            Nothing         -> Nothing

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) p q = Parser $ \input ->
        case parse p input of
            result@(Just (value, rest)) -> result
            Nothing                     -> parse q input

    many :: Parser a -> Parser [a]
    many parser = parse_many
        where
            parse_many = parse_some <|> pure []
            parse_some = (:) <$> parser <*> parse_many

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) parser fn = Parser $ \input ->
        case parse parser input of
            Just (value, rest) -> parse (fn value) rest
            Nothing            -> Nothing
