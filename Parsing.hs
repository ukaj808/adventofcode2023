module Parsing where

import Control.Applicative (Alternative (..))
import Data.List (nub)

type Parser a = String -> [(a, String)]

--The three primitive parsers defined in this section are the building blocks of combinator parsing.

--The first parser is result v, which succeeds without consuming
-- any of the input string, and returns the single result v
result :: a -> Parser a
result v = \inp -> [(v, inp)]

-- Dually, the parser zero always fails, regardless of the input string
zero :: Parser a
zero = \inp -> []

--Our final primitive is item, which successfully consumes the first character if the
-- input string is non-empty, and fails otherwise:

item :: Parser Char
item = \inp ->
    case inp of
        [] -> []
        (x:xs) -> [(x, xs)]

{-- The definition for bind can be interpreted as follows. First of all, the parser p is
applied to the input string, yielding a list of (value,string) pairs. Now since f is a
function that takes a value and returns a parser, it can be applied to each value
(and unconsumed input string) in turn. This results in a list of lists of (value,string)
pairs, that can then be flattened to a single list using concat. --}
-- p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = 
    \inp -> concat [ f val rest | (val, rest) <- p inp]

{-  Using the bind combinator, we are now able to define some simple but useful
parsers. Recall that the item parser consumes a single character unconditionally. In
practice, we are normally only interested in consuming certain specific characters.
For this reason, we use item to define a combinator sat that takes a predicate (a
Boolean valued function), and yields a parser that consumes a single character if it
satisfies the predicate, and fails otherwise: -}

sat :: (Char -> Bool) -> Parser Char
sat pred = 
    item `bind` \ch -> if pred ch then result ch else zero

{-
Using sat, we can define parsers for specific characters, single digits, lower-case
letters, and upper-case letters:-}
char :: Char -> Parser Char
char ch = sat (== ch)

digit :: Parser Char
digit = sat (\ch -> ch >= '0' && ch <= '9')

lower :: Parser Char
lower = sat (\ch -> ch >= 'a' && ch <= 'z') 

upper :: Parser Char
upper = sat (\ch -> ch >= 'A' && ch <= 'Z') 

firstTwoLower :: Parser String
firstTwoLower =
    lower `bind` \x ->
        lower `bind` \y ->
            result [x,y]

-- A suitable choice combinator for parsers, plus, is as follows
plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp ->
    (p inp ++ q inp)

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = digit `plus` letter

word :: Parser String
word = neWord `plus` result ""
    where
        neWord = 
            letter `bind` \x ->
                word `bind` \xs ->
                    result (x:xs)
    