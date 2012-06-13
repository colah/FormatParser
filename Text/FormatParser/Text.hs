
module Text.FormatParser (whitespace, char, digit, int) where

import Text.FormatParser.Definitions
import Text.FormatParser.Primitives

whitespace :: FormatParser [Char] a String
whitespace = const " " =|= many (oneOf " \t\n")

char :: Char -> FormatParser [Char] a Char
char c = const c =|= oneOf [c]

digit = oneOf "0123456789"

int :: FormatParser [Char] Int Int
int = 
	(do
		nstr <- show =|= many digit
		return $ read nstr
	) <|> (do
		char '-'
		nstr <- show =|= many digit
		return $ - read nstr
	)



