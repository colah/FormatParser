{-# LANGUAGE ViewPatterns #-}

module Text.FormatParser.Primitives where

import Control.Monad
import Text.FormatParser.Definitions

parseBy :: [a] -> FormatParser a b c -> Maybe c
parseBy a rw = fmap fst $ parser rw a

formatBy :: b -> FormatParser a b c -> Maybe [a]
formatBy a rw = fmap snd $ formater rw a

a <|> b = a `mplus` b

(=|=) :: (i1 -> i2) -> FormatParser s i2 o -> FormatParser s i1 o
f =|= FormatParser a b = FormatParser (a.f) b

oneOf vals = FormatParser formater parser
	where
		parser (x:xs) | x `elem` vals = Just (x, xs)
		parser _ = Nothing
		formater a | a `elem` vals = Just (a, [a])
		formater _ = Nothing

noneOf vals = FormatParser formater parse
	where
		parse (x:xs) | not (x `elem` vals) = Just (x, xs)
		parse _ = Nothing
		formater a | not (a `elem` vals) = Just (a, [a])
		formater a = Nothing

manyN :: Int -> FormatParser s i o -> FormatParser s [i] [o]
manyN n (FormatParser childformater childParse) = FormatParser formater (parse n)
	where
		parse n (childParse -> Just (v, xs) ) = 
			do 
				(results, remainder) <- parse (n-1) xs
				return (v:results, remainder)
		parse n l | n <= 0 = Just ([], l)
		parse _ _ = Nothing
		formater vals | length vals >= n = do
			(newvals, strs) <- fmap unzip $ sequence $ map childformater vals
			return (newvals, concat strs)

many = manyN 0
many1 = manyN 1

-- minor error, fix later
sepByN :: Int -> FormatParser s i o -> FormatParser s () o2 -> FormatParser s [i] [o]
sepByN 0 inter sep = sepByN 1 inter sep <|> (head =|= inter >>= return . return)
sepByN n inter sep = do
	x <- head =|= inter
	let intersep = do
		(const ()) =|= sep
		a <- inter
		return a
	xs <- tail =|= manyN (n-1) intersep
	return (x:xs)

sepBy1 = sepByN 1
sepBy  = sepByN 0

endByN n inter sep = manyN n $ do
	a <- inter
	(const ()) =|= sep
	return a

endBy1 = endByN 1
endBy  = endByN 0



whitespace :: FormatParser Char a String
whitespace = (const " ") =|= (many $ oneOf " \t\n")

char :: Char -> FormatParser Char a Char
char c = const c =|= oneOf [c]


