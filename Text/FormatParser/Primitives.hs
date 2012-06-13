{-# LANGUAGE ViewPatterns, UndecidableInstances, NoMonomorphismRestriction #-}

module Text.FormatParser.Primitives where -- ((<|>),(=|=), oneOf, noneOf, many, many1, sepBy, sepBy1, endBy, endBy1) where

import Prelude hiding (null, (++), map, concat, length, elem, zip, unzip, sequence, tail, head)
import qualified Prelude
import Control.Monad hiding (sequence)
import Text.FormatParser.Definitions
import Data.ListLike
import Data.ListLike.Base


-- usefull later

(++) = append

lldecons l = if null l
	then Nothing
	else Just (head l, tail l)

-- some handy operators

a <|> b = a `mplus` b

(=|=) :: (i1 -> i2) -> FormatParser s i2 o -> FormatParser s i1 o
f =|= FormatParser a b = FormatParser (a.f) b

-- generic single element parsers

oneOf vals = FormatParser formater parser
	where
		parser (lldecons -> Just (x, xs)) | x `elem` vals = Just (x, xs)
		parser _ = Nothing
		formater a | a `elem` vals = Just (a, [a])
		formater _ = Nothing

noneOf vals = FormatParser formater parse
	where
		parse (lldecons -> Just (x, xs)) | not (x `elem` vals) = Just (x, xs)
		parse _ = Nothing
		formater a | not (a `elem` vals) = Just (a, [a])
		formater a = Nothing

-- many


many p = 
	(do
		x  <- head =|= p
		xs <- tail =|= many p
		return (x:xs)
	) <|> return []

manyMinN n p = do
	xs <- many p
	guard (length xs >= n)
	return xs

manyMin1 = manyMinN 1

manyN 0 _ = return []
manyN n p = do
		x  <- head =|= p
		xs <- tail =|= manyN (n-1) p
		return (x:xs)

many2 = manyN 2
many3 = manyN 3
many4 = manyN 4
many5 = manyN 5

manyT2 p = do
	[a,b] <- (\(a,b) -> [a,b]) =|= many2 p
	return (a,b)

manyT3 p = do
	[a,b,c] <- (\(a,b,c) -> [a,b,c]) =|= many3 p
	return (a,b,c)

manyT4 p = do
	[a,b,c,d] <- (\(a,b,c,d) -> [a,b,c,d]) =|= many4 p
	return (a,b,c,d)

manyT5 p = do
	[a,b,c,d,e] <- (\(a,b,c,d,e) -> [a,b,c,d,e]) =|= many5 p
	return (a,b,c,d,e)


--SepBy

sepBy inter sep = sepBy' inter sep <|> return []
	where sepBy' inter sep = 
		(do
			x  <- head =|= inter
			sep
			xs <- tail =|= sepBy inter sep
			return (x:xs)
		) <|> (do
			x  <- head =|= inter
			return [x]
		)

sepByMinN n inter sep = do
	xs <- sepBy inter sep
	guard (length xs >= n)
	return xs

sepByMin1 = sepByMinN 1

sepByN 0 _ _ = return []
sepByN n inter sep = do
		x  <- head =|= inter
		sep
		xs <- tail =|= sepByN (n-1) inter sep
		return (x:xs)

sepBy2 = sepByN 2
sepBy3 = sepByN 3
sepBy4 = sepByN 4
sepBy5 = sepByN 5

sepByT2 inter sep = do
	[a,b] <- (\(a,b) -> [a,b]) =|= sepBy2 inter sep
	return (a,b)

sepByT3 inter sep = do
	[a,b,c] <- (\(a,b,c) -> [a,b,c]) =|= sepBy3 inter sep
	return (a,b,c)

sepByT4 inter sep = do
	[a,b,c,d] <- (\(a,b,c,d) -> [a,b,c,d]) =|= sepBy4 inter sep
	return (a,b,c,d)

sepByT5 inter sep = do
	[a,b,c,d,e] <- (\(a,b,c,d,e) -> [a,b,c,d,e]) =|= sepBy5 inter sep
	return (a,b,c,d,e)


--endBy

endBy inter sep = 
	(do
		x  <- head =|= inter
		sep
		xs <- tail =|= endBy inter sep
		return (x:xs)
	) <|> return []

endByMinN n inter sep = do
	xs <- endBy inter sep
	guard (length xs >= n)
	return xs

endByMin1 = endByMinN 1

endByN 0 _ _ = return []
endByN n inter sep = do
		x  <- head =|= inter
		sep
		xs <- tail =|= endByN (n-1) inter sep
		return (x:xs)

endBy2 = endByN 2
endBy3 = endByN 3
endBy4 = endByN 4
endBy5 = endByN 5

endByT2 inter sep = do
	[a,b] <- (\(a,b) -> [a,b]) =|= endBy2 inter sep
	return (a,b)

endByT3 inter sep = do
	[a,b,c] <- (\(a,b,c) -> [a,b,c]) =|= endBy3 inter sep
	return (a,b,c)

endByT4 inter sep = do
	[a,b,c,d] <- (\(a,b,c,d) -> [a,b,c,d]) =|= endBy4 inter sep
	return (a,b,c,d)

endByT5 inter sep = do
	[a,b,c,d,e] <- (\(a,b,c,d,e) -> [a,b,c,d,e]) =|= endBy5 inter sep
	return (a,b,c,d,e)


