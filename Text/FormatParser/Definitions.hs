
module Text.FormatParser.Definitions where

import Control.Monad

data FormatParser s i o = 
	FormatParser {
		formater :: i -> Maybe (o, [s]),
		parser :: [s] -> Maybe (o, [s])
	}


instance Monad (FormatParser s i) where
	return a = FormatParser (\_ -> Just (a, [])) (\b -> Just (a,b))
	m >>= f = FormatParser formater' parser'
		where
			formater' val = do
				(mval, mstr) <- formater m val
				let n = f mval
				(nval, nstr) <- formater n val
				return (nval, mstr ++ nstr)
			parser' str = do
				(mval, str') <- parser m str
				let n = f mval
				(nval, str'') <- parser n str'
				return (nval, str'')

instance MonadPlus (FormatParser s i) where
	mzero = FormatParser (const Nothing) (const Nothing)
	mplus a b = FormatParser formater' parser'
		where
			formater' val = formater a val `mplus` formater b val
			parser' val = parser a val `mplus` parser b val

