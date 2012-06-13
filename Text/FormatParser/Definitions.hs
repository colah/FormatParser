{-# LANGUAGE UndecidableInstances, NoMonomorphismRestriction #-}


module Text.FormatParser.Definitions (FormatParser(..), parseBy, formatBy)  where

import Prelude hiding ((++))
import Control.Monad
import Data.ListLike

(++) = append

data FormatParser s i o = 
	FormatParser {
		formater :: i -> Maybe (o, s),
		parser   :: s -> Maybe (o, s)
	}


instance ListLike s a => Monad (FormatParser s i) where
	return a = FormatParser (\_ -> Just (a, empty)) (\b -> Just (a,b))
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

instance ListLike s a => MonadPlus (FormatParser s i) where
	mzero = FormatParser (const Nothing) (const Nothing)
	mplus a b = FormatParser formater' parser'
		where
			formater' val = formater a val `mplus` formater b val
			parser' val = parser a val `mplus` parser b val


parseBy :: ListLike s a => s -> FormatParser s b c -> Maybe c
parseBy a rw = fmap fst $ parser rw a

formatBy :: ListLike s a => b -> FormatParser s b c -> Maybe s
formatBy a rw = fmap snd $ formater rw a

