module Text.FormatParser.Binary where

import Text.FormatParser.Definitions

import Data.Serialize.Put
import Data.Serialize.Get
import Data.Serialize.IEEE754

import Data.Word
import Data.ByteString
import Data.ByteString.Internal (w2c, c2w)


class Bin8 a where
	getBin8 :: Get a
	putBin8 :: a -> Put

instance Bin8 Word8 where
	getBin8 = getWord8
	putBin8 = putWord8

instance Bin8 Int where
	getBin8 = fmap fromIntegral getWord8
	putBin8 = putWord8 . fromIntegral

instance Bin8 Char where
	getBin8 = fmap w2c getWord8
	putBin8 = putWord8 . c2w



class Bin32le a where
	getBin32le :: Get a
	putBin32le :: a -> Put

instance Bin32le Float where
	getBin32le = getFloat32le
	putBin32le = putFloat32le

instance Bin32le Word32 where
	getBin32le = fmap fromIntegral getWord32le
	putBin32le = putWord32le . fromIntegral

instance Bin32le Int where
	getBin32le = fmap fromIntegral getWord32le
	putBin32le = putWord32le . fromIntegral

instance Bin32le Integer where
	getBin32le = fmap fromIntegral getWord32le
	putBin32le = putWord32le . fromIntegral





class Bin64le a where
	getBin64le :: Get a
	putBin64le :: a -> Put

instance Bin64le Double where
	getBin64le = getFloat64le
	putBin64le = putFloat64le

instance Bin64le Word64 where
	getBin64le = fmap fromIntegral getWord64le
	putBin64le = putWord64le . fromIntegral

instance Bin64le Integer where
	getBin64le = fmap fromIntegral getWord64le
	putBin64le = putWord64le . fromIntegral







class Bin32be a where
	getBin32be :: Get a
	putBin32be :: a -> Put

instance Bin32be Float where
	getBin32be = getFloat32be
	putBin32be = putFloat32be

instance Bin32be Word32 where
	getBin32be = fmap fromIntegral getWord32be
	putBin32be = putWord32be . fromIntegral

instance Bin32be Int where
	getBin32be = fmap fromIntegral getWord32be
	putBin32be = putWord32be . fromIntegral

instance Bin32be Integer where
	getBin32be = fmap fromIntegral getWord32be
	putBin32be = putWord32be . fromIntegral





class Bin64be a where
	getBin64be :: Get a
	putBin64be :: a -> Put

instance Bin64be Double where
	getBin64be = getFloat64be
	putBin64be = putFloat64be

instance Bin64be Word64 where
	getBin64be = fmap fromIntegral getWord64be
	putBin64be = putWord64be . fromIntegral

instance Bin64be Integer where
	getBin64be = fmap fromIntegral getWord64be
	putBin64be = putWord64be . fromIntegral





bin8 :: Bin32le a => FormatParser ByteString a a
bin8 = FormatParser formater parser
	where
		formater val = Just (val, runPut $ putBin32le val )
		parser str = case runGetPartial getBin32le str of
			Done result remainder -> Just (result, remainder)
			_ -> Nothing

bin32le :: Bin32le a => FormatParser ByteString a a
bin32le = FormatParser formater parser
	where
		formater val = Just (val, runPut $ putBin32le val )
		parser str = case runGetPartial getBin32le str of
			Done result remainder -> Just (result, remainder)
			_ -> Nothing

bin64le :: Bin64le a => FormatParser ByteString a a
bin64le = FormatParser formater parser
	where
		formater val = Just (val, runPut $ putBin64le val )
		parser str = case runGetPartial getBin64le str of
			Done result remainder -> Just (result, remainder)
			_ -> Nothing

bin32be :: Bin32be a => FormatParser ByteString a a
bin32be = FormatParser formater parser
	where
		formater val = Just (val, runPut $ putBin32be val )
		parser str = case runGetPartial getBin32be str of
			Done result remainder -> Just (result, remainder)
			_ -> Nothing

bin64be :: Bin64be a => FormatParser ByteString a a
bin64be = FormatParser formater parser
	where
		formater val = Just (val, runPut $ putBin64be val )
		parser str = case runGetPartial getBin64be str of
			Done result remainder -> Just (result, remainder)
			_ -> Nothing


