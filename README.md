FormatParser: Parsec-inspired (Simultaneous) Parsing & Formatting
=================================================================

Parsec and friends makes writing parsers in Haskell a lovely experience. But, if you're like me, you find yourself irate with the duplication of effort that comes with writing separate formaters. After all, they both convey essentially the same information!

The following, from *Real World Haskell* is one of my favorite Parsec examples:

```haskell
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'
```

Beautiful! We just said what a `csvFile` was and had a parser! But there is certainly enough information there to format the data as well. Why can't we?

With FormatParser you can!

```haskell
import Text.FormatParser.Primitives

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'
```

```haskell
> "abc,def,egh\nfoo,bar,blah\n" `parseBy` csvFile 
Just [["abc","def","egh"],["foo","bar","blah"]]
> [["abc","def","egh"],["foo","bar","blah"]] `formatBy` csvFile
Just "abc,def,egh\nfoo,bar,blah\n"
```

FormatParser also provides really cool binary parsing tools. But we'll need to get used to working with FormatParser Monads before we get to that...

Monadic Examples and Friends
----------------------------

So, how do these work as monads?

```haskell
int :: FormatParser Char Int Int
int = do
	n <- show =|= many digit
	return (read n)
```

```haskell
> "123" `parseBy` int
Just 123
> 123 `formatBy` int
Just "123"

```

For the most part, this will look familiar to users of Parsec. The only new thing is `show =|=`. This is for describing how the input value should be converted into the input value of that component of the parser (in this case, the `many digit`s we want to show correspond to showing the input integer). This may be more clear if we wrote it as:

```haskell
int :: FormatParser Char Int Int
int = do
	n <- (\inVal -> show inVal) =|= many digit
	return (read n)
```

Read it as `many digit` being equal to showing the input value.

From here we can make something more interesting. For example, let's parse a two-tuple of numbers:

```haskell
twoTuple :: FormatParser Char (Int, Int) (Int, Int)
twoTuple = do
	char '('
	whitespace
	-- first number parse corresponds to first
	-- component of input
	a <- fst =|= int
	whitespace
	char ','
	whitespace
	-- first number parse corresponds to second
	-- component of input
	b <- snd =|= int
	whitespace
	char ')'
	return (a,b)
```

```haskell
> "(1, 3)" `parseBy` twoTuple
Just (1,3)
> (1, 3) `formatBy` twoTuple
Just "( 1 , 3 )"
```

Now let's consider a strange format where a list of comma separated numbers where the first one tells us how many of the others we should parse.

```haskell
strange :: FormatParser Char [Int] [Int]
strange = do
	-- The first number corresponds to the number of values we'll parse,
	-- the length of our input value
	len  <- length =|= int
	-- Then, using our friend from Control.Monad, we loop over those.
	vals <- forM [0 .. len - 1] $ \n -> do
		-- The comma separator
		char ','
		-- And then the value, which corresponds to the 
		-- value in position n of the input
		(!! n) =|= int
	return vals
```

```haskell
> "3,23,45,21,5" `parseBy` strange
Just [23,45,21]
> [2^n | n <- [0..4]] `formatBy` strange 
Just "5,1,2,4,8,16"
```

**Note on Style**: If you need no information from the input value, make it polymorphic instead of `()`. For example:

```haskell
-- Good
whitespace :: FormatParser Char a String
whitespace = const " " =|= many (oneOf " \t\n")

-- bad
char :: Char -> FormatParser Char () Char
char c = const c =|= oneOf [c]
```

The reason is that one wants to be able to write things like

```haskell
do
	int
	whitespace
	int
	char ','
	int
```

Instead of 

```haskell
do
	int
	const () =|= whitespace
	int
	const () =|= char ','
	int
```

On the other hand, when you need an input of a parser with zero information, use a `()`. This avoids second rank types and makes them more flexible. For example, `sepBy :: FormatParser s i o -> FormatParser s () o2 -> FormatParser s [i] [o]`.


Binary Parsing
--------------

`Text.FormatParser.Binary` provides a number of useful binary parsing primitives: `bin8`, `bin32le` (little Endian), `bin32be` (big Endian)... These are all polymorphic because of the different things that can be represented in 8 bits or 32 bits. If you want to get a 32 bit `Int`

```haskell
do
	...
	foo :: Int <- bin32be
	...
```

and if you want a `Float`

```haskell
do
	...
	foo :: Float <- bin32be
	...
```

For a more specific example, lets write a [binary STL](http://en.wikipedia.org/wiki/STL_%28file_format%29#Binary_STL) parser. 

```haskell
type Vec = (Float, Float, Float)
type Triangle = (Vec, Vec, Vec)

vec :: FormatParser ByteString Vec Vec
vec = manyT3 bin32be

bstl :: FormatParser ByteString [Triangle] [Triangle]
bstl = do
	header :: [Int]      <- (const [1..80]) =|= manyN 80 bin8
	len    :: Int        <- length          =|= bin32be
	tris   :: [Triangle] <- manyN len $ do
				const (0,0,0) =|= vec -- throw away normal
				tri <- manyT3 vec
				return tri
	return tris
```


