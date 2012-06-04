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

The Nitty-Gritty Details
-------------------------

A `FormatParser s i o` parses streams of `s`s into output `o`s and formats input `i`s into streams of `s`s.

It is a `Monad` and `MonadPlus`, but that only provides us with a way to route the output values, `o`. We need a way to handle the input values as well!

The key is the `=|=` operator, of type `(i1 -> i2) -> FormatParser s i2 o -> FormatParser s i1 o`, which allows one to transform the input type.




