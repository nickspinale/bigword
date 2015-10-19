# bigword

Fixed-size bit-vectors as wrapped Integers, using `GHC.TypeLits`.
Inspired by the [largeword](https://hackage.org/packages/largeword) package by Dominic Steinitz.

### Motivation

Many domains, such as cryptography and network protocols, have concepts of small pieces of fixed-length binary data.
Examples include port numbers, message digests, flag bits, etc.
Haskell, like most languages, has types such as `Word8`, `Word16`, `Word32`, and `Word64`.
The actual implementation of these typed depends on the host machine's architecture, but they are more or less just wrappers and combinations of normal machine words.
While these all instantiate the `Integral`, `Num`, and `Bits` classes, dealing with them can sometimes be inconvenient.
For example, consider the following piece of code which inspects a `ByteString` for a big-endian `HostAddress` (which is a type synonym for `Word32`).

```haskell
import Data.Attoparsec.ByteString
import Data.Bits
import Network.Socket

hostAddr :: Parser HostAddress
hostAddr = combine <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
  where
    combine a b c d =  shift (fromIntegral a) 24
                   .|. shift (fromIntegral b) 16
                   .|. shift (fromIntegral c) 8 
                   .|. fromIntegral d
```

### Content

This package uses GHC's new type-level naturals (from `GHC.TypeLits` in base) to provide a convenient way for dealing with this sort of fixed-length binary data.
The module `Data.Word.N` exports:

*   The `W (n :: Nat) :: *` `newtype`, which wraps an `Integer` as a bit-vector of length `n`
*   Typeclasses `ToW (n :: Nat) (a :: *) | a -> n` and `FromW (n :: Nat) (a :: *) | a -> n`, which provide typesafe ways of converting between, say, a `W 16` and a `Word16`
*   Base operations `>+<` and `split`, which are concatenation and deconstruction respectively

The module `Data.Word.N.Util` exports:

*   Convenient ways of generalizing monoidal and applicative operations on parts to the whole.
*   Various other utilites

The module `Data.Word.N.Church` is currently a work in progress.
It will soon provide an church-encoding-like interface to `W`'s, similar to but more general than that of the [fixed-vector](https://hackage.haskell.org/package/fixed-vector) package.

### Examples

`assembleR` transforms an applicative action that results in a `W d` to on that results in a `W n`, provided that `d | n` (hence the `:|:` constraint), treating the first results as more significant.
Here is an example using attoparsec to parse a big-endian unsigned 128-bit integer.

```haskell
import Data.Attoparsec.ByteString
import Data.Word

anyWord128BE :: Parser (W 128)
anyWord128BE = assembleR $ fmap (fromIntegral :: Word8 -> W 8) anyWord8
```

`disassembleR` breaks a `W n` into its constituent `d`-sized chunks, and combines them according to the provided monoid.
More significant chunks are combined first.
Here is an example of creating a `Data.ByteString.Builder.builder` from a `W 128`:

```haskell
import Data.ByteString.Builder

word128BE :: W 128 -> Builder
word128BE = disassembleR (word8 . (fromIntegral :: W 8 -> Word8))
```

As you can see, these generic solutions are simpler and more typesafe, and thus less error prone, than the ad-hoc solutions described in the motivations section.

### Non-hackage Dependencies

* [mod-n](https://nickspinale.com/mod-n)
* [data-type-utils](https://nickspinale.com/data-type-utils)

If you have any questions or suggestions, feel free to contact me at by [email](mailto:spinalen@carleton.edu).

### Additional Information

This package's complete documentation can be found here: [nickspinale.com/bigword](https://nickspinale.com/bigword)

If you have any questions or suggestions, feel free to contact me at by [email](mailto:spinalen@carleton.edu).
