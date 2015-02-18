0.12.1.3

* Fixed incorrect tracking of Text lengths
  (https://github.com/bos/attoparsec/issues/80)

0.12.1.2

* Fixed the incorrect tracking of capacity if the initial buffer was
  empty (https://github.com/bos/attoparsec/issues/75)

0.12.1.1

* Fixed a data corruption bug that occurred under some circumstances
  if a buffer grew after prompting for more input
  (https://github.com/bos/attoparsec/issues/74)

0.12.1.0

* Now compatible with GHC 7.9

* Reintroduced the Chunk class, used by the parsers package

0.12.0.0

* A new internal representation makes almost all real-world parsers
  faster, sometimes by big margins.  For example, parsing JSON data
  with aeson is now up to 70% faster.  These performance improvements
  also come with reduced memory consumption and some new capabilities.

* The new match combinator gives both the result of a parse and the
  input that it matched.

* The test suite has doubled in size.  This made it possible to switch
  to the new internal representation with a decent degree of
  confidence that everything was more or less working.

* The benchmark suite now contains a small family of benchmarks taken
  from real-world uses of attoparsec.

* A few types that ought to have been private now are.

* A few obsolete modules and functions have been marked as deprecated.
  They will be removed from the next major release.

0.11.3.0

* New function scientific is compatible with rational, but parses
  integers more efficiently (https://github.com/bos/aeson/issues/198)

0.11.2.0

* The new Chunk typeclass allows for some code sharing with Ed
  Kmett's parsers package: http://hackage.haskell.org/package/parsers

* New function runScanner generalises scan to return the final state
  of the scanner as well as the input consumed.


0.11.1.0

* New dependency: the scientific package.  This allows us to parse
  numbers much more efficiently.

* peekWord8', peekChar': new primitive parsers that allow
  single-character lookahead.
