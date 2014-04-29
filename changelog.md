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
