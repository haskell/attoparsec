My goal in working on the
[new GHC I/O manager](http://www.serpentine.com/blog/2010/01/22/new-ghc-io-manager-first-benchmark-numbers/)
has been to get the Haskell network stack into a state where it could
be used to attack high-performance and scalable networking problems,
domains in which it has historically been weak.

While it's encouraging to have an excellent networking stack (Johan
and I now have this thoroughly in hand), the next thing I'd look for
is libraries to help build networked applications.  One of the
fundamental things that such apps need to do well is parse data, be it
received from the network or read from files.

The Haskell parsing library of first resort has for years been
[Parsec](http://www.haskell.org/haskellwiki/Parsec).  While other
capable libraries exist
(e.g. [polyparse](http://hackage.haskell.org/package/polyparse) and
[uu-parsinglib](http://hackage.haskell.org/package/uu-parsinglib)),
they don't appear to see much use.

As appealing as Parsec's API is, it has a few problems:

* Parsec 2 is slow, and it has high memory overhead, due to its use of
  Haskell's `String` type for tokens.  Parsec 3 can use the more
  efficient `ByteString` type (which is in any case much more
  appropriate for networked applications that deal in octets), but it
  achieves this flexibility at the cost of being even slower than
  Parsec 2.

* Parsec's API demands that all of a parser's input be available at
  once.  People usually work around this by feeding a Parsec parser
  with lazily read data, but lazy I/O is at odds with my goal of
  writing solid networked code.

What properties should a parsing library for networked applications
ideally possess?  There are a few obvious desiderata that have been
well known for years. For example, it's important to have an appealing
API and programming model.  Parsec squarely fits this desire.
  
Performance is also a big consideration.  Ideally, a parsing library
would be fast enough that you wouldn't feel any real need for either
of the following:

* A few weeks to write an insane hand-bummed parser.
  
* Mechanical parser generators or lexers
  (e.g. [happy](http://www.haskell.org/happy/) or
  [alex](http://www.haskell.org/alex/)).
  
There are some additional important constraints on a realistic
library: it must fit well into a highly concurrent networked world
full of unreliable, hostile and incompetent clients.

* High concurrency levels demand a low per-connection memory
  footprint.

* The need to cope with poorly behaved clients requires that
  applications must be able to throttle connections that are too busy,
  or kill connections that are too slow or attempting to consume too
  many server resources.  A good parsing library will not get in the
  way of these needs.

A few years ago, I made a few half-hearted attempts to write a
specialised version of Parsec, which I eventually named
[Attoparsec](http://hackage.haskell.org/package/attoparsec).

I began with a stripped-down Parsec that was specialised to accept
`ByteString` input.  I then extended the API to allow a parser to
consume small chunks of input at a time.

Because I wasn't using Attoparsec "in anger" at the time, I made sure
that my library worked (more or less), but I was not measuring its
performance.

In late January of this year, I began to think about using Attoparsec
as the parser for a simple HTTP server that I could use to benchmark
our new GHC I/O manager code.  Clearly, I'd want the parser to perform
well, or it would distort my numbers rather badly.

By coincidence, [John MacFarlane](http://johnmacfarlane.net/) emailed
me around the same time, with disturbing findings: he'd tried
Attoparsec, and found its performance to be *terrible*!  In fact, it
was 4 to 20 times _slower_ than plain Parsec with his experimental
parser and test data.  Clearly, I had some hard work to look forward
to.

Happily, that work is now almost complete, and I am pleased with the
results.  In the next post, I'll have some details of what this all
entails.
