In my
[first of this pair of articles](http://www.serpentine.com/blog/2010/03/03/whats-in-a-parsing-library-1/),
I laid out some of the qualities I've been looking for in a parsing
library.

Before I dive back into detail, I want to show off some numbers.  The
new Attoparsec code is _fast_.

![Performance](http://chart.apis.google.com/chart?cht=bvs&chs=340x200&chd=t:260,0,0,0,0|0,471,0,0,0|0,0,17037,0,0|0,0,0,23753,0|0,0,0,0,36753&chds=0,40000&chco=4D89F9|894DF9|F94D89|4DB989|1969FD&chdl=260+ms:+http_parser|471+ms:+Attoparsec|17037+ms:+Parsec+3+CPS|23753+ms:+Lazy+Parsec+3+CPS|36753+ms:+Parsec+3&chxt=y&chxl=0:||10|20|30|40&chtt=Time+to+parse+45,668|HTTP+GET+requests)

What did I benchmark?  I captured some real HTTP GET requests from a
live public web server, averaging 431 bytes per request.  I chucked
them into a file, and measured the time needed to parse the entire
contents of the file with the following libraries:

* Ryan Dahl's [http-parser](http://github.com/ry/http-parser) library,
  which is 1,672 lines of hand-rolled C craziness.  Its heritage seems
  to be closely based on the Ragel-generated parser used by Mongrel.
  This library is a fair approximation to about as fast as you can
  get, since it's been tuned for just one purpose.  I wrote a small,
  but reasonably realistic,
  [driver program](http://bitbucket.org/bos/attoparsec/src/tip/examples/rfc2616.c)
  to wire it up to file-based data, adding another 210 lines of code.
  
* An Attoparsec-based
  [HTTP request parser](http://bitbucket.org/bos/attoparsec/src/tip/examples/RFC2616.hs),
  54 lines long, with about 30 lines of
  [driver program](http://bitbucket.org/bos/attoparsec/src/tip/examples/TestRFC2616.hs).
  (Attoparsec itself is about 900 lines of code.)
  
* Several
  [Parsec-3-based parsers](http://bitbucket.org/bos/attoparsec/src/tip/examples/Parsec_RFC2616.hs),
  which are almost identical in length to the Attoparsec-based
  version.
  
The Parsec 3 parsers come in three varieties:

* The fastest uses a patch that Antoine Latter wrote to switch Parsec
  3's internal machinery over to using continuation passing style
  (CPS).  This parser uses `ByteString` for input, and reads the
  entire 18.8MB file in one chunk.
  
* Next is the same parser, using lazy `ByteString` I/O to read the
  file in 64KB chunks.  This costs about 50% in performance, but is
  almost mandatory to maintain a sensible footprint on large inputs.
  
* In last place is the official version of Parsec 3, reading the input
  in one chunk.  (Reading lazily still costs an additional 50%, but I
  didn't want to further clutter the chart with more big numbers.)

What's interesting to me is that the tiny Attoparsec-based parser,
which is more or less a transliteration of the relevant parts of
[RFC 2616](http://www.w3.org/Protocols/rfc2616/rfc2616.html), is so
fast.

I went back and remeasured performance of the Attoparsec and C parsers
on a larger data set (295,568 URLs), and got these numbers:

* Attoparsec: 2.889 seconds, or 102,308 requests/second

* C: 1.614 seconds, or 183,128 requests/second

That clocks the Attoparsec-based parser at about 56% the speed of the
C parser.  Not bad, given that it's about 3.2% the number of lines of
code!

Of course there are tradeoffs involved here.

* Parsec 3 emits much more friendly error messages, and can handle
  many different input types.  Attoparsec, being aimed at
  plumbing-oriented network protocols, considers friendly error
  messages to not be worth the effort, and is specialised to the
  arrays of bytes you get straight off the network.
  
* Parsec 3 requires all of its input to be available when the parser
  is run (either in one large chunk or via lazy I/O).  If Attoparsec
  has insufficient data to return a complete result, it hands back a
  continuation that you provide extra data to.  This eliminates the
  need for lazy I/O and any additional buffering, and makes for a
  beautiful, pure API that doesn't care what its input source is.

The memory footprint of the Attoparsec-based parser is small: it will
run in 568KB of heap on my 64-bit laptop.  The smallest heap size that
the Parsec 3 parser can survive in isn't all that much larger: with
lazily read input, it will run in a 750KB heap.

Overall, this is yet another instance where a little careful attention
to performance yields very exciting results.  Personally, I'd be quite
happy to trade a 97% reduction in code size for such a small
performance hit, especially given the clarity, ease of use, and
flexibility of the resulting code.  (The `http_parser` API is frankly
not so much fun to use, even though I completely understand the
motivation behind it.)
