turnt-octo-wight
================

For CS 6535 at Northeastern University (Spring 2014), testing shared-memory concurrent programs in C

In the spirit of [ConTest][contest_ref], this project can be used to
instrument C code in order to explore different thread interleavings
at runtime.

requirements
------------
- [Haskell Platform (preferably 7.6.*)](http://www.haskell.org/platform/)
- gcc (version ?)

[contest_ref]:https://www.research.ibm.com/haifa/projects/verification/contest/

caveats
-------
- Translating assignments of the form `x *= E;` to `x = x * E` is not
  necessarily sound.
- Use of `++` and `--` are discouraged, as they are not annotated as
  writes.
