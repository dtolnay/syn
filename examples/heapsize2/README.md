A complete working custom derive that illustrates the hygiene properties of
Macros 2.0. Currently requires a nightly Rust compiler >=1.24.0-nightly but we
are working to stabilize all of the APIs used here.

This is the same example as the unhygienic stable [`heapsize`](../heapsize)
example. Please read that other example first before diving into this one, as
the comments here assume an understanding of how the other one works.
