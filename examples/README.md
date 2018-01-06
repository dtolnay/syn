### [`dump-syntax`](dump-syntax)

Little utility to parse a Rust source file into a `syn::File` and print out a
debug representation of the syntax tree.

### [`heapsize`](heapsize)

A complete working Macros 1.1 implementation of a custom derive. Works on any
Rust compiler >=1.15.0.

### [`heapsize2`](heapsize2)

The equivalent of the previous example but using fancy new APIs from the nightly
compiler. It illustrates some neat features of the hygiene system of Macros 2.0
and shows how to leverage those to provide amazing error messages to users.
Currently requires a nightly Rust compiler >=1.24.0-nightly but we are working
to stabilize all of the APIs involved.

### [`lazy-static`](lazy-static)

An example of parsing a custom syntax within a `functionlike!(...)` procedural
macro. Demonstrates how to trigger custom warnings and error messages on
individual tokens of the input.

### [`trace-var`](trace-var)

An attribute procedural macro that uses a syntax tree traversal to transform
certain syntax tree nodes in a function body.
