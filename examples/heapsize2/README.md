A complete working custom derive that illustrates the hygiene properties of
Macros 2.0. Currently requires a nightly Rust compiler >=1.24.0-nightly but we
are working to stabilize all of the APIs used here.

This is the same example as the unhygienic stable [`heapsize`](../heapsize)
example. Please read that other example first before diving into this one, as
the comments here assume an understanding of how the other one works.

- [`heapsize/src/lib.rs`](heapsize/src/lib.rs) (unchanged)
- [`heapsize_derive/src/lib.rs`](heapsize_derive/src/lib.rs)
- [`example/src/main.rs`](example/src/main.rs)

A major advantage of spans and the token-based procedural macro API is that we
get great control over where the compiler's error messages are displayed.
Consider the error the user sees if one of their field types does not implement
`HeapSize`.

```rust
#[derive(HeapSize)]
struct Broken {
    ok: String,
    bad: std::thread::Thread,
}
```

The Macros 1.1 string-based procedural macro and a naively implemented
token-based procedural macro result in the following error.

```
error[E0599]: no method named `heap_size_of_children` found for type `std::thread::Thread` in the current scope
 --> src/main.rs:4:10
  |
4 | #[derive(HeapSize)]
  |          ^^^^^^^^
```

With just a bit of work, as shown in the `heapsize_derive` implementation here,
we can improve this error to point out exactly which field is not right.

```
error[E0277]: the trait bound `std::thread::Thread: HeapSize` is not satisfied
 --> src/main.rs:7:5
  |
7 |     bad: std::thread::Thread,
  |     ^^^ the trait `HeapSize` is not implemented for `std::thread::Thread`
```

Some unstable APIs in the `proc-macro2` crate let us improve this further by
joining together the span of the field name and the field type. There is no
difference in our code -- everything is as shown in this directory -- but
building the example crate with `cargo build` shows errors like the one above
and building with `RUSTFLAGS='--cfg procmacro2_semver_exempt' cargo build` is
able to show errors like the following.

```
error[E0277]: the trait bound `std::thread::Thread: HeapSize` is not satisfied
 --> src/main.rs:7:5
  |
7 |     bad: std::thread::Thread,
  |     ^^^^^^^^^^^^^^^^^^^^^^^^ the trait `HeapSize` is not implemented for `std::thread::Thread`
```
