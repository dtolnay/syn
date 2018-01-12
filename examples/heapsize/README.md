A complete working Macros 1.1 implementation of a custom derive. Works on any
Rust compiler >=1.15.0.

- [`heapsize/src/lib.rs`](heapsize/src/lib.rs)
- [`heapsize_derive/src/lib.rs`](heapsize_derive/src/lib.rs)
- [`example/src/main.rs`](example/src/main.rs)

We are deriving the `HeapSize` trait which computes an estimate of the amount of
heap memory owned by a value.

```rust
pub trait HeapSize {
    /// Total number of bytes of heap memory owned by `self`.
    fn heap_size_of_children(&self) -> usize;
}
```

The custom derive allows users to write `#[derive(HeapSize)]` on data structures
in their program.

```rust
#[derive(HeapSize)]
struct Demo<'a, T: ?Sized> {
    a: Box<T>,
    b: u8,
    c: &'a str,
    d: String,
}
```
