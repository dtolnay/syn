#[macro_use]
extern crate heapsize_derive;

extern crate heapsize;
use heapsize::HeapSize;

#[derive(HeapSize)]
struct Demo<'a, T: ?Sized> {
    a: Box<T>,
    b: u8,
    c: &'a str,
    d: String,
}

fn main() {}
