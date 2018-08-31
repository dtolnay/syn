#[macro_use]
extern crate heapsize_derive;

extern crate heapsize;

#[derive(HeapSize)]
struct Demo<'a, T: ?Sized> {
    a: Box<T>,
    b: u8,
    c: &'a str,
    d: HeapSize,
}

#[derive(HeapSize)]
struct HeapSize {
    e: String,
}

fn main() {
    let demo = Demo {
        a: b"bytestring".to_vec().into_boxed_slice(),
        b: 255,
        c: "&'static str",
        d: HeapSize {
            e: "String".to_owned(),
        },
    };
    println!(
        "heap size = {}",
        heapsize::HeapSize::heap_size_of_children(&demo)
    );
}
