#[macro_use]
extern crate heapsize_derive;

// Demonstrate that hygiene is working correctly by having no `extern crate
// heapsize` in scope here. The macro-generated impl is not for something like
// `::heapsize::HeapSize` like is common in Macros 1.1. It is for the `HeapSize`
// trait brought into scope by the procedural macro definition.
#[derive(HeapSize)]
struct Demo<'a, T: ?Sized> {
    a: Box<T>,
    b: u8,
    c: &'a str,
    d: HeapSize,
}

// This derive is going to generate `impl HeapSize for HeapSize` which is fine
// because the two `HeapSize` tokens have different hygiene context. The first
// one resolves to the `HeapSize` trait brough into scop eby the procedural
// macro definition, and the second one refers to this struct.
//
// Also demonstrate that even though both `impl HeapSize for Demo` and `impl
// HeapSize for HeapSize` spit out a `mod scope`, they do not conflict because
// of hygiene.
#[derive(HeapSize)]
struct HeapSize {
    e: String,
}

// Try to trip up the custom derive by having an inherent method with the same
// name as the trait method. The `impl HeapSize for Demo` is going to generate
// an expression that calls `self.d.heap_size_of_children()`. Ordinarily
// inherent methods take precedence over trait methods, so an unhygienic macro
// implementation would resolve to this method. Instead the call in the
// generated code resolves correctly to the `HeapSize` trait method because that
// is what is in scope within the macro definition.
impl HeapSize {
    #[allow(dead_code)]
    fn heap_size_of_children(&self) -> usize {
        unimplemented!()
    }
}

fn main() {
    extern crate heapsize;

    let demo = Demo {
        a: b"bytestring".to_vec().into_boxed_slice(),
        b: 255,
        c: "&'static str",
        d: HeapSize {
            e: "String".to_owned(),
        },
    };
    println!("heap size = {}", heapsize::HeapSize::heap_size_of_children(&demo));
}
