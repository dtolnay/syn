#[macro_use]
extern crate unstable;

pub fn main() {
    demo!((a, b) = (1, 2, 3));
    demo!((a, b, c) = (1, 2, 3));
    demo!((c) = (1, 2, 3));
    demo!((c) ? (1, 2, 3));
    demo!(c = (1, 2, 3));
    demo!((a, b, c) = (1, 2, 3) hi);
}

