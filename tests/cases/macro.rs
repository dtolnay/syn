macro_rules! test {
    ($($tt:tt)*) => {};
}

fn func() {
    test! { ... }

    test!(...);
    test![...];
}
