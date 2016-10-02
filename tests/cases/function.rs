fn f() {
    (
        || (),
        |_| (),
        || -> () { () },
        |a| a,
        |a, b| a + b,
        |a: u8, b: u8| a + b,
        |a, b| -> u8 { a + b },
        move |a, b| a + b,
    )
}
