fn closure() {
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

fn ascript() {
    (
        G::<u8>::f::<S>(),
        <G<u8> as m::Trait<u8>>::Assoc::f::<S>(),
    )
}

fn conditional() {
    if a == b {
        0
    } else if a > b {
        1
    } else {
        -1
    };

    if let a = b {
        0
    } else if c {
        1
    } else if let d = e {
        -1
    };
}

fn looping() {
    loop {
        print(a);
    }

    while true {
        print(a);
    }

    while let a = true {
        print(a);
    }

    for a in b {
        print(a);
    }
}

fn item() {
    struct S;

    let a = 1;
    let a: u8 = 1;
    let mut a = 1;
}

fn expr() {
    fallible()?;

    [repeat; 1 + 1];

    A::B {};
    A::B { a: () };
    A::B { .. c };
    A::B { a: (), b: (), .. c };
}
