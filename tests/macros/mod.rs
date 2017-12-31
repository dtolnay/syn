#[macro_export]
macro_rules! errorf {
    ($($tt:tt)*) => {{
        use ::std::io::Write;
        let stderr = ::std::io::stderr();
        write!(stderr.lock(), $($tt)*).unwrap();
    }};
}

#[macro_export]
macro_rules! delimited {
    ($first:expr, $($rest:expr,)*) => {{
        let mut d = ::syn::delimited::Delimited::new();
        let mut last = $first;
        $(
            d.push(::std::mem::replace(&mut last, $rest));
            d.push_trailing(::std::default::Default::default());
        )*
        last = last;
        d.push(last);
        d
    }};

    ($($e:expr),+) => {
        delimited!($($e,)+)
    };
}
