#[macro_export]
macro_rules! errorf {
    ($($tt:tt)*) => {{
        use ::std::io::Write;
        let stderr = ::std::io::stderr();
        write!(stderr.lock(), $($tt)*).unwrap();
    }};
}

#[macro_export]
macro_rules! punctuated {
    ($first:expr, $($rest:expr,)*) => {{
        let mut seq = ::syn::punctuated::Punctuated::new();
        let mut last = $first;
        $(
            seq.push(::std::mem::replace(&mut last, $rest));
            seq.push_trailing(::std::default::Default::default());
        )*
        last = last;
        seq.push(last);
        seq
    }};

    ($($e:expr),+) => {
        punctuated!($($e,)+)
    };
}
