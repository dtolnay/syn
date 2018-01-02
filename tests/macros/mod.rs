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
    ($($e:expr,)+) => {{
        let mut seq = ::syn::punctuated::Punctuated::new();
        $(
            seq.push($e);
        )+
        seq
    }};

    ($($e:expr),+) => {
        punctuated!($($e,)+)
    };
}
