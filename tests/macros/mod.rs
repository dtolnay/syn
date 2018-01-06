// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

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
