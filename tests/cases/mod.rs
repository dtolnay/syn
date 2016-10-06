#[cfg(feature = "parsing")]
pub mod parsing {
    //use super::*;
}

extern {
    fn f();
}

extern "C" {
    static x: u8;
}
