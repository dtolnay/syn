use std::ops::{AddAssign, MulAssign};

// For implementing base10_digits() accessor on LitInt.
pub struct BigInt {
    digits: Vec<u8>,
}

impl BigInt {
    pub fn new() -> Self {
        BigInt {
            digits: Vec::new(),
        }
    }

    pub fn to_string(&self) -> String {
        unimplemented!()
    }
}

impl AddAssign<u8> for BigInt {
    fn add_assign(&mut self, increment: u8) {
        let _ = increment;
        unimplemented!()
    }
}

impl MulAssign<u8> for BigInt {
    fn mul_assign(&mut self, base: u8) {
        let _ = base;
        unimplemented!()
    }
}
