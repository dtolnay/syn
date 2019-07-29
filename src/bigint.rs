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

    fn reserve_two_digits(&mut self) {
        let len = self.digits.len();
        let desired = len
            + !self.digits.ends_with(&[0, 0]) as usize
            + !self.digits.ends_with(&[0]) as usize;
        self.digits.resize(desired, 0);
    }
}

impl AddAssign<u8> for BigInt {
    // Assumes increment <16.
    fn add_assign(&mut self, mut increment: u8) {
        self.reserve_two_digits();

        let mut i = 0;
        while increment > 0 {
            let sum = self.digits[i] + increment;
            self.digits[i] = sum % 10;
            increment = sum / 10;
            i += 1;
        }
    }
}

impl MulAssign<u8> for BigInt {
    fn mul_assign(&mut self, base: u8) {
        let _ = base;
        unimplemented!()
    }
}
