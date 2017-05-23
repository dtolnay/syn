use tokens;

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum BinOp {
        /// The `+` operator (addition)
        Add(tokens::Add),
        /// The `-` operator (subtraction)
        Sub(tokens::Sub),
        /// The `*` operator (multiplication)
        Mul(tokens::Star),
        /// The `/` operator (division)
        Div(tokens::Div),
        /// The `%` operator (modulus)
        Rem(tokens::Rem),
        /// The `&&` operator (logical and)
        And(tokens::AndAnd),
        /// The `||` operator (logical or)
        Or(tokens::OrOr),
        /// The `^` operator (bitwise xor)
        BitXor(tokens::Caret),
        /// The `&` operator (bitwise and)
        BitAnd(tokens::And),
        /// The `|` operator (bitwise or)
        BitOr(tokens::Or),
        /// The `<<` operator (shift left)
        Shl(tokens::Shl),
        /// The `>>` operator (shift right)
        Shr(tokens::Shr),
        /// The `==` operator (equality)
        Eq(tokens::EqEq),
        /// The `<` operator (less than)
        Lt(tokens::Lt),
        /// The `<=` operator (less than or equal to)
        Le(tokens::Le),
        /// The `!=` operator (not equal to)
        Ne(tokens::Ne),
        /// The `>=` operator (greater than or equal to)
        Ge(tokens::Ge),
        /// The `>` operator (greater than)
        Gt(tokens::Gt),
        /// The `+=` operator
        AddEq(tokens::AddEq),
        /// The `-=` operator
        SubEq(tokens::SubEq),
        /// The `*=` operator
        MulEq(tokens::MulEq),
        /// The `/=` operator
        DivEq(tokens::DivEq),
        /// The `%=` operator
        RemEq(tokens::RemEq),
        /// The `^=` operator
        BitXorEq(tokens::CaretEq),
        /// The `&=` operator
        BitAndEq(tokens::AndEq),
        /// The `|=` operator
        BitOrEq(tokens::OrEq),
        /// The `<<=` operator
        ShlEq(tokens::ShlEq),
        /// The `>>=` operator
        ShrEq(tokens::ShrEq),
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum UnOp {
        /// The `*` operator for dereferencing
        Deref(tokens::Star),
        /// The `!` operator for logical inversion
        Not(tokens::Bang),
        /// The `-` operator for negation
        Neg(tokens::Sub),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    named!(pub binop -> BinOp, alt!(
        punct!("&&") => { |_| BinOp::And(tokens::AndAnd::default()) }
        |
        punct!("||") => { |_| BinOp::Or(tokens::OrOr::default()) }
        |
        punct!("<<") => { |_| BinOp::Shl(tokens::Shl::default()) }
        |
        punct!(">>") => { |_| BinOp::Shr(tokens::Shr::default()) }
        |
        punct!("==") => { |_| BinOp::Eq(tokens::EqEq::default()) }
        |
        punct!("<=") => { |_| BinOp::Le(tokens::Le::default()) }
        |
        punct!("!=") => { |_| BinOp::Ne(tokens::Ne::default()) }
        |
        punct!(">=") => { |_| BinOp::Ge(tokens::Ge::default()) }
        |
        punct!("+") => { |_| BinOp::Add(tokens::Add::default()) }
        |
        punct!("-") => { |_| BinOp::Sub(tokens::Sub::default()) }
        |
        punct!("*") => { |_| BinOp::Mul(tokens::Star::default()) }
        |
        punct!("/") => { |_| BinOp::Div(tokens::Div::default()) }
        |
        punct!("%") => { |_| BinOp::Rem(tokens::Rem::default()) }
        |
        punct!("^") => { |_| BinOp::BitXor(tokens::Caret::default()) }
        |
        punct!("&") => { |_| BinOp::BitAnd(tokens::And::default()) }
        |
        punct!("|") => { |_| BinOp::BitOr(tokens::Or::default()) }
        |
        punct!("<") => { |_| BinOp::Lt(tokens::Lt::default()) }
        |
        punct!(">") => { |_| BinOp::Gt(tokens::Gt::default()) }
    ));

    #[cfg(feature = "full")]
    named!(pub assign_op -> BinOp, alt!(
        punct!("+=") => { |_| BinOp::AddEq(tokens::AddEq::default()) }
        |
        punct!("-=") => { |_| BinOp::SubEq(tokens::SubEq::default()) }
        |
        punct!("*=") => { |_| BinOp::MulEq(tokens::MulEq::default()) }
        |
        punct!("/=") => { |_| BinOp::DivEq(tokens::DivEq::default()) }
        |
        punct!("%=") => { |_| BinOp::RemEq(tokens::RemEq::default()) }
        |
        punct!("^=") => { |_| BinOp::BitXorEq(tokens::CaretEq::default()) }
        |
        punct!("&=") => { |_| BinOp::BitAndEq(tokens::AndEq::default()) }
        |
        punct!("|=") => { |_| BinOp::BitOrEq(tokens::OrEq::default()) }
        |
        punct!("<<=") => { |_| BinOp::ShlEq(tokens::ShlEq::default()) }
        |
        punct!(">>=") => { |_| BinOp::ShrEq(tokens::ShrEq::default()) }
    ));

    named!(pub unop -> UnOp, alt!(
        punct!("*") => { |_| UnOp::Deref(tokens::Star::default()) }
        |
        punct!("!") => { |_| UnOp::Not(tokens::Bang::default()) }
        |
        punct!("-") => { |_| UnOp::Neg(tokens::Sub::default()) }
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for BinOp {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                BinOp::Add(ref t) => t.to_tokens(tokens),
                BinOp::Sub(ref t) => t.to_tokens(tokens),
                BinOp::Mul(ref t) => t.to_tokens(tokens),
                BinOp::Div(ref t) => t.to_tokens(tokens),
                BinOp::Rem(ref t) => t.to_tokens(tokens),
                BinOp::And(ref t) => t.to_tokens(tokens),
                BinOp::Or(ref t) => t.to_tokens(tokens),
                BinOp::BitXor(ref t) => t.to_tokens(tokens),
                BinOp::BitAnd(ref t) => t.to_tokens(tokens),
                BinOp::BitOr(ref t) => t.to_tokens(tokens),
                BinOp::Shl(ref t) => t.to_tokens(tokens),
                BinOp::Shr(ref t) => t.to_tokens(tokens),
                BinOp::Eq(ref t) => t.to_tokens(tokens),
                BinOp::Lt(ref t) => t.to_tokens(tokens),
                BinOp::Le(ref t) => t.to_tokens(tokens),
                BinOp::Ne(ref t) => t.to_tokens(tokens),
                BinOp::Ge(ref t) => t.to_tokens(tokens),
                BinOp::Gt(ref t) => t.to_tokens(tokens),
                BinOp::AddEq(ref t) => t.to_tokens(tokens),
                BinOp::SubEq(ref t) => t.to_tokens(tokens),
                BinOp::MulEq(ref t) => t.to_tokens(tokens),
                BinOp::DivEq(ref t) => t.to_tokens(tokens),
                BinOp::RemEq(ref t) => t.to_tokens(tokens),
                BinOp::BitXorEq(ref t) => t.to_tokens(tokens),
                BinOp::BitAndEq(ref t) => t.to_tokens(tokens),
                BinOp::BitOrEq(ref t) => t.to_tokens(tokens),
                BinOp::ShlEq(ref t) => t.to_tokens(tokens),
                BinOp::ShrEq(ref t) => t.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for UnOp {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                UnOp::Deref(ref t) => t.to_tokens(tokens),
                UnOp::Not(ref t) => t.to_tokens(tokens),
                UnOp::Neg(ref t) => t.to_tokens(tokens),
            }
        }
    }
}
