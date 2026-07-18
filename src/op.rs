ast_enum! {
    /// A binary operator: `+`, `+=`, `&`.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    #[non_exhaustive]
    pub enum BinOp {
        /// The `+` operator (addition)
        Add(Token![+]),
        /// The `-` operator (subtraction)
        Sub(Token![-]),
        /// The `*` operator (multiplication)
        Mul(Token![*]),
        /// The `/` operator (division)
        Div(Token![/]),
        /// The `%` operator (modulus)
        Rem(Token![%]),
        /// The `&&` operator (logical and)
        And(Token![&&]),
        /// The `||` operator (logical or)
        Or(Token![||]),
        /// The `^` operator (bitwise xor)
        BitXor(Token![^]),
        /// The `&` operator (bitwise and)
        BitAnd(Token![&]),
        /// The `|` operator (bitwise or)
        BitOr(Token![|]),
        /// The `<<` operator (shift left)
        Shl(Token![<<]),
        /// The `>>` operator (shift right)
        Shr(Token![>>]),
        /// The `==` operator (equality)
        Eq(Token![==]),
        /// The `<` operator (less than)
        Lt(Token![<]),
        /// The `<=` operator (less than or equal to)
        Le(Token![<=]),
        /// The `!=` operator (not equal to)
        Ne(Token![!=]),
        /// The `>=` operator (greater than or equal to)
        Ge(Token![>=]),
        /// The `>` operator (greater than)
        Gt(Token![>]),
        /// The `+=` operator
        AddAssign(Token![+=]),
        /// The `-=` operator
        SubAssign(Token![-=]),
        /// The `*=` operator
        MulAssign(Token![*=]),
        /// The `/=` operator
        DivAssign(Token![/=]),
        /// The `%=` operator
        RemAssign(Token![%=]),
        /// The `^=` operator
        BitXorAssign(Token![^=]),
        /// The `&=` operator
        BitAndAssign(Token![&=]),
        /// The `|=` operator
        BitOrAssign(Token![|=]),
        /// The `<<=` operator
        ShlAssign(Token![<<=]),
        /// The `>>=` operator
        ShrAssign(Token![>>=]),
    }
}

ast_enum! {
    /// A unary operator: `*`, `!`, `-`.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    #[non_exhaustive]
    pub enum UnOp {
        /// The `*` operator for dereferencing
        Deref(Token![*]),
        /// The `!` operator for logical inversion
        Not(Token![!]),
        /// The `-` operator for negation
        Neg(Token![-]),
    }
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use crate::error::Result;
    use crate::op::{BinOp, UnOp};
    use crate::parse::{Parse, ParseStream};

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for BinOp {
        fn parse(input: ParseStream) -> Result<Self> {
            if let Some(op) = input.parse_optional(Token![+=]) {
                Ok(BinOp::AddAssign(op))
            } else if let Some(op) = input.parse_optional(Token![-=]) {
                Ok(BinOp::SubAssign(op))
            } else if let Some(op) = input.parse_optional(Token![*=]) {
                Ok(BinOp::MulAssign(op))
            } else if let Some(op) = input.parse_optional(Token![/=]) {
                Ok(BinOp::DivAssign(op))
            } else if let Some(op) = input.parse_optional(Token![%=]) {
                Ok(BinOp::RemAssign(op))
            } else if let Some(op) = input.parse_optional(Token![^=]) {
                Ok(BinOp::BitXorAssign(op))
            } else if let Some(op) = input.parse_optional(Token![&=]) {
                Ok(BinOp::BitAndAssign(op))
            } else if let Some(op) = input.parse_optional(Token![|=]) {
                Ok(BinOp::BitOrAssign(op))
            } else if let Some(op) = input.parse_optional(Token![<<=]) {
                Ok(BinOp::ShlAssign(op))
            } else if let Some(op) = input.parse_optional(Token![>>=]) {
                Ok(BinOp::ShrAssign(op))
            } else if let Some(op) = input.parse_optional(Token![&&]) {
                Ok(BinOp::And(op))
            } else if let Some(op) = input.parse_optional(Token![||]) {
                Ok(BinOp::Or(op))
            } else if let Some(op) = input.parse_optional(Token![<<]) {
                Ok(BinOp::Shl(op))
            } else if let Some(op) = input.parse_optional(Token![>>]) {
                Ok(BinOp::Shr(op))
            } else if let Some(op) = input.parse_optional(Token![==]) {
                Ok(BinOp::Eq(op))
            } else if let Some(op) = input.parse_optional(Token![<=]) {
                Ok(BinOp::Le(op))
            } else if let Some(op) = input.parse_optional(Token![!=]) {
                Ok(BinOp::Ne(op))
            } else if let Some(op) = input.parse_optional(Token![>=]) {
                Ok(BinOp::Ge(op))
            } else if let Some(op) = input.parse_optional(Token![+]) {
                Ok(BinOp::Add(op))
            } else if let Some(op) = input.parse_optional(Token![-]) {
                Ok(BinOp::Sub(op))
            } else if let Some(op) = input.parse_optional(Token![*]) {
                Ok(BinOp::Mul(op))
            } else if let Some(op) = input.parse_optional(Token![/]) {
                Ok(BinOp::Div(op))
            } else if let Some(op) = input.parse_optional(Token![%]) {
                Ok(BinOp::Rem(op))
            } else if let Some(op) = input.parse_optional(Token![^]) {
                Ok(BinOp::BitXor(op))
            } else if let Some(op) = input.parse_optional(Token![&]) {
                Ok(BinOp::BitAnd(op))
            } else if let Some(op) = input.parse_optional(Token![|]) {
                Ok(BinOp::BitOr(op))
            } else if let Some(op) = input.parse_optional(Token![<]) {
                Ok(BinOp::Lt(op))
            } else if let Some(op) = input.parse_optional(Token![>]) {
                Ok(BinOp::Gt(op))
            } else {
                Err(input.error("expected binary operator"))
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for UnOp {
        fn parse(input: ParseStream) -> Result<Self> {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![*]) {
                input.parse().map(UnOp::Deref)
            } else if lookahead.peek(Token![!]) {
                input.parse().map(UnOp::Not)
            } else if lookahead.peek(Token![-]) {
                input.parse().map(UnOp::Neg)
            } else {
                Err(lookahead.error())
            }
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use crate::op::{BinOp, UnOp};
    use proc_macro2::TokenStream;
    use quote::ToTokens;

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for BinOp {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                BinOp::Add(t) => t.to_tokens(tokens),
                BinOp::Sub(t) => t.to_tokens(tokens),
                BinOp::Mul(t) => t.to_tokens(tokens),
                BinOp::Div(t) => t.to_tokens(tokens),
                BinOp::Rem(t) => t.to_tokens(tokens),
                BinOp::And(t) => t.to_tokens(tokens),
                BinOp::Or(t) => t.to_tokens(tokens),
                BinOp::BitXor(t) => t.to_tokens(tokens),
                BinOp::BitAnd(t) => t.to_tokens(tokens),
                BinOp::BitOr(t) => t.to_tokens(tokens),
                BinOp::Shl(t) => t.to_tokens(tokens),
                BinOp::Shr(t) => t.to_tokens(tokens),
                BinOp::Eq(t) => t.to_tokens(tokens),
                BinOp::Lt(t) => t.to_tokens(tokens),
                BinOp::Le(t) => t.to_tokens(tokens),
                BinOp::Ne(t) => t.to_tokens(tokens),
                BinOp::Ge(t) => t.to_tokens(tokens),
                BinOp::Gt(t) => t.to_tokens(tokens),
                BinOp::AddAssign(t) => t.to_tokens(tokens),
                BinOp::SubAssign(t) => t.to_tokens(tokens),
                BinOp::MulAssign(t) => t.to_tokens(tokens),
                BinOp::DivAssign(t) => t.to_tokens(tokens),
                BinOp::RemAssign(t) => t.to_tokens(tokens),
                BinOp::BitXorAssign(t) => t.to_tokens(tokens),
                BinOp::BitAndAssign(t) => t.to_tokens(tokens),
                BinOp::BitOrAssign(t) => t.to_tokens(tokens),
                BinOp::ShlAssign(t) => t.to_tokens(tokens),
                BinOp::ShrAssign(t) => t.to_tokens(tokens),
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for UnOp {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                UnOp::Deref(t) => t.to_tokens(tokens),
                UnOp::Not(t) => t.to_tokens(tokens),
                UnOp::Neg(t) => t.to_tokens(tokens),
            }
        }
    }
}
