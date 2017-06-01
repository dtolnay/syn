use super::*;
use delimited::Delimited;

ast_enum_of_structs! {
    pub enum ConstExpr {
        /// A function call
        pub Call(ConstCall {
            /// The function being called
            pub func: Box<ConstExpr>,

            /// The arguments to the function being called
            pub args: Delimited<ConstExpr, tokens::Comma>,

            pub paren_token: tokens::Paren,
        }),

        /// A binary operation (For example: `a + b`, `a * b`)
        pub Binary(ConstBinary {
            /// The binary operation this represents
            pub op: BinOp,

            /// The left-hand-side of the constant binary op
            pub left: Box<ConstExpr>,

            /// The right-hand-side of the constant binary op
            pub right: Box<ConstExpr>,
        }),

        /// A unary operation (For example: `!x`, `*x`)
        pub Unary(ConstUnary {
            /// Operation being performed
            pub op: UnOp,

            /// Expression acted on
            pub expr: Box<ConstExpr>,
        }),

        /// A literal (For example: `1`, `"foo"`)
        pub Lit(Lit),

        /// A cast (`foo as f64`)
        pub Cast(ConstCast {
            /// Value being casted
            pub expr: Box<ConstExpr>,

            /// Type casted to
            pub ty: Box<Ty>,

            pub as_token: tokens::As,
        }),

        /// Variable reference, possibly containing `::` and/or type
        /// parameters, e.g. foo::bar::<baz>.
        pub Path(Path),

        /// An indexing operation (`foo[2]`)
        pub Index(ConstIndex {
            /// Value that is being indexed
            pub expr: Box<ConstExpr>,

            /// Index expression
            pub index: Box<ConstExpr>,

            pub bracket_token: tokens::Bracket,
        }),

        /// No-op: used solely so we can pretty-print faithfully
        pub Paren(ConstParen {
            /// Expression that's parenthesized
            pub expr: Box<ConstExpr>,
            pub paren_token: tokens::Paren,
        }),

        /// If compiling with full support for expression syntax, any expression is
        /// allowed
        pub Other(Other),
    }
}

#[cfg(not(feature = "full"))]
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Other {
    _private: (),
}

#[cfg(feature = "full")]
pub type Other = Expr;

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use synom::Synom;
    use synom::tokens::*;

    impl Synom for ConstExpr {
        named!(parse -> Self, do_parse!(
            mut e: alt!(
                map!(syn!(ConstUnary), |e: ConstUnary| e.into())
                |
                map!(syn!(Lit), |e: Lit| e.into())
                |
                map!(syn!(Path), |e: Path| e.into())
                |
                map!(syn!(ConstParen), |e: ConstParen| e.into())
                // Cannot handle ConstExpr::Other here because for example
                // `[u32; n!()]` would end up successfully parsing `n` as
                // ConstExpr::Path and then fail to parse `!()`. Instead, callers
                // are required to handle Other. See ty::parsing::array_len and
                // data::parsing::discriminant.
            ) >>
            many0!(alt!(
                tap!(args: and_call => {
                    let (args, paren) = args;
                    e = ConstCall {
                        func: Box::new(e),
                        args: args,
                        paren_token: paren,
                    }.into();
                })
                |
                tap!(more: and_binary => {
                    let (op, other) = more;
                    e = ConstBinary { op: op, left: Box::new(e), right: Box::new(other) }.into();
                })
                |
                tap!(ty: and_cast => {
                    let (ty, token) = ty;
                    e = ConstCast {
                        expr: Box::new(e),
                        ty: Box::new(ty),
                        as_token: token,
                    }.into();
                })
                |
                tap!(i: and_index => {
                    let (i, bracket) = i;
                    e = ConstIndex {
                        expr: Box::new(e),
                        index: Box::new(i),
                        bracket_token: bracket,
                    }.into();
                })
            )) >>
            (e)
        ));
    }

    named!(and_call -> (Delimited<ConstExpr, tokens::Comma>, tokens::Paren),
           parens!(call!(Delimited::parse_terminated)));

    named!(and_binary -> (BinOp, ConstExpr),
           tuple!(call!(BinOp::parse_binop), syn!(ConstExpr)));

    impl Synom for ConstUnary {
        named!(parse -> Self, do_parse!(
            operator: syn!(UnOp) >>
            operand: syn!(ConstExpr) >>
            (ConstUnary { op: operator, expr: Box::new(operand) })
        ));
    }

    named!(and_index -> (ConstExpr, tokens::Bracket),
           brackets!(syn!(ConstExpr)));

    impl Synom for ConstParen {
        named!(parse -> Self, do_parse!(
            parens: parens!(syn!(ConstExpr)) >>
            (ConstParen {
                expr: Box::new(parens.0),
                paren_token: parens.1,
            })
        ));
    }

    named!(and_cast -> (Ty, tokens::As), do_parse!(
        as_tok: syn!(As) >>
        ty: syn!(Ty) >>
        (ty, as_tok)
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for ConstCall {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.func.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ConstBinary {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for ConstUnary {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.op.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ConstCast {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.as_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ConstIndex {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.bracket_token.surround(tokens, |tokens| {
                self.index.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ConstParen {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            })
        }
    }

    #[cfg(not(feature = "full"))]
    impl ToTokens for Other {
        fn to_tokens(&self, _tokens: &mut Tokens) {
            unreachable!()
        }
    }
}
