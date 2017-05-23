use super::*;

ast_enum_of_structs! {
    pub enum ConstExpr {
        /// A function call
        pub Call(ConstCall {
            /// The function being called
            pub func: Box<ConstExpr>,

            /// The arguments to the function being called
            pub args: Vec<ConstExpr>,
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
        }),

        /// No-op: used solely so we can pretty-print faithfully
        pub Paren(ConstParen {
            /// Expression that's parenthesized
            pub expr: Box<ConstExpr>,
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
    use {BinOp, Ty};
    use lit::parsing::lit;
    use op::parsing::{binop, unop};
    use ty::parsing::{path, ty};

    named!(pub const_expr -> ConstExpr, do_parse!(
        mut e: alt!(
            expr_unary
            |
            expr_lit
            |
            expr_path
            |
            expr_paren
            // Cannot handle ConstExpr::Other here because for example
            // `[u32; n!()]` would end up successfully parsing `n` as
            // ConstExpr::Path and then fail to parse `!()`. Instead, callers
            // are required to handle Other. See ty::parsing::array_len and
            // data::parsing::discriminant.
        ) >>
        many0!(alt!(
            tap!(args: and_call => {
                e = ConstCall { func: Box::new(e), args: args }.into();
            })
            |
            tap!(more: and_binary => {
                let (op, other) = more;
                e = ConstBinary { op: op, left: Box::new(e), right: Box::new(other) }.into();
            })
            |
            tap!(ty: and_cast => {
                e = ConstCast { expr: Box::new(e), ty: Box::new(ty) }.into();
            })
            |
            tap!(i: and_index => {
                e = ConstIndex { expr: Box::new(e), index: Box::new(i) }.into();
            })
        )) >>
        (e)
    ));

    named!(and_call -> Vec<ConstExpr>, do_parse!(
        punct!("(") >>
        args: terminated_list!(punct!(","), const_expr) >>
        punct!(")") >>
        (args)
    ));

    named!(and_binary -> (BinOp, ConstExpr), tuple!(binop, const_expr));

    named!(expr_unary -> ConstExpr, do_parse!(
        operator: unop >>
        operand: const_expr >>
        (ConstUnary { op: operator, expr: Box::new(operand) }.into())
    ));

    named!(expr_lit -> ConstExpr, map!(lit, ConstExpr::Lit));

    named!(expr_path -> ConstExpr, map!(path, ConstExpr::Path));

    named!(and_index -> ConstExpr, delimited!(punct!("["), const_expr, punct!("]")));

    named!(expr_paren -> ConstExpr, do_parse!(
        punct!("(") >>
        e: const_expr >>
        punct!(")") >>
        (ConstParen { expr: Box::new(e) }.into())
    ));

    named!(and_cast -> Ty, do_parse!(
        keyword!("as") >>
        ty: ty >>
        (ty)
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for ConstCall {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.func.to_tokens(tokens);
            tokens.append("(");
            tokens.append_separated(&self.args, ",");
            tokens.append(")");
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
            tokens.append("as");
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ConstIndex {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            tokens.append("[");
            self.index.to_tokens(tokens);
            tokens.append("]");
        }
    }

    impl ToTokens for ConstParen {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("(");
            self.expr.to_tokens(tokens);
            tokens.append(")");
        }
    }

    #[cfg(not(feature = "full"))]
    impl ToTokens for Other {
        fn to_tokens(&self, _tokens: &mut Tokens) {
            unreachable!()
        }
    }
}
