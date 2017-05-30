use super::*;
use delimited::Delimited;

ast_struct! {
    /// An expression.
    pub struct Expr {
        /// Type of the expression.
        pub node: ExprKind,

        /// Attributes tagged on the expression.
        pub attrs: Vec<Attribute>,
    }
}

impl From<ExprKind> for Expr {
    fn from(node: ExprKind) -> Expr {
        Expr {
            node: node,
            attrs: Vec::new(),
        }
    }
}

ast_enum_of_structs! {
    pub enum ExprKind {
        /// A `box x` expression.
        pub Box(ExprBox {
            pub expr: Box<Expr>,
            pub box_token: tokens::Box,
        }),

        /// E.g. 'place <- val'.
        pub InPlace(ExprInPlace {
            pub place: Box<Expr>,
            pub value: Box<Expr>,
            pub in_token: tokens::In,
        }),

        /// An array, e.g. `[a, b, c, d]`.
        pub Array(ExprArray {
            pub exprs: Delimited<Expr, tokens::Comma>,
            pub bracket_token: tokens::Bracket,
        }),

        /// A function call.
        pub Call(ExprCall {
            pub func: Box<Expr>,
            pub args: Delimited<Expr, tokens::Comma>,
            pub paren_token: tokens::Paren,
        }),

        /// A method call (`x.foo::<Bar, Baz>(a, b, c, d)`)
        ///
        /// The `Ident` is the identifier for the method name.
        /// The vector of `Ty`s are the ascripted type parameters for the method
        /// (within the angle brackets).
        ///
        /// Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
        /// `ExprKind::MethodCall(foo, [Bar, Baz], [x, a, b, c, d])`.
        pub MethodCall(ExprMethodCall {
            pub expr: Box<Expr>,
            pub method: Ident,
            pub typarams: Delimited<Ty, tokens::Comma>,
            pub args: Delimited<Expr, tokens::Comma>,
            pub paren_token: tokens::Paren,
            pub dot_token: tokens::Dot,
            pub lt_token: Option<tokens::Lt>,
            pub colon2_token: Option<tokens::Colon2>,
            pub gt_token: Option<tokens::Gt>,
        }),

        /// A tuple, e.g. `(a, b, c, d)`.
        pub Tup(ExprTup {
            pub args: Delimited<Expr, tokens::Comma>,
            pub paren_token: tokens::Paren,
            pub lone_comma: Option<tokens::Comma>,
        }),

        /// A binary operation, e.g. `a + b`, `a * b`.
        pub Binary(ExprBinary {
            pub op: BinOp,
            pub left: Box<Expr>,
            pub right: Box<Expr>,
        }),

        /// A unary operation, e.g. `!x`, `*x`.
        pub Unary(ExprUnary {
            pub op: UnOp,
            pub expr: Box<Expr>,
        }),

        /// A literal, e.g. `1`, `"foo"`.
        pub Lit(Lit),

        /// A cast, e.g. `foo as f64`.
        pub Cast(ExprCast {
            pub expr: Box<Expr>,
            pub as_token: tokens::As,
            pub ty: Box<Ty>,
        }),

        /// A type ascription, e.g. `foo: f64`.
        pub Type(ExprType {
            pub expr: Box<Expr>,
            pub colon_token: tokens::Colon,
            pub ty: Box<Ty>,
        }),

        /// An `if` block, with an optional else block
        ///
        /// E.g., `if expr { block } else { expr }`
        pub If(ExprIf {
            pub cond: Box<Expr>,
            pub if_true: Block,
            pub if_false: Option<Box<Expr>>,
            pub if_token: tokens::If,
            pub else_token: Option<tokens::Else>,
        }),

        /// An `if let` expression with an optional else block
        ///
        /// E.g., `if let pat = expr { block } else { expr }`
        ///
        /// This is desugared to a `match` expression.
        pub IfLet(ExprIfLet {
            pub pat: Box<Pat>,
            pub expr: Box<Expr>,
            pub if_true: Block,
            pub if_false: Option<Box<Expr>>,
            pub if_token: tokens::If,
            pub let_token: tokens::Let,
            pub eq_token: tokens::Eq,
            pub else_token: Option<tokens::Else>,
        }),

        /// A while loop, with an optional label
        ///
        /// E.g., `'label: while expr { block }`
        pub While(ExprWhile {
            pub cond: Box<Expr>,
            pub body: Block,
            pub label: Option<Ident>,
            pub colon_token: Option<tokens::Colon>,
            pub while_token: tokens::While,
        }),

        /// A while-let loop, with an optional label.
        ///
        /// E.g., `'label: while let pat = expr { block }`
        ///
        /// This is desugared to a combination of `loop` and `match` expressions.
        pub WhileLet(ExprWhileLet {
            pub pat: Box<Pat>,
            pub expr: Box<Expr>,
            pub body: Block,
            pub label: Option<Ident>,
            pub colon_token: Option<tokens::Colon>,
            pub while_token: tokens::While,
            pub let_token: tokens::Let,
            pub eq_token: tokens::Eq,
        }),

        /// A for loop, with an optional label.
        ///
        /// E.g., `'label: for pat in expr { block }`
        ///
        /// This is desugared to a combination of `loop` and `match` expressions.
        pub ForLoop(ExprForLoop {
            pub pat: Box<Pat>,
            pub expr: Box<Expr>,
            pub body: Block,
            pub label: Option<Ident>,
            pub for_token: tokens::For,
            pub colon_token: Option<tokens::Colon>,
            pub in_token: tokens::In,
        }),

        /// Conditionless loop with an optional label.
        ///
        /// E.g. `'label: loop { block }`
        pub Loop(ExprLoop {
            pub body: Block,
            pub label: Option<Ident>,
            pub loop_token: tokens::Loop,
            pub colon_token: Option<tokens::Colon>,
        }),

        /// A `match` block.
        pub Match(ExprMatch {
            pub match_token: tokens::Match,
            pub brace_token: tokens::Brace,
            pub expr: Box<Expr>,
            pub arms: Vec<Arm>,
        }),

        /// A closure (for example, `move |a, b, c| a + b + c`)
        pub Closure(ExprClosure {
            pub capture: CaptureBy,
            pub decl: Box<FnDecl>,
            pub body: Box<Expr>,
            pub or1_token: tokens::Or,
            pub or2_token: tokens::Or,
        }),

        /// A block (`{ ... }` or `unsafe { ... }`)
        pub Block(ExprBlock {
            pub unsafety: Unsafety,
            pub block: Block,
        }),

        /// An assignment (`a = foo()`)
        pub Assign(ExprAssign {
            pub left: Box<Expr>,
            pub right: Box<Expr>,
            pub eq_token: tokens::Eq,
        }),

        /// An assignment with an operator
        ///
        /// For example, `a += 1`.
        pub AssignOp(ExprAssignOp {
            pub op: BinOp,
            pub left: Box<Expr>,
            pub right: Box<Expr>,
        }),

        /// Access of a named struct field (`obj.foo`)
        pub Field(ExprField {
            pub expr: Box<Expr>,
            pub field: Ident,
            pub dot_token: tokens::Dot,
        }),

        /// Access of an unnamed field of a struct or tuple-struct
        ///
        /// For example, `foo.0`.
        pub TupField(ExprTupField {
            pub expr: Box<Expr>,
            pub field: Lit,
            pub dot_token: tokens::Dot,
        }),

        /// An indexing operation (`foo[2]`)
        pub Index(ExprIndex {
            pub expr: Box<Expr>,
            pub index: Box<Expr>,
            pub bracket_token: tokens::Bracket,
        }),

        /// A range (`1..2`, `1..`, `..2`, `1...2`, `1...`, `...2`)
        pub Range(ExprRange {
            pub from: Option<Box<Expr>>,
            pub to: Option<Box<Expr>>,
            pub limits: RangeLimits,
        }),

        /// Variable reference, possibly containing `::` and/or type
        /// parameters, e.g. foo::bar::<baz>.
        ///
        /// Optionally "qualified",
        /// E.g. `<Vec<T> as SomeTrait>::SomeType`.
        pub Path(ExprPath {
            pub qself: Option<QSelf>,
            pub path: Path,
        }),

        /// A referencing operation (`&a` or `&mut a`)
        pub AddrOf(ExprAddrOf {
            pub and_token: tokens::And,
            pub mutbl: Mutability,
            pub expr: Box<Expr>,
        }),

        /// A `break`, with an optional label to break, and an optional expression
        pub Break(ExprBreak {
            pub label: Option<Ident>,
            pub expr: Option<Box<Expr>>,
            pub break_token: tokens::Break,
        }),

        /// A `continue`, with an optional label
        pub Continue(ExprContinue {
            pub label: Option<Ident>,
            pub continue_token: tokens::Continue,
        }),

        /// A `return`, with an optional value to be returned
        pub Ret(ExprRet {
            pub expr: Option<Box<Expr>>,
            pub return_token: tokens::Return,
        }),

        /// A macro invocation; pre-expansion
        pub Mac(Mac),

        /// A struct literal expression.
        ///
        /// For example, `Foo {x: 1, y: 2}`, or
        /// `Foo {x: 1, .. base}`, where `base` is the `Option<Expr>`.
        pub Struct(ExprStruct {
            pub path: Path,
            pub fields: Delimited<FieldValue, tokens::Comma>,
            pub rest: Option<Box<Expr>>,
            pub dot2_token: Option<tokens::Dot2>,
            pub brace_token: tokens::Brace,
        }),

        /// An array literal constructed from one repeated element.
        ///
        /// For example, `[1; 5]`. The first expression is the element
        /// to be repeated; the second is the number of times to repeat it.
        pub Repeat(ExprRepeat {
            pub bracket_token: tokens::Bracket,
            pub semi_token: tokens::Semi,
            pub expr: Box<Expr>,
            pub amt: Box<Expr>,
        }),

        /// No-op: used solely so we can pretty-print faithfully
        pub Paren(ExprParen {
            pub expr: Box<Expr>,
            pub paren_token: tokens::Paren,
        }),

        /// `expr?`
        pub Try(ExprTry {
            pub expr: Box<Expr>,
            pub question_token: tokens::Question,
        }),

        /// A catch expression.
        ///
        /// E.g. `do catch { block }`
        pub Catch(ExprCatch {
            pub do_token: tokens::Do,
            pub catch_token: tokens::Catch,
            pub block: Block,
        }),
    }
}

ast_struct! {
    /// A field-value pair in a struct literal.
    pub struct FieldValue {
        /// Name of the field.
        pub ident: Ident,

        /// Value of the field.
        pub expr: Expr,

        /// Whether this is a shorthand field, e.g. `Struct { x }`
        /// instead of `Struct { x: x }`.
        pub is_shorthand: bool,

        /// Attributes tagged on the field.
        pub attrs: Vec<Attribute>,

        pub colon_token: Option<tokens::Colon>,
    }
}

ast_struct! {
    /// A Block (`{ .. }`).
    ///
    /// E.g. `{ .. }` as in `fn foo() { .. }`
    pub struct Block {
        pub brace_token: tokens::Brace,
        /// Statements in a block
        pub stmts: Vec<Stmt>,
    }
}

ast_enum! {
    /// A statement, usually ending in a semicolon.
    pub enum Stmt {
        /// A local (let) binding.
        Local(Box<Local>),

        /// An item definition.
        Item(Box<Item>),

        /// Expr without trailing semicolon.
        Expr(Box<Expr>),

        /// Expression with trailing semicolon;
        Semi(Box<Expr>, tokens::Semi),

        /// Macro invocation.
        Mac(Box<(Mac, MacStmtStyle, Vec<Attribute>)>),
    }
}

ast_enum! {
    /// How a macro was invoked.
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum MacStmtStyle {
        /// The macro statement had a trailing semicolon, e.g. `foo! { ... };`
        /// `foo!(...);`, `foo![...];`
        Semicolon(tokens::Semi),

        /// The macro statement had braces; e.g. foo! { ... }
        Braces,

        /// The macro statement had parentheses or brackets and no semicolon; e.g.
        /// `foo!(...)`. All of these will end up being converted into macro
        /// expressions.
        NoBraces,
    }
}

ast_struct! {
    /// Local represents a `let` statement, e.g., `let <pat>:<ty> = <expr>;`
    pub struct Local {
        pub let_token: tokens::Let,
        pub colon_token: Option<tokens::Colon>,
        pub eq_token: Option<tokens::Eq>,
        pub semi_token: tokens::Semi,

        pub pat: Box<Pat>,
        pub ty: Option<Box<Ty>>,

        /// Initializer expression to set the value, if any
        pub init: Option<Box<Expr>>,
        pub attrs: Vec<Attribute>,
    }
}

ast_enum_of_structs! {
    // Clippy false positive
    // https://github.com/Manishearth/rust-clippy/issues/1241
    #[cfg_attr(feature = "cargo-clippy", allow(enum_variant_names))]
    pub enum Pat {
        /// Represents a wildcard pattern (`_`)
        pub Wild(PatWild {
            pub underscore_token: tokens::Underscore,
        }),

        /// A `Pat::Ident` may either be a new bound variable (`ref mut binding @ OPT_SUBPATTERN`),
        /// or a unit struct/variant pattern, or a const pattern (in the last two cases the third
        /// field must be `None`). Disambiguation cannot be done with parser alone, so it happens
        /// during name resolution.
        pub Ident(PatIdent {
            pub mode: BindingMode,
            pub ident: Ident,
            pub subpat: Option<Box<Pat>>,
            pub at_token: Option<tokens::At>,
        }),

        /// A struct or struct variant pattern, e.g. `Variant {x, y, ..}`.
        /// The `bool` is `true` in the presence of a `..`.
        pub Struct(PatStruct {
            pub path: Path,
            pub fields: Delimited<FieldPat, tokens::Comma>,
            pub brace_token: tokens::Brace,
            pub dot2_token: Option<tokens::Dot2>,
        }),

        /// A tuple struct/variant pattern `Variant(x, y, .., z)`.
        /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
        /// 0 <= position <= subpats.len()
        pub TupleStruct(PatTupleStruct {
            pub path: Path,
            pub pat: PatTuple,
        }),

        /// A possibly qualified path pattern.
        /// Unquailfied path patterns `A::B::C` can legally refer to variants, structs, constants
        /// or associated constants. Quailfied path patterns `<A>::B::C`/`<A as Trait>::B::C` can
        /// only legally refer to associated constants.
        pub Path(PatPath {
            pub qself: Option<QSelf>,
            pub path: Path,
        }),

        /// A tuple pattern `(a, b)`.
        /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
        /// 0 <= position <= subpats.len()
        pub Tuple(PatTuple {
            pub pats: Delimited<Pat, tokens::Comma>,
            pub dots_pos: Option<usize>,
            pub paren_token: tokens::Paren,
            pub dot2_token: Option<tokens::Dot2>,
            pub comma_token: Option<tokens::Comma>,
        }),
        /// A `box` pattern
        pub Box(PatBox {
            pub pat: Box<Pat>,
            pub box_token: tokens::Box,
        }),
        /// A reference pattern, e.g. `&mut (a, b)`
        pub Ref(PatRef {
            pub pat: Box<Pat>,
            pub mutbl: Mutability,
            pub and_token: tokens::And,
        }),
        /// A literal
        pub Lit(PatLit {
            pub expr: Box<Expr>,
        }),
        /// A range pattern, e.g. `1...2`
        pub Range(PatRange {
            pub lo: Box<Expr>,
            pub hi: Box<Expr>,
            pub limits: RangeLimits,
        }),
        /// `[a, b, ..i, y, z]` is represented as:
        pub Slice(PatSlice {
            pub front: Delimited<Pat, tokens::Comma>,
            pub middle: Option<Box<Pat>>,
            pub back: Delimited<Pat, tokens::Comma>,
            pub dot2_token: Option<tokens::Dot2>,
            pub comma_token: Option<tokens::Comma>,
            pub bracket_token: tokens::Bracket,
        }),
        /// A macro pattern; pre-expansion
        pub Mac(Mac),
    }
}

ast_struct! {
    /// An arm of a 'match'.
    ///
    /// E.g. `0...10 => { println!("match!") }` as in
    ///
    /// ```rust,ignore
    /// match n {
    ///     0...10 => { println!("match!") },
    ///     // ..
    /// }
    /// ```
    pub struct Arm {
        pub attrs: Vec<Attribute>,
        pub pats: Delimited<Pat, tokens::Or>,
        pub if_token: Option<tokens::If>,
        pub guard: Option<Box<Expr>>,
        pub rocket_token: tokens::Rocket,
        pub body: Box<Expr>,
        pub comma: Option<tokens::Comma>,
    }
}

ast_enum! {
    /// A capture clause
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum CaptureBy {
        Value(tokens::Move),
        Ref,
    }
}

ast_enum! {
    /// Limit types of a range (inclusive or exclusive)
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum RangeLimits {
        /// Inclusive at the beginning, exclusive at the end
        HalfOpen(tokens::Dot2),
        /// Inclusive at the beginning and end
        Closed(tokens::Dot3),
    }
}

ast_struct! {
    /// A single field in a struct pattern
    ///
    /// Patterns like the fields of Foo `{ x, ref y, ref mut z }`
    /// are treated the same as `x: x, y: ref y, z: ref mut z`,
    /// except `is_shorthand` is true
    pub struct FieldPat {
        /// The identifier for the field
        pub ident: Ident,
        /// The pattern the field is destructured to
        pub pat: Box<Pat>,
        pub is_shorthand: bool,
        pub colon_token: Option<tokens::Colon>,
        pub attrs: Vec<Attribute>,
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum BindingMode {
        ByRef(tokens::Ref, Mutability),
        ByValue(Mutability),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use {BinOp, FnArg, FnDecl, FunctionRetTy, Ident, Lifetime, Mac,
         TokenTree, Ty, UnOp, Unsafety, ArgCaptured, TyInfer};
    use attr::parsing::outer_attr;
    use generics::parsing::lifetime;
    use ident::parsing::{ident, wordlike};
    use item::parsing::item;
    use lit::parsing::lit;
    use mac::parsing::{mac, token_stream};
    use synom::IResult::{self, Error};
    use op::parsing::{assign_op, binop, unop};
    use ty::parsing::{mutability, path, qpath, ty, unsafety};
    use synom::{self, IResult};

    use proc_macro2::{self, TokenKind, Delimiter};

    // Struct literals are ambiguous in certain positions
    // https://github.com/rust-lang/rfcs/pull/92
    macro_rules! named_ambiguous_expr {
        ($name:ident -> $o:ty, $allow_struct:ident, $submac:ident!( $($args:tt)* )) => {
            fn $name(i: &[$crate::synom::TokenTree], $allow_struct: bool)
                     -> $crate::synom::IResult<&[$crate::synom::TokenTree], $o> {
                $submac!(i, $($args)*)
            }
        };
    }

    macro_rules! ambiguous_expr {
        ($i:expr, $allow_struct:ident) => {
            ambiguous_expr($i, $allow_struct, true)
        };
    }

    named!(pub expr -> Expr, ambiguous_expr!(true));

    named!(expr_no_struct -> Expr, ambiguous_expr!(false));

    #[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]
    fn ambiguous_expr(i: &[synom::TokenTree],
                      allow_struct: bool,
                      allow_block: bool)
                      -> IResult<&[synom::TokenTree], Expr> {
        do_parse!(
            i,
            mut e: alt!(
                expr_lit // must be before expr_struct
                |
                cond_reduce!(allow_struct, expr_struct) // must be before expr_path
                |
                expr_paren // must be before expr_tup
                |
                expr_mac // must be before expr_path
                |
                call!(expr_break, allow_struct) // must be before expr_path
                |
                expr_continue // must be before expr_path
                |
                call!(expr_ret, allow_struct) // must be before expr_path
                |
                call!(expr_box, allow_struct)
                |
                expr_in_place
                |
                expr_array
                |
                expr_tup
                |
                call!(expr_unary, allow_struct)
                |
                expr_if
                |
                expr_while
                |
                expr_for_loop
                |
                expr_loop
                |
                expr_match
                |
                expr_catch
                |
                call!(expr_closure, allow_struct)
                |
                cond_reduce!(allow_block, expr_block)
                |
                call!(expr_range, allow_struct)
                |
                expr_path
                |
                call!(expr_addr_of, allow_struct)
                |
                expr_repeat
            ) >>
            many0!(alt!(
                tap!(args: and_call => {
                    let (args, paren) = args;
                    e = ExprCall {
                        func: Box::new(e.into()),
                        args: args,
                        paren_token: paren,
                    }.into();
                })
                |
                tap!(more: and_method_call => {
                    let mut call = more;
                    call.expr = Box::new(e.into());
                    e = call.into();
                })
                |
                tap!(more: call!(and_binary, allow_struct) => {
                    let (op, other) = more;
                    e = ExprBinary {
                        op: op,
                        left: Box::new(e.into()),
                        right: Box::new(other),
                    }.into();
                })
                |
                tap!(ty: and_cast => {
                    let (ty, token) = ty;
                    e = ExprCast {
                        expr: Box::new(e.into()),
                        ty: Box::new(ty),
                        as_token: token,
                    }.into();
                })
                |
                tap!(ty: and_ascription => {
                    let (ty, token) = ty;
                    e = ExprType {
                        expr: Box::new(e.into()),
                        ty: Box::new(ty),
                        colon_token: token,
                    }.into();
                })
                |
                tap!(v: call!(and_assign, allow_struct) => {
                    let (v, token) = v;
                    e = ExprAssign {
                        left: Box::new(e.into()),
                        eq_token: token,
                        right: Box::new(v),
                    }.into();
                })
                |
                tap!(more: call!(and_assign_op, allow_struct) => {
                    let (op, v) = more;
                    e = ExprAssignOp {
                        op: op,
                        left: Box::new(e.into()),
                        right: Box::new(v),
                    }.into();
                })
                |
                tap!(field: and_field => {
                    let (field, token) = field;
                    e = ExprField {
                        expr: Box::new(e.into()),
                        field: field,
                        dot_token: token,
                    }.into();
                })
                |
                tap!(field: and_tup_field => {
                    let (field, token) = field;
                    e = ExprTupField {
                        expr: Box::new(e.into()),
                        field: field,
                        dot_token: token,
                    }.into();
                })
                |
                tap!(i: and_index => {
                    let (i, token) = i;
                    e = ExprIndex {
                        expr: Box::new(e.into()),
                        bracket_token: token,
                        index: Box::new(i),
                    }.into();
                })
                |
                tap!(more: call!(and_range, allow_struct) => {
                    let (limits, hi) = more;
                    e = ExprRange {
                        from: Some(Box::new(e.into())),
                        to: hi.map(Box::new),
                        limits: limits,
                    }.into();
                })
                |
                tap!(_try: punct!("?") => {
                    e = ExprTry {
                        expr: Box::new(e.into()),
                        question_token: tokens::Question::default(),
                    }.into();
                })
            )) >>
            (e.into())
        )
    }

    named!(expr_mac -> ExprKind, map!(mac, ExprKind::Mac));

    named!(expr_paren -> ExprKind, do_parse!(
        punct!("(") >>
        e: expr >>
        punct!(")") >>
        (ExprParen {
            expr: Box::new(e),
            paren_token: tokens::Paren::default(),
        }.into())
    ));

    named_ambiguous_expr!(expr_box -> ExprKind, allow_struct, do_parse!(
        keyword!("box") >>
        inner: ambiguous_expr!(allow_struct) >>
        (ExprBox {
            expr: Box::new(inner),
            box_token: tokens::Box::default(),
        }.into())
    ));

    named!(expr_in_place -> ExprKind, do_parse!(
        keyword!("in") >>
        place: expr_no_struct >>
        punct!("{") >>
        value: within_block >>
        punct!("}") >>
        (ExprInPlace {
            in_token: tokens::In::default(),
            place: Box::new(place),
            value: Box::new(Expr {
                node: ExprBlock {
                    unsafety: Unsafety::Normal,
                    block: Block {
                        stmts: value,
                        brace_token: tokens::Brace::default(),
                    },
                }.into(),
                attrs: Vec::new(),
            }),
        }.into())
    ));

    named!(expr_array -> ExprKind, do_parse!(
        punct!("[") >>
        elems: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                expr) >>
        punct!("]") >>
        (ExprArray {
            exprs: elems,
            bracket_token: tokens::Bracket::default(),
        }.into())
    ));

    named!(and_call -> (Delimited<Expr, tokens::Comma>, tokens::Paren), do_parse!(
        punct!("(") >>
        args: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                               expr) >>
        punct!(")") >>
        (args, tokens::Paren::default())
    ));

    named!(and_method_call -> ExprMethodCall, do_parse!(
        punct!(".") >>
        method: ident >>
        typarams: option!(do_parse!(
            punct!("::") >>
            punct!("<") >>
            tys: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                  ty) >>
            punct!(">") >>
            (tys)
        )) >>
        punct!("(") >>
        args: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                               expr) >>
        punct!(")") >>
        (ExprMethodCall {
            // this expr will get overwritten after being returned
            expr: Box::new(ExprKind::Lit(Lit {
                span: Span::default(),
                value: LitKind::Bool(false),
            }).into()),

            method: method,
            args: args,
            paren_token: tokens::Paren::default(),
            dot_token: tokens::Dot::default(),
            lt_token: typarams.as_ref().map(|_| tokens::Lt::default()),
            gt_token: typarams.as_ref().map(|_| tokens::Gt::default()),
            colon2_token: typarams.as_ref().map(|_| tokens::Colon2::default()),
            typarams: typarams.unwrap_or_default(),
        })
    ));

    named!(expr_tup -> ExprKind, do_parse!(
        punct!("(") >>
        elems: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                expr) >>
        punct!(")") >>
        (ExprTup {
            args: elems,
            paren_token: tokens::Paren::default(),
            lone_comma: None, // TODO: parse this
        }.into())
    ));

    named_ambiguous_expr!(and_binary -> (BinOp, Expr), allow_struct, tuple!(
        binop,
        ambiguous_expr!(allow_struct)
    ));

    named_ambiguous_expr!(expr_unary -> ExprKind, allow_struct, do_parse!(
        operator: unop >>
        operand: ambiguous_expr!(allow_struct) >>
        (ExprUnary { op: operator, expr: Box::new(operand) }.into())
    ));

    named!(expr_lit -> ExprKind, map!(lit, ExprKind::Lit));

    named!(and_cast -> (Ty, tokens::As), do_parse!(
        keyword!("as") >>
        ty: ty >>
        (ty, tokens::As::default())
    ));

    named!(and_ascription -> (Ty, tokens::Colon),
           preceded!(punct!(":"), map!(ty, |t| (t, tokens::Colon::default()))));

    enum Cond {
        Let(Pat, Expr, tokens::Eq, tokens::Let),
        Expr(Expr),
    }

    named!(cond -> Cond, alt!(
        do_parse!(
            keyword!("let") >>
            pat: pat >>
            punct!("=") >>
            value: expr_no_struct >>
            (Cond::Let(pat, value, tokens::Eq::default(), tokens::Let::default()))
        )
        |
        map!(expr_no_struct, Cond::Expr)
    ));

    named!(expr_if -> ExprKind, do_parse!(
        keyword!("if") >>
        cond: cond >>
        then_block: delim!(Brace, within_block) >>
        else_block: option!(preceded!(
            keyword!("else"),
            alt!(
                expr_if
                |
                do_parse!(
                    punct!("{") >>
                    else_block: within_block >>
                    punct!("}") >>
                    (ExprKind::Block(ExprBlock {
                        unsafety: Unsafety::Normal,
                        block: Block {
                            stmts: else_block,
                            brace_token: tokens::Brace::default(),
                        },
                    }).into())
                )
            )
        )) >>
        (match cond {
            Cond::Let(pat, expr, eq_token, let_token) => ExprIfLet {
                pat: Box::new(pat),
                expr: Box::new(expr),
                eq_token: eq_token,
                let_token: let_token,
                if_true: Block {
                    stmts: then_block,
                    brace_token: tokens::Brace::default(),
                },
                if_token: tokens::If::default(),
                else_token: else_block.as_ref().map(|_| tokens::Else::default()),
                if_false: else_block.map(|els| Box::new(els.into())),
            }.into(),
            Cond::Expr(cond) => ExprIf {
                cond: Box::new(cond),
                if_true: Block {
                    stmts: then_block,
                    brace_token: tokens::Brace::default(),
                },
                if_token: tokens::If::default(),
                else_token: else_block.as_ref().map(|_| tokens::Else::default()),
                if_false: else_block.map(|els| Box::new(els.into())),
            }.into(),
        })
    ));

    named!(expr_for_loop -> ExprKind, do_parse!(
        lbl: option!(terminated!(label, punct!(":"))) >>
        keyword!("for") >>
        pat: pat >>
        keyword!("in") >>
        expr: expr_no_struct >>
        loop_block: block >>
        (ExprForLoop {
            for_token: tokens::For::default(),
            in_token: tokens::In::default(),
            colon_token: lbl.as_ref().map(|_| tokens::Colon::default()),
            pat: Box::new(pat),
            expr: Box::new(expr),
            body: loop_block,
            label: lbl,
        }.into())
    ));

    named!(expr_loop -> ExprKind, do_parse!(
        lbl: option!(terminated!(label, punct!(":"))) >>
        keyword!("loop") >>
        loop_block: block >>
        (ExprLoop {
            loop_token: tokens::Loop::default(),
            colon_token: lbl.as_ref().map(|_| tokens::Colon::default()),
            body: loop_block,
            label: lbl,
        }.into())
    ));

    named!(expr_match -> ExprKind, do_parse!(
        keyword!("match") >>
        obj: expr_no_struct >>
        res: delim!(Brace, do_parse!(
            mut arms: many0!(do_parse!(
                arm: match_arm >>
                    cond!(arm_requires_comma(&arm), punct!(",")) >>
                    cond!(!arm_requires_comma(&arm), option!(punct!(","))) >>
                    (arm)
            )) >>
            last_arm: option!(match_arm) >>
            (ExprKind::Match(Box::new(obj), {
                arms.extend(last_arm);
                arms
            }))
        )) >>
        last_arm: option!(match_arm) >>
        punct!("}") >>
        (ExprMatch {
            expr: Box::new(obj),
            match_token: tokens::Match::default(),
            brace_token: tokens::Brace::default(),
            arms: {
                for arm in &mut arms {
                    if arm_requires_comma(arm) {
                        arm.comma = Some(tokens::Comma::default());
                    }
                }
                arms.extend(last_arm);
                arms
            },
        }.into())
    ));

    named!(expr_catch -> ExprKind, do_parse!(
        keyword!("do") >>
        keyword!("catch") >>
        catch_block: block >>
        (ExprCatch {
            block: catch_block,
            do_token: tokens::Do::default(),
            catch_token: tokens::Catch::default(),
        }.into())
    ));

    fn arm_requires_comma(arm: &Arm) -> bool {
        if let ExprKind::Block(ExprBlock { unsafety: Unsafety::Normal, .. }) = arm.body.node {
            false
        } else {
            true
        }
    }

    named!(match_arm -> Arm, do_parse!(
        attrs: many0!(outer_attr) >>
        pats: separated_nonempty_list!(map!(punct!("|"), |_| tokens::Or::default()),
                                       pat) >>
        guard: option!(preceded!(keyword!("if"), expr)) >>
        punct!("=>") >>
        body: alt!(
            map!(block, |blk| {
                ExprKind::Block(ExprBlock {
                    unsafety: Unsafety::Normal,
                    block: blk,
                }).into()
            })
            |
            expr
        ) >>
        (Arm {
            rocket_token: tokens::Rocket::default(),
            if_token: guard.as_ref().map(|_| tokens::If::default()),
            attrs: attrs,
            pats: pats,
            guard: guard.map(Box::new),
            body: Box::new(body),
            comma: None,
        })
    ));

    named_ambiguous_expr!(expr_closure -> ExprKind, allow_struct, do_parse!(
        capture: capture_by >>
        punct!("|") >>
        inputs: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 closure_arg) >>
        punct!("|") >>
        ret_and_body: alt!(
            do_parse!(
                punct!("->") >>
                ty: ty >>
                body: block >>
                (FunctionRetTy::Ty(ty, tokens::RArrow::default()),
                 ExprKind::Block(ExprBlock {
                    unsafety: Unsafety::Normal,
                    block: body,
                }).into())
            )
            |
            map!(ambiguous_expr!(allow_struct), |e| (FunctionRetTy::Default, e))
        ) >>
        (ExprClosure {
            capture: capture,
            or1_token: tokens::Or::default(),
            or2_token: tokens::Or::default(),
            decl: Box::new(FnDecl {
                inputs: inputs,
                output: ret_and_body.0,
                variadic: false,
                dot_tokens: None,
                fn_token: tokens::Fn::default(),
                generics: Generics::default(),
                paren_token: tokens::Paren::default(),
            }),
            body: Box::new(ret_and_body.1),
        }.into())
    ));

    named!(closure_arg -> FnArg, do_parse!(
        pat: pat >>
        ty: option!(preceded!(punct!(":"), ty)) >>
        (ArgCaptured {
            pat: pat,
            colon_token: tokens::Colon::default(),
            ty: ty.unwrap_or_else(|| TyInfer {
                underscore_token: tokens::Underscore::default(),
            }.into()),
        }.into())
    ));

    named!(expr_while -> ExprKind, do_parse!(
        lbl: option!(terminated!(label, punct!(":"))) >>
        keyword!("while") >>
        cond: cond >>
        while_block: block >>
        (match cond {
            Cond::Let(pat, expr, eq_token, let_token) => ExprWhileLet {
                eq_token: eq_token,
                let_token: let_token,
                while_token: tokens::While::default(),
                colon_token: lbl.as_ref().map(|_| tokens::Colon::default()),
                pat: Box::new(pat),
                expr: Box::new(expr),
                body: while_block,
                label: lbl,
            }.into(),
            Cond::Expr(cond) => ExprWhile {
                while_token: tokens::While::default(),
                colon_token: lbl.as_ref().map(|_| tokens::Colon::default()),
                cond: Box::new(cond),
                body: while_block,
                label: lbl,
            }.into(),
        })
    ));

    named!(expr_continue -> ExprKind, do_parse!(
        keyword!("continue") >>
        lbl: option!(label) >>
        (ExprContinue {
            continue_token: tokens::Continue::default(),
            label: lbl,
        }.into())
    ));

    named_ambiguous_expr!(expr_break -> ExprKind, allow_struct, do_parse!(
        keyword!("break") >>
        lbl: option!(label) >>
        val: option!(call!(ambiguous_expr, allow_struct, false)) >>
        (ExprBreak {
            label: lbl,
            expr: val.map(Box::new),
            break_token: tokens::Break::default(),
        }.into())
    ));

    named_ambiguous_expr!(expr_ret -> ExprKind, allow_struct, do_parse!(
        keyword!("return") >>
        ret_value: option!(ambiguous_expr!(allow_struct)) >>
        (ExprRet {
            expr: ret_value.map(Box::new),
            return_token: tokens::Return::default(),
        }.into())
    ));

    named!(expr_struct -> ExprKind, do_parse!(
        path: path >>
        punct!("{") >>
        fields: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 field_value) >>
        base: option!(
            cond!(fields.is_empty() || fields.trailing_delim(),
                do_parse!(
                    punct!("..") >>
                    base: expr >>
                    (base)
                )
            )
        ) >>
        punct!("}") >>
        (ExprStruct {
            brace_token: tokens::Brace::default(),
            path: path,
            fields: fields,
            dot2_token: base.as_ref().and_then(|b| b.as_ref())
                            .map(|_| tokens::Dot2::default()),
            rest: base.and_then(|b| b.map(Box::new)),
        }.into())
    ));

    named!(field_value -> FieldValue, alt!(
        do_parse!(
            name: wordlike >>
            punct!(":") >>
            value: expr >>
            (FieldValue {
                ident: name,
                expr: value,
                is_shorthand: false,
                attrs: Vec::new(),
                colon_token: Some(tokens::Colon::default()),
            })
        )
        |
        map!(ident, |name: Ident| FieldValue {
            ident: name.clone(),
            expr: ExprKind::Path(ExprPath { qself: None, path: name.into() }).into(),
            is_shorthand: true,
            attrs: Vec::new(),
            colon_token: None,
        })
    ));

    named!(expr_repeat -> ExprKind, delim!(Bracket, do_parse!(
        value: expr >>
        punct!(";") >>
        times: expr >>
        punct!("]") >>
        (ExprRepeat {
            expr: Box::new(value),
            amt: Box::new(times),
            bracket_token: tokens::Bracket::default(),
            semi_token: tokens::Semi::default(),
        }.into())
    ));

    named!(expr_block -> ExprKind, do_parse!(
        rules: unsafety >>
        b: block >>
        (ExprBlock {
            unsafety: rules,
            block: b,
        }.into())
    ));

    named_ambiguous_expr!(expr_range -> ExprKind, allow_struct, do_parse!(
        limits: range_limits >>
        hi: option!(ambiguous_expr!(allow_struct)) >>
        (ExprRange { from: None, to: hi.map(Box::new), limits: limits }.into())
    ));

    named!(range_limits -> RangeLimits, alt!(
        punct!("...") => { |_| RangeLimits::Closed(tokens::Dot3::default()) }
        |
        punct!("..") => { |_| RangeLimits::HalfOpen(tokens::Dot2::default()) }
    ));

    named!(expr_path -> ExprKind, map!(qpath, |(qself, path)| {
        ExprPath { qself: qself, path: path }.into()
    }));

    named_ambiguous_expr!(expr_addr_of -> ExprKind, allow_struct, do_parse!(
        punct!("&") >>
        mutability: mutability >>
        expr: ambiguous_expr!(allow_struct) >>
        (ExprAddrOf {
            mutbl: mutability,
            expr: Box::new(expr),
            and_token: tokens::And::default(),
        }.into())
    ));

    named_ambiguous_expr!(and_assign -> (Expr, tokens::Eq), allow_struct, preceded!(
        punct!("="),
        map!(ambiguous_expr!(allow_struct), |t| (t, tokens::Eq::default()))
    ));

    named_ambiguous_expr!(and_assign_op -> (BinOp, Expr), allow_struct, tuple!(
        assign_op,
        ambiguous_expr!(allow_struct)
    ));

    named!(and_field -> (Ident, tokens::Dot),
           preceded!(punct!("."), map!(ident, |t| (t, tokens::Dot::default()))));

    named!(and_tup_field -> (Lit, tokens::Dot),
           preceded!(punct!("."), map!(lit, |l| (l, tokens::Dot::default()))));

    named!(and_index -> (Expr, tokens::Bracket),
           map!(delimited!(punct!("["), expr, punct!("]")),
                |t| (t, tokens::Bracket::default())));

    named_ambiguous_expr!(and_range -> (RangeLimits, Option<Expr>), allow_struct, tuple!(
        range_limits,
        option!(call!(ambiguous_expr, allow_struct, false))
    ));

    named!(pub block -> Block, do_parse!(
        stmts: delim!(Brace, within_block) >>
        (Block {
            stmts: stmts,
            brace_token: tokens::Brace::default(),
        })
    ));

    named!(pub within_block -> Vec<Stmt>, do_parse!(
        many0!(punct!(";")) >>
        mut standalone: many0!(terminated!(stmt, many0!(punct!(";")))) >>
        last: option!(expr) >>
        (match last {
            None => standalone,
            Some(last) => {
                standalone.push(Stmt::Expr(Box::new(last)));
                standalone
            }
        })
    ));

    named!(pub stmt -> Stmt, alt!(
        stmt_mac
        |
        stmt_local
        |
        stmt_item
        |
        stmt_expr
    ));

    named!(stmt_mac -> Stmt, do_parse!(
        attrs: many0!(outer_attr) >>
        what: path >>
        punct!("!") >>
    // Only parse braces here; paren and bracket will get parsed as
    // expression statements
        punct!("{") >>
        ts: token_stream >>
        punct!("}") >>
        semi: option!(punct!(";")) >>
        (Stmt::Mac(Box::new((
            Mac {
                path: what,
                bang_token: tokens::Bang::default(),
                tokens: vec![TokenTree(proc_macro2::TokenTree {
                    span: Default::default(),
                    kind: TokenKind::Sequence(Delimiter::Brace, ts),
                })],
            },
            if semi.is_some() {
                MacStmtStyle::Semicolon(tokens::Semi::default())
            } else {
                MacStmtStyle::Braces
            },
            attrs,
        ))))
    ));

    named!(stmt_local -> Stmt, do_parse!(
        attrs: many0!(outer_attr) >>
        keyword!("let") >>
        pat: pat >>
        ty: option!(preceded!(punct!(":"), ty)) >>
        init: option!(preceded!(punct!("="), expr)) >>
        punct!(";") >>
        (Stmt::Local(Box::new(Local {
            let_token: tokens::Let::default(),
            semi_token: tokens::Semi::default(),
            colon_token: ty.as_ref().map(|_| tokens::Colon::default()),
            eq_token: init.as_ref().map(|_| tokens::Eq::default()),
            pat: Box::new(pat),
            ty: ty.map(Box::new),
            init: init.map(Box::new),
            attrs: attrs,
        })))
    ));

    named!(stmt_item -> Stmt, map!(item, |i| Stmt::Item(Box::new(i))));

    fn requires_semi(e: &Expr) -> bool {
        match e.node {
            ExprKind::If(_) |
            ExprKind::IfLet(_) |
            ExprKind::While(_) |
            ExprKind::WhileLet(_) |
            ExprKind::ForLoop(_) |
            ExprKind::Loop(_) |
            ExprKind::Match(_) |
            ExprKind::Block(_) => false,

            _ => true,
        }
    }

    named!(stmt_expr -> Stmt, do_parse!(
        attrs: many0!(outer_attr) >>
        mut e: expr >>
        semi: option!(punct!(";")) >>
        ({
            e.attrs = attrs;
            if semi.is_some() {
                Stmt::Semi(Box::new(e), tokens::Semi::default())
            } else if requires_semi(&e) {
                return IResult::Error;
            } else {
                Stmt::Expr(Box::new(e))
            }
        })
    ));

    named!(pub pat -> Pat, alt!(
        pat_wild // must be before pat_ident
        |
        pat_box // must be before pat_ident
        |
        pat_range // must be before pat_lit
        |
        pat_tuple_struct // must be before pat_ident
        |
        pat_struct // must be before pat_ident
        |
        pat_mac // must be before pat_ident
        |
        pat_lit // must be before pat_ident
        |
        pat_ident // must be before pat_path
        |
        pat_path
        |
        map!(pat_tuple, |t: PatTuple| t.into())
        |
        pat_ref
        |
        pat_slice
    ));

    named!(pat_mac -> Pat, map!(mac, Pat::Mac));

    named!(pat_wild -> Pat, map!(keyword!("_"), |_| {
        PatWild { underscore_token: tokens::Underscore::default() }.into()
    }));

    named!(pat_box -> Pat, do_parse!(
        keyword!("box") >>
        pat: pat >>
        (PatBox {
            pat: Box::new(pat),
            box_token: tokens::Box::default(),
        }.into())
    ));

    named!(pat_ident -> Pat, do_parse!(
        mode: option!(keyword!("ref")) >>
        mutability: mutability >>
        name: alt!(
            ident
            |
            keyword!("self") => { Into::into }
        ) >>
        not!(punct!("<")) >>
        not!(punct!("::")) >>
        subpat: option!(preceded!(punct!("@"), pat)) >>
        (PatIdent {
            mode: if mode.is_some() {
                BindingMode::ByRef(tokens::Ref::default(), mutability)
            } else {
                BindingMode::ByValue(mutability)
            },
            ident: name,
            at_token: subpat.as_ref().map(|_| tokens::At::default()),
            subpat: subpat.map(Box::new),
        }.into())
    ));

    named!(pat_tuple_struct -> Pat, do_parse!(
        path: path >>
        tuple: pat_tuple >>
        (PatTupleStruct {
            path: path,
            pat: tuple,
        }.into())
    ));

    named!(pat_struct -> Pat, do_parse!(
        path: path >>
        punct!("{") >>
        fields: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 field_pat) >>
        base: option!(
            cond!(fields.is_empty() || fields.trailing_delim(),
                  punct!(".."))
        ) >>
        punct!("}") >>
        (PatStruct {
            path: path,
            fields: fields,
            brace_token: tokens::Brace::default(),
            dot2_token: base.and_then(|m| m).map(|_| tokens::Dot2::default()),
        }.into())
    ));

    named!(field_pat -> FieldPat, alt!(
        do_parse!(
            ident: wordlike >>
            punct!(":") >>
            pat: pat >>
            (FieldPat {
                ident: ident,
                pat: Box::new(pat),
                is_shorthand: false,
                attrs: Vec::new(),
                colon_token: Some(tokens::Colon::default()),
            })
        )
        |
        do_parse!(
            boxed: option!(keyword!("box")) >>
            mode: option!(keyword!("ref")) >>
            mutability: mutability >>
            ident: ident >>
            ({
                let mut pat: Pat = PatIdent {
                    mode: if mode.is_some() {
                        BindingMode::ByRef(tokens::Ref::default(), mutability)
                    } else {
                        BindingMode::ByValue(mutability)
                    },
                    ident: ident.clone(),
                    subpat: None,
                    at_token: None,
                }.into();
                if boxed.is_some() {
                    pat = PatBox {
                        pat: Box::new(pat),
                        box_token: tokens::Box::default(),
                    }.into();
                }
                FieldPat {
                    ident: ident,
                    pat: Box::new(pat),
                    is_shorthand: true,
                    attrs: Vec::new(),
                    colon_token: None,
                }
            })
        )
    ));

    named!(pat_path -> Pat, map!(qpath, |(qself, path)| {
        PatPath { qself: qself, path: path }.into()
    }));

    named!(pat_tuple -> PatTuple, do_parse!(
        punct!("(") >>
        mut elems: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                    pat) >>
        dotdot: map!(cond!(
            elems.is_empty() || elems.trailing_delim(),
            option!(do_parse!(
                punct!("..") >>
                trailing: option!(punct!(",")) >>
                (trailing.is_some())
            ))
        ), |x: Option<_>| x.and_then(|x| x)) >>
        rest: cond!(dotdot == Some(true),
                    terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                     pat)) >>
        punct!(")") >>
        (PatTuple {
            paren_token: tokens::Paren::default(),
            dots_pos: dotdot.map(|_| elems.len()),
            dot2_token: dotdot.map(|_| tokens::Dot2::default()),
            comma_token: dotdot.and_then(|b| {
                if b {
                    Some(tokens::Comma::default())
                } else {
                    None
                }
            }),
            pats: {
                if let Some(rest) = rest {
                    for elem in rest.into_iter() {
                        elems.push(elem);
                    }
                }
                elems
            },
        })
    )));

    named!(pat_ref -> Pat, do_parse!(
        punct!("&") >>
        mutability: mutability >>
        pat: pat >>
        (PatRef {
            pat: Box::new(pat),
            mutbl: mutability,
            and_token: tokens::And::default(),
        }.into())
    ));

    named!(pat_lit -> Pat, do_parse!(
        lit: pat_lit_expr >>
        (if let ExprKind::Path(_) = lit.node {
            return IResult::Error; // these need to be parsed by pat_path
        } else {
            PatLit {
                expr: Box::new(lit),
            }.into()
        })
    ));

    named!(pat_range -> Pat, do_parse!(
        lo: pat_lit_expr >>
        limits: range_limits >>
        hi: pat_lit_expr >>
        (PatRange {
            lo: Box::new(lo),
            hi: Box::new(hi),
            limits: limits,
        }.into())
    ));

    named!(pat_lit_expr -> Expr, do_parse!(
        neg: option!(punct!("-")) >>
        v: alt!(
            lit => { ExprKind::Lit }
            |
            path => { |p| ExprPath { qself: None, path: p }.into() }
        ) >>
        (if neg.is_some() {
            ExprKind::Unary(ExprUnary {
                op: UnOp::Neg(tokens::Sub::default()),
                expr: Box::new(v.into())
            }).into()
        } else {
            v.into()
        })
    ));

    named!(pat_slice -> Pat, do_parse!(
        punct!("[") >>
        mut before: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 pat) >>
        middle: option!(do_parse!(
            punct!("..") >>
            trailing: option!(punct!(",")) >>
            (trailing.is_some())
        )) >>
        after: cond!(
            match middle {
                Some(trailing) => trailing,
                _ => false,
            },
            terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                             pat)
        ) >>
        punct!("]") >>
        (PatSlice {
            dot2_token: middle.as_ref().map(|_| tokens::Dot2::default()),
            comma_token: {
                let trailing = middle.unwrap_or(false);
                if trailing {Some(tokens::Comma::default())} else {None}
            },
            bracket_token: tokens::Bracket::default(),
            middle: middle.and_then(|_| {
                if !before.is_empty() && !before.trailing_delim() {
                    Some(Box::new(before.pop().unwrap().into_item()))
                } else {
                    None
                }
            }),
            front: before,
            back: after.unwrap_or_default(),
        }.into())
    ));

    named!(capture_by -> CaptureBy, alt!(
        keyword!("move") => { |_| CaptureBy::Value(tokens::Move::default()) }
        |
        epsilon!() => { |_| CaptureBy::Ref }
    ));

    named!(label -> Ident, map!(lifetime, |lt: Lifetime| lt.ident));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use attr::FilterAttrs;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Expr {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.node.to_tokens(tokens)
        }
    }

    impl ToTokens for ExprBox {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.box_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprInPlace {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.in_token.to_tokens(tokens);
            self.place.to_tokens(tokens);
            self.value.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprArray {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bracket_token.surround(tokens, |tokens| {
                self.exprs.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprCall {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.func.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprMethodCall {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.method.to_tokens(tokens);
            self.colon2_token.to_tokens(tokens);
            self.lt_token.to_tokens(tokens);
            self.typarams.to_tokens(tokens);
            self.gt_token.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ExprTup {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
                self.lone_comma.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprBinary {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprUnary {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.op.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprCast {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.as_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprType {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprIf {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.if_token.to_tokens(tokens);
            self.cond.to_tokens(tokens);
            self.if_true.to_tokens(tokens);
            self.else_token.to_tokens(tokens);
            self.if_false.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprIfLet {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.if_token.to_tokens(tokens);
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
            self.if_true.to_tokens(tokens);
            self.else_token.to_tokens(tokens);
            self.if_false.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprWhile {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.label.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.while_token.to_tokens(tokens);
            self.cond.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprWhileLet {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.label.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.while_token.to_tokens(tokens);
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprForLoop {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.label.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.for_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.in_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprLoop {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.label.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.loop_token.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprMatch {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.match_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.arms);
            });
        }
    }

    impl ToTokens for ExprCatch {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.do_token.to_tokens(tokens);
            self.catch_token.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprClosure {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.capture.to_tokens(tokens);
            self.or1_token.to_tokens(tokens);
            for item in self.decl.inputs.iter() {
                match **item.item() {
                    FnArg::Captured(ArgCaptured { ref pat, ty: Ty::Infer(_), .. }) => {
                        pat.to_tokens(tokens);
                    }
                    _ => item.item().to_tokens(tokens),
                }
                item.delimiter().to_tokens(tokens);
            }
            self.or2_token.to_tokens(tokens);
            self.decl.output.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprBlock {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.unsafety.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprAssign {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.left.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprAssignOp {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprField {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.field.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprTupField {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.field.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprIndex {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.bracket_token.surround(tokens, |tokens| {
                self.index.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ExprRange {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.from.to_tokens(tokens);
            self.limits.to_tokens(tokens);
            self.to.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprPath {
        fn to_tokens(&self, tokens: &mut Tokens) {
            ::PathTokens(&self.qself, &self.path).to_tokens(tokens)
        }
    }

    impl ToTokens for ExprAddrOf {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.and_token.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprBreak {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.break_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprContinue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.continue_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprRet {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.return_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprStruct {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
                self.dot2_token.to_tokens(tokens);
                self.rest.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprRepeat {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bracket_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
                self.amt.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprParen {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ExprTry {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.question_token.to_tokens(tokens);
        }
    }

    impl ToTokens for FieldValue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            if !self.is_shorthand {
                self.colon_token.to_tokens(tokens);
                self.expr.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for Arm {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(&self.attrs);
            self.pats.to_tokens(tokens);
            self.if_token.to_tokens(tokens);
            self.guard.to_tokens(tokens);
            self.rocket_token.to_tokens(tokens);
            self.body.to_tokens(tokens);
            self.comma.to_tokens(tokens);
        }
    }

    impl ToTokens for PatWild {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.underscore_token.to_tokens(tokens);
        }
    }

    impl ToTokens for PatIdent {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.mode.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.at_token.to_tokens(tokens);
            self.subpat.to_tokens(tokens);
        }
    }

    impl ToTokens for PatStruct {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
                self.dot2_token.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for PatTupleStruct {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    impl ToTokens for PatPath {
        fn to_tokens(&self, tokens: &mut Tokens) {
            ::PathTokens(&self.qself, &self.path).to_tokens(tokens);
        }
    }

    impl ToTokens for PatTuple {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                for (i, token) in self.pats.iter().enumerate() {
                    if Some(i) == self.dots_pos {
                        self.dot2_token.to_tokens(tokens);
                        self.comma_token.to_tokens(tokens);
                    }
                    token.to_tokens(tokens);
                }

                if Some(self.pats.len()) == self.dots_pos {
                    self.dot2_token.to_tokens(tokens);
                }
            });
        }
    }

    impl ToTokens for PatBox {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.box_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    impl ToTokens for PatRef {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.and_token.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    impl ToTokens for PatLit {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for PatRange {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.lo.to_tokens(tokens);
            self.limits.to_tokens(tokens);
            self.hi.to_tokens(tokens);
        }
    }

    impl ToTokens for PatSlice {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bracket_token.surround(tokens, |tokens| {
                self.front.to_tokens(tokens);
                self.middle.to_tokens(tokens);
                self.dot2_token.to_tokens(tokens);
                self.comma_token.to_tokens(tokens);
                self.back.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for RangeLimits {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                RangeLimits::HalfOpen(ref t) => t.to_tokens(tokens),
                RangeLimits::Closed(ref t) => t.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for FieldPat {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if !self.is_shorthand {
                self.ident.to_tokens(tokens);
                self.colon_token.to_tokens(tokens);
            }
            self.pat.to_tokens(tokens);
        }
    }

    impl ToTokens for BindingMode {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                BindingMode::ByRef(ref t, ref m) => {
                    t.to_tokens(tokens);
                    m.to_tokens(tokens);
                }
                BindingMode::ByValue(ref m) => {
                    m.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for CaptureBy {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                CaptureBy::Value(ref t) => t.to_tokens(tokens),
                CaptureBy::Ref => {
                    // nothing
                }
            }
        }
    }

    impl ToTokens for Block {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.stmts);
            });
        }
    }

    impl ToTokens for Stmt {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Stmt::Local(ref local) => local.to_tokens(tokens),
                Stmt::Item(ref item) => item.to_tokens(tokens),
                Stmt::Expr(ref expr) => expr.to_tokens(tokens),
                Stmt::Semi(ref expr, ref semi) => {
                    expr.to_tokens(tokens);
                    semi.to_tokens(tokens);
                }
                Stmt::Mac(ref mac) => {
                    let (ref mac, ref style, ref attrs) = **mac;
                    tokens.append_all(attrs.outer());
                    mac.to_tokens(tokens);
                    match *style {
                        MacStmtStyle::Semicolon(ref s) => s.to_tokens(tokens),
                        MacStmtStyle::Braces | MacStmtStyle::NoBraces => {
                            // no semicolon
                        }
                    }
                }
            }
        }
    }

    impl ToTokens for Local {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.init.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }
}
