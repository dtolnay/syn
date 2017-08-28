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
        pub Box(ExprBox #full {
            pub expr: Box<Expr>,
            pub box_token: tokens::Box_,
        }),

        /// E.g. 'place <- val' or `in place { val }`.
        pub InPlace(ExprInPlace #full {
            pub place: Box<Expr>,
            pub kind: InPlaceKind,
            pub value: Box<Expr>,
        }),

        /// An array, e.g. `[a, b, c, d]`.
        pub Array(ExprArray #full {
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
        pub MethodCall(ExprMethodCall #full {
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
        pub Tup(ExprTup #full {
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
        pub If(ExprIf #full {
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
        pub IfLet(ExprIfLet #full {
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
        pub While(ExprWhile #full {
            pub cond: Box<Expr>,
            pub body: Block,
            pub label: Option<Lifetime>,
            pub colon_token: Option<tokens::Colon>,
            pub while_token: tokens::While,
        }),

        /// A while-let loop, with an optional label.
        ///
        /// E.g., `'label: while let pat = expr { block }`
        ///
        /// This is desugared to a combination of `loop` and `match` expressions.
        pub WhileLet(ExprWhileLet #full {
            pub pat: Box<Pat>,
            pub expr: Box<Expr>,
            pub body: Block,
            pub label: Option<Lifetime>,
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
        pub ForLoop(ExprForLoop #full {
            pub pat: Box<Pat>,
            pub expr: Box<Expr>,
            pub body: Block,
            pub label: Option<Lifetime>,
            pub for_token: tokens::For,
            pub colon_token: Option<tokens::Colon>,
            pub in_token: tokens::In,
        }),

        /// Conditionless loop with an optional label.
        ///
        /// E.g. `'label: loop { block }`
        pub Loop(ExprLoop #full {
            pub body: Block,
            pub label: Option<Lifetime>,
            pub loop_token: tokens::Loop,
            pub colon_token: Option<tokens::Colon>,
        }),

        /// A `match` block.
        pub Match(ExprMatch #full {
            pub match_token: tokens::Match,
            pub brace_token: tokens::Brace,
            pub expr: Box<Expr>,
            pub arms: Vec<Arm>,
        }),

        /// A closure (for example, `move |a, b, c| a + b + c`)
        pub Closure(ExprClosure #full {
            pub capture: CaptureBy,
            pub decl: Box<FnDecl>,
            pub body: Box<Expr>,
            pub or1_token: tokens::Or,
            pub or2_token: tokens::Or,
        }),

        /// A block (`{ ... }` or `unsafe { ... }`)
        pub Block(ExprBlock #full {
            pub unsafety: Unsafety,
            pub block: Block,
        }),

        /// An assignment (`a = foo()`)
        pub Assign(ExprAssign #full {
            pub left: Box<Expr>,
            pub right: Box<Expr>,
            pub eq_token: tokens::Eq,
        }),

        /// An assignment with an operator
        ///
        /// For example, `a += 1`.
        pub AssignOp(ExprAssignOp #full {
            pub op: BinOp,
            pub left: Box<Expr>,
            pub right: Box<Expr>,
        }),

        /// Access of a named struct field (`obj.foo`)
        pub Field(ExprField #full {
            pub expr: Box<Expr>,
            pub field: Ident,
            pub dot_token: tokens::Dot,
        }),

        /// Access of an unnamed field of a struct or tuple-struct
        ///
        /// For example, `foo.0`.
        pub TupField(ExprTupField #full {
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
        pub Range(ExprRange #full {
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
        pub AddrOf(ExprAddrOf #full {
            pub and_token: tokens::And,
            pub mutbl: Mutability,
            pub expr: Box<Expr>,
        }),

        /// A `break`, with an optional label to break, and an optional expression
        pub Break(ExprBreak #full {
            pub label: Option<Lifetime>,
            pub expr: Option<Box<Expr>>,
            pub break_token: tokens::Break,
        }),

        /// A `continue`, with an optional label
        pub Continue(ExprContinue #full {
            pub label: Option<Lifetime>,
            pub continue_token: tokens::Continue,
        }),

        /// A `return`, with an optional value to be returned
        pub Ret(ExprRet #full {
            pub expr: Option<Box<Expr>>,
            pub return_token: tokens::Return,
        }),

        /// A macro invocation; pre-expansion
        pub Mac(Mac),

        /// A struct literal expression.
        ///
        /// For example, `Foo {x: 1, y: 2}`, or
        /// `Foo {x: 1, .. base}`, where `base` is the `Option<Expr>`.
        pub Struct(ExprStruct #full {
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
        pub Repeat(ExprRepeat #full {
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

        /// No-op: used solely so we can pretty-print faithfully
        ///
        /// A `group` represents a `None`-delimited span in the input
        /// `TokenStream` which affects the precidence of the resulting
        /// expression. They are used for macro hygiene.
        pub Group(ExprGroup {
            pub expr: Box<Expr>,
            pub group_token: tokens::Group,
        }),

        /// `expr?`
        pub Try(ExprTry #full {
            pub expr: Box<Expr>,
            pub question_token: tokens::Question,
        }),

        /// A catch expression.
        ///
        /// E.g. `do catch { block }`
        pub Catch(ExprCatch #full {
            pub do_token: tokens::Do,
            pub catch_token: tokens::Catch,
            pub block: Block,
        }),

        /// A yield expression.
        ///
        /// E.g. `yield expr`
        pub Yield(ExprYield #full {
            pub yield_token: tokens::Yield,
            pub expr: Option<Box<Expr>>,
        }),
    }
}

#[cfg(feature = "full")]
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

#[cfg(feature = "full")]
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

#[cfg(feature = "full")]
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

#[cfg(feature = "full")]
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

#[cfg(feature = "full")]
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

#[cfg(feature = "full")]
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
            pub box_token: tokens::Box_,
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
        /// `[a, b, i.., y, z]` is represented as:
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

#[cfg(feature = "full")]
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

#[cfg(feature = "full")]
ast_enum! {
    /// A capture clause
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum CaptureBy {
        Value(tokens::Move),
        Ref,
    }
}

#[cfg(feature = "full")]
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

#[cfg(feature = "full")]
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

#[cfg(feature = "full")]
ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum BindingMode {
        ByRef(tokens::Ref, Mutability),
        ByValue(Mutability),
    }
}

#[cfg(feature = "full")]
ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum InPlaceKind {
        Arrow(tokens::LArrow),
        In(tokens::In),
    }
}

#[cfg(any(feature = "parsing", feature = "printing"))]
#[cfg(feature = "full")]
fn arm_expr_requires_comma(expr: &Expr) -> bool {
    // see https://github.com/rust-lang/rust/blob/eb8f2586e
    //                       /src/libsyntax/parse/classify.rs#L17-L37
    match expr.node {
        ExprKind::Block(..) |
        ExprKind::If(..) |
        ExprKind::IfLet(..) |
        ExprKind::Match(..) |
        ExprKind::While(..) |
        ExprKind::WhileLet(..) |
        ExprKind::Loop(..) |
        ExprKind::ForLoop(..) |
        ExprKind::Catch(..) => false,
        _ => true,
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use ty::parsing::qpath;

    #[cfg(feature = "full")]
    use proc_macro2::{TokenStream, TokenNode, Delimiter, Term};
    use synom::{PResult, Cursor, Synom};
    #[cfg(feature = "full")]
    use synom::parse_error;
    use synom::tokens::*;

    /// When we're parsing expressions which occur before blocks, like in
    /// an if statement's condition, we cannot parse a struct literal.
    ///
    /// Struct literals are ambiguous in certain positions
    /// https://github.com/rust-lang/rfcs/pull/92
    macro_rules! ambiguous_expr {
        ($i:expr, $allow_struct:ident) => {
            ambiguous_expr($i, $allow_struct, true)
        };
    }

    /// When we are parsing an optional suffix expression, we cannot allow
    /// blocks if structs are not allowed.
    ///
    /// Example:
    /// ```ignore
    /// if break { } { }
    /// // is ambiguous between:
    /// if (break { }) { }
    /// // - or -
    /// if (break) { } { }
    /// ```
    #[cfg(feature = "full")]
    macro_rules! opt_ambiguous_expr {
        ($i:expr, $allow_struct:ident) => {
            option!($i, call!(ambiguous_expr, $allow_struct, $allow_struct))
        };
    }

    impl Synom for Expr {
        named!(parse -> Self, ambiguous_expr!(true));

        fn description() -> Option<&'static str> {
            Some("expression")
        }
    }

    #[cfg(feature = "full")]
    named!(expr_no_struct -> Expr, ambiguous_expr!(false));

    /// Parse an arbitrary expression.
    #[cfg(feature = "full")]
    fn ambiguous_expr(i: Cursor,
                      allow_struct: bool,
                      allow_block: bool)
                      -> PResult<Expr> {
        map!(
            i,
            call!(assign_expr, allow_struct, allow_block),
            ExprKind::into
        )
    }

    #[cfg(not(feature = "full"))]
    fn ambiguous_expr(i: Cursor,
                      allow_struct: bool,
                      allow_block: bool)
                      -> PResult<Expr> {
        map!(
            i,
            // NOTE: We intentionally skip assign_expr, placement_expr, and
            // range_expr, as they are not parsed in non-full mode.
            call!(or_expr, allow_struct, allow_block),
            ExprKind::into
        )
    }

    /// Parse a left-associative binary operator.
    macro_rules! binop {
        (
            $name: ident,
            $next: ident,
            $submac: ident!( $($args:tt)* )
        ) => {
            named!($name(allow_struct: bool, allow_block: bool) -> ExprKind, do_parse!(
                mut e: call!($next, allow_struct, allow_block) >>
                many0!(do_parse!(
                    op: $submac!($($args)*) >>
                    rhs: call!($next, allow_struct, true) >>
                    ({
                        e = ExprBinary {
                            left: Box::new(e.into()),
                            op: op,
                            right: Box::new(rhs.into()),
                        }.into();
                    })
                )) >>
                (e)
            ));
        }
    }

    /// ```ignore
    /// <placement> = <placement> ..
    /// <placement> += <placement> ..
    /// <placement> -= <placement> ..
    /// <placement> *= <placement> ..
    /// <placement> /= <placement> ..
    /// <placement> %= <placement> ..
    /// <placement> ^= <placement> ..
    /// <placement> &= <placement> ..
    /// <placement> |= <placement> ..
    /// <placement> <<= <placement> ..
    /// <placement> >>= <placement> ..
    /// ```
    ///
    /// NOTE: This operator is right-associative.
    #[cfg(feature = "full")]
    named!(assign_expr(allow_struct: bool, allow_block: bool) -> ExprKind, do_parse!(
        mut e: call!(placement_expr, allow_struct, allow_block) >>
        alt!(
            do_parse!(
                eq: syn!(Eq) >>
                // Recurse into self to parse right-associative operator.
                rhs: call!(assign_expr, allow_struct, true) >>
                ({
                    e = ExprAssign {
                        left: Box::new(e.into()),
                        eq_token: eq,
                        right: Box::new(rhs.into()),
                    }.into();
                })
            )
            |
            do_parse!(
                op: call!(BinOp::parse_assign_op) >>
                // Recurse into self to parse right-associative operator.
                rhs: call!(assign_expr, allow_struct, true) >>
                ({
                    e = ExprAssignOp {
                        left: Box::new(e.into()),
                        op: op,
                        right: Box::new(rhs.into()),
                    }.into();
                })
            )
            |
            epsilon!()
        ) >>
        (e)
    ));

    /// ```ignore
    /// <range> <- <range> ..
    /// ```
    ///
    /// NOTE: The `in place { expr }` version of this syntax is parsed in
    /// `atom_expr`, not here.
    ///
    /// NOTE: This operator is right-associative.
    #[cfg(feature = "full")]
    named!(placement_expr(allow_struct: bool, allow_block: bool) -> ExprKind, do_parse!(
        mut e: call!(range_expr, allow_struct, allow_block) >>
        alt!(
            do_parse!(
                arrow: syn!(LArrow) >>
                // Recurse into self to parse right-associative operator.
                rhs: call!(placement_expr, allow_struct, true) >>
                ({
                    e = ExprInPlace {
                        // op: BinOp::Place(larrow),
                        place: Box::new(e.into()),
                        kind: InPlaceKind::Arrow(arrow),
                        value: Box::new(rhs.into()),
                    }.into();
                })
            )
            |
            epsilon!()
        ) >>
        (e)
    ));

    /// ```ignore
    /// <or> ... <or> ..
    /// <or> .. <or> ..
    /// <or> ..
    /// ```
    ///
    /// NOTE: This is currently parsed oddly - I'm not sure of what the exact
    /// rules are for parsing these expressions are, but this is not correct.
    /// For example, `a .. b .. c` is not a legal expression. It should not
    /// be parsed as either `(a .. b) .. c` or `a .. (b .. c)` apparently.
    ///
    /// NOTE: The form of ranges which don't include a preceding expression are
    /// parsed by `atom_expr`, rather than by this function.
    #[cfg(feature = "full")]
    named!(range_expr(allow_struct: bool, allow_block: bool) -> ExprKind, do_parse!(
        mut e: call!(or_expr, allow_struct, allow_block) >>
        many0!(do_parse!(
            limits: syn!(RangeLimits) >>
            // We don't want to allow blocks here if we don't allow structs. See
            // the reasoning for `opt_ambiguous_expr!` above.
            hi: option!(call!(or_expr, allow_struct, allow_struct)) >>
            ({
                e = ExprRange {
                    from: Some(Box::new(e.into())),
                    limits: limits,
                    to: hi.map(|e| Box::new(e.into())),
                }.into();
            })
        )) >>
        (e)
    ));

    /// ```ignore
    /// <and> || <and> ...
    /// ```
    binop!(or_expr, and_expr, map!(syn!(OrOr), BinOp::Or));

    /// ```ignore
    /// <compare> && <compare> ...
    /// ```
    binop!(and_expr, compare_expr, map!(syn!(AndAnd), BinOp::And));

    /// ```ignore
    /// <bitor> == <bitor> ...
    /// <bitor> != <bitor> ...
    /// <bitor> >= <bitor> ...
    /// <bitor> <= <bitor> ...
    /// <bitor> > <bitor> ...
    /// <bitor> < <bitor> ...
    /// ```
    ///
    /// NOTE: This operator appears to be parsed as left-associative, but errors
    /// if it is used in a non-associative manner.
    binop!(compare_expr, bitor_expr, alt!(
        syn!(EqEq) => { BinOp::Eq }
        |
        syn!(Ne) => { BinOp::Ne }
        |
        // must be above Lt
        syn!(Le) => { BinOp::Le }
        |
        // must be above Gt
        syn!(Ge) => { BinOp::Ge }
        |
        do_parse!(
            // Make sure that we don't eat the < part of a <- operator
            not!(syn!(LArrow)) >>
            t: syn!(Lt) >>
            (BinOp::Lt(t))
        )
        |
        syn!(Gt) => { BinOp::Gt }
    ));

    /// ```ignore
    /// <bitxor> | <bitxor> ...
    /// ```
    binop!(bitor_expr, bitxor_expr, do_parse!(
        not!(syn!(OrOr)) >>
        not!(syn!(OrEq)) >>
        t: syn!(Or) >>
        (BinOp::BitOr(t))
    ));

    /// ```ignore
    /// <bitand> ^ <bitand> ...
    /// ```
    binop!(bitxor_expr, bitand_expr, do_parse!(
        // NOTE: Make sure we aren't looking at ^=.
        not!(syn!(CaretEq)) >>
        t: syn!(Caret) >>
        (BinOp::BitXor(t))
    ));

    /// ```ignore
    /// <shift> & <shift> ...
    /// ```
    binop!(bitand_expr, shift_expr, do_parse!(
        // NOTE: Make sure we aren't looking at && or &=.
        not!(syn!(AndAnd)) >>
        not!(syn!(AndEq)) >>
        t: syn!(And) >>
        (BinOp::BitAnd(t))
    ));

    /// ```ignore
    /// <arith> << <arith> ...
    /// <arith> >> <arith> ...
    /// ```
    binop!(shift_expr, arith_expr, alt!(
        syn!(Shl) => { BinOp::Shl }
        |
        syn!(Shr) => { BinOp::Shr }
    ));

    /// ```ignore
    /// <term> + <term> ...
    /// <term> - <term> ...
    /// ```
    binop!(arith_expr, term_expr, alt!(
        syn!(Add) => { BinOp::Add }
        |
        syn!(Sub) => { BinOp::Sub }
    ));

    /// ```ignore
    /// <cast> * <cast> ...
    /// <cast> / <cast> ...
    /// <cast> % <cast> ...
    /// ```
    binop!(term_expr, cast_expr, alt!(
        syn!(Star) => { BinOp::Mul }
        |
        syn!(Div) => { BinOp::Div }
        |
        syn!(Rem) => { BinOp::Rem }
    ));

    /// ```ignore
    /// <unary> as <ty>
    /// <unary> : <ty>
    /// ```
    named!(cast_expr(allow_struct: bool, allow_block: bool) -> ExprKind, do_parse!(
        mut e: call!(unary_expr, allow_struct, allow_block) >>
        many0!(alt!(
            do_parse!(
                as_: syn!(As) >>
                // We can't accept `A + B` in cast expressions, as it's
                // ambiguous with the + expression.
                ty: call!(Ty::without_plus) >>
                ({
                    e = ExprCast {
                        expr: Box::new(e.into()),
                        as_token: as_,
                        ty: Box::new(ty),
                    }.into();
                })
            )
            |
            do_parse!(
                colon: syn!(Colon) >>
                // We can't accept `A + B` in cast expressions, as it's
                // ambiguous with the + expression.
                ty: call!(Ty::without_plus) >>
                ({
                    e = ExprType {
                        expr: Box::new(e.into()),
                        colon_token: colon,
                        ty: Box::new(ty),
                    }.into();
                })
            )
        )) >>
        (e)
    ));

    /// ```
    /// <UnOp> <trailer>
    /// & <trailer>
    /// &mut <trailer>
    /// box <trailer>
    /// ```
    #[cfg(feature = "full")]
    named!(unary_expr(allow_struct: bool, allow_block: bool) -> ExprKind, alt!(
        do_parse!(
            op: syn!(UnOp) >>
            expr: call!(unary_expr, allow_struct, true) >>
            (ExprUnary {
                op: op,
                expr: Box::new(expr.into()),
            }.into())
        )
        |
        do_parse!(
            and: syn!(And) >>
            mutability: syn!(Mutability) >>
            expr: call!(unary_expr, allow_struct, true) >>
            (ExprAddrOf {
                and_token: and,
                mutbl: mutability,
                expr: Box::new(expr.into()),
            }.into())
        )
        |
        do_parse!(
            box_: syn!(Box_) >>
            expr: call!(unary_expr, allow_struct, true) >>
            (ExprBox {
                box_token: box_,
                expr: Box::new(expr.into()),
            }.into())
        )
        |
        call!(trailer_expr, allow_struct, allow_block)
    ));

    // XXX: This duplication is ugly
    #[cfg(not(feature = "full"))]
    named!(unary_expr(allow_struct: bool, allow_block: bool) -> ExprKind, alt!(
        do_parse!(
            op: syn!(UnOp) >>
            expr: call!(unary_expr, allow_struct, true) >>
            (ExprUnary {
                op: op,
                expr: Box::new(expr.into()),
            }.into())
        )
        |
        call!(trailer_expr, allow_struct, allow_block)
    ));

    /// ```ignore
    /// <atom> (..<args>) ...
    /// <atom> . <ident> (..<args>) ...
    /// <atom> . <ident> ...
    /// <atom> . <lit> ...
    /// <atom> [ <expr> ] ...
    /// <atom> ? ...
    /// ```
    #[cfg(feature = "full")]
    named!(trailer_expr(allow_struct: bool, allow_block: bool) -> ExprKind, do_parse!(
        mut e: call!(atom_expr, allow_struct, allow_block) >>
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
            tap!(question: syn!(Question) => {
                e = ExprTry {
                    expr: Box::new(e.into()),
                    question_token: question,
                }.into();
            })
        )) >>
        (e)
    ));

    // XXX: Duplication == ugly
    #[cfg(not(feature = "full"))]
    named!(trailer_expr(allow_struct: bool, allow_block: bool) -> ExprKind, do_parse!(
        mut e: call!(atom_expr, allow_struct, allow_block) >>
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
            tap!(i: and_index => {
                let (i, token) = i;
                e = ExprIndex {
                    expr: Box::new(e.into()),
                    bracket_token: token,
                    index: Box::new(i),
                }.into();
            })
        )) >>
        (e)
    ));

    /// Parse all atomic expressions which don't have to worry about precidence
    /// interactions, as they are fully contained.
    #[cfg(feature = "full")]
    named!(atom_expr(allow_struct: bool, allow_block: bool) -> ExprKind, alt!(
        syn!(ExprGroup) => { ExprKind::Group } // must be placed first
        |
        syn!(Lit) => { ExprKind::Lit } // must be before expr_struct
        |
        // must be before expr_path
        cond_reduce!(allow_struct, map!(syn!(ExprStruct), ExprKind::Struct))
        |
        syn!(ExprParen) => { ExprKind::Paren } // must be before expr_tup
        |
        syn!(Mac) => { ExprKind::Mac } // must be before expr_path
        |
        call!(expr_break, allow_struct) // must be before expr_path
        |
        syn!(ExprContinue) => { ExprKind::Continue } // must be before expr_path
        |
        call!(expr_ret, allow_struct) // must be before expr_path
        |
        // NOTE: The `in place { expr }` form. `place <- expr` is parsed above.
        syn!(ExprInPlace) => { ExprKind::InPlace }
        |
        syn!(ExprArray) => { ExprKind::Array }
        |
        syn!(ExprTup) => { ExprKind::Tup }
        |
        syn!(ExprIf) => { ExprKind::If }
        |
        syn!(ExprIfLet) => { ExprKind::IfLet }
        |
        syn!(ExprWhile) => { ExprKind::While }
        |
        syn!(ExprWhileLet) => { ExprKind::WhileLet }
        |
        syn!(ExprForLoop) => { ExprKind::ForLoop }
        |
        syn!(ExprLoop) => { ExprKind::Loop }
        |
        syn!(ExprMatch) => { ExprKind::Match }
        |
        syn!(ExprCatch) => { ExprKind::Catch }
        |
        syn!(ExprYield) => { ExprKind::Yield }
        |
        call!(expr_closure, allow_struct)
        |
        cond_reduce!(allow_block, map!(syn!(ExprBlock), ExprKind::Block))
        |
        // NOTE: This is the prefix-form of range
        call!(expr_range, allow_struct)
        |
        syn!(ExprPath) => { ExprKind::Path }
        |
        syn!(ExprRepeat) => { ExprKind::Repeat }
    ));

    #[cfg(not(feature = "full"))]
    named!(atom_expr(_allow_struct: bool, _allow_block: bool) -> ExprKind, alt!(
        syn!(ExprGroup) => { ExprKind::Group } // must be placed first
        |
        syn!(Lit) => { ExprKind::Lit } // must be before expr_struct
        |
        syn!(ExprParen) => { ExprKind::Paren } // must be before expr_tup
        |
        syn!(Mac) => { ExprKind::Mac } // must be before expr_path
        |
        syn!(ExprPath) => { ExprKind::Path }
    ));


    #[cfg(feature = "full")]
    named!(expr_nosemi -> Expr, map!(alt!(
        syn!(ExprIf) => { ExprKind::If }
        |
        syn!(ExprIfLet) => { ExprKind::IfLet }
        |
        syn!(ExprWhile) => { ExprKind::While }
        |
        syn!(ExprWhileLet) => { ExprKind::WhileLet }
        |
        syn!(ExprForLoop) => { ExprKind::ForLoop }
        |
        syn!(ExprLoop) => { ExprKind::Loop }
        |
        syn!(ExprMatch) => { ExprKind::Match }
        |
        syn!(ExprCatch) => { ExprKind::Catch }
        |
        syn!(ExprYield) => { ExprKind::Yield }
        |
        syn!(ExprBlock) => { ExprKind::Block }
    ), Expr::from));

    impl Synom for ExprGroup {
        named!(parse -> Self, do_parse!(
            e: grouped!(syn!(Expr)) >>
            (ExprGroup {
                expr: Box::new(e.0),
                group_token: e.1,
            }.into())
        ));
    }

    impl Synom for ExprParen {
        named!(parse -> Self, do_parse!(
            e: parens!(syn!(Expr)) >>
            (ExprParen {
                expr: Box::new(e.0),
                paren_token: e.1,
            }.into())
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprInPlace {
        named!(parse -> Self, do_parse!(
            in_: syn!(In) >>
            place: expr_no_struct >>
            value: braces!(call!(Block::parse_within)) >>
            (ExprInPlace {
                place: Box::new(place),
                kind: InPlaceKind::In(in_),
                value: Box::new(Expr {
                    node: ExprBlock {
                        unsafety: Unsafety::Normal,
                        block: Block {
                            stmts: value.0,
                            brace_token: value.1,
                        },
                    }.into(),
                    attrs: Vec::new(),
                }),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprArray {
        named!(parse -> Self, do_parse!(
            elems: brackets!(call!(Delimited::parse_terminated)) >>
            (ExprArray {
                exprs: elems.0,
                bracket_token: elems.1,
            })
        ));
    }

    named!(and_call -> (Delimited<Expr, tokens::Comma>, tokens::Paren),
           parens!(call!(Delimited::parse_terminated)));

    #[cfg(feature = "full")]
    named!(and_method_call -> ExprMethodCall, do_parse!(
        dot: syn!(Dot) >>
        method: syn!(Ident) >>
        typarams: option!(do_parse!(
            colon2: syn!(Colon2) >>
            lt: syn!(Lt) >>
            tys: call!(Delimited::parse_terminated) >>
            gt: syn!(Gt) >>
            (colon2, lt, tys, gt)
        )) >>
        args: parens!(call!(Delimited::parse_terminated)) >>
        ({
            let (colon2, lt, tys, gt) = match typarams {
                Some((a, b, c, d)) => (Some(a), Some(b), Some(c), Some(d)),
                None => (None, None, None, None),
            };
            ExprMethodCall {
                // this expr will get overwritten after being returned
                expr: Box::new(ExprKind::Lit(Lit {
                    span: Span::default(),
                    value: LitKind::Bool(false),
                }).into()),

                method: method,
                args: args.0,
                paren_token: args.1,
                dot_token: dot,
                lt_token: lt,
                gt_token: gt,
                colon2_token: colon2,
                typarams: tys.unwrap_or_default(),
            }
        })
    ));

    #[cfg(feature = "full")]
    impl Synom for ExprTup {
        named!(parse -> Self, do_parse!(
            elems: parens!(call!(Delimited::parse_terminated)) >>
            (ExprTup {
                args: elems.0,
                paren_token: elems.1,
                lone_comma: None, // TODO: parse this
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprIfLet {
        named!(parse -> Self, do_parse!(
            if_: syn!(If) >>
            let_: syn!(Let) >>
            pat: syn!(Pat) >>
            eq: syn!(Eq) >>
            cond: expr_no_struct >>
            then_block: braces!(call!(Block::parse_within)) >>
            else_block: option!(else_block) >>
            (ExprIfLet {
                pat: Box::new(pat),
                let_token: let_,
                eq_token: eq,
                expr: Box::new(cond),
                if_true: Block {
                    stmts: then_block.0,
                    brace_token: then_block.1,
                },
                if_token: if_,
                else_token: else_block.as_ref().map(|p| Else((p.0).0)),
                if_false: else_block.map(|p| Box::new(p.1.into())),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprIf {
        named!(parse -> Self, do_parse!(
            if_: syn!(If) >>
            cond: expr_no_struct >>
            then_block: braces!(call!(Block::parse_within)) >>
            else_block: option!(else_block) >>
            (ExprIf {
                cond: Box::new(cond),
                if_true: Block {
                    stmts: then_block.0,
                    brace_token: then_block.1,
                },
                if_token: if_,
                else_token: else_block.as_ref().map(|p| Else((p.0).0)),
                if_false: else_block.map(|p| Box::new(p.1.into())),
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(else_block -> (Else, ExprKind), do_parse!(
        else_: syn!(Else) >>
        expr: alt!(
            syn!(ExprIf) => { ExprKind::If }
            |
            syn!(ExprIfLet) => { ExprKind::IfLet }
            |
            do_parse!(
                else_block: braces!(call!(Block::parse_within)) >>
                (ExprKind::Block(ExprBlock {
                    unsafety: Unsafety::Normal,
                    block: Block {
                        stmts: else_block.0,
                        brace_token: else_block.1,
                    },
                }))
            )
        ) >>
        (else_, expr)
    ));


    #[cfg(feature = "full")]
    impl Synom for ExprForLoop {
        named!(parse -> Self, do_parse!(
            lbl: option!(tuple!(syn!(Lifetime), syn!(Colon))) >>
            for_: syn!(For) >>
            pat: syn!(Pat) >>
            in_: syn!(In) >>
            expr: expr_no_struct >>
            loop_block: syn!(Block) >>
            (ExprForLoop {
                for_token: for_,
                in_token: in_,
                pat: Box::new(pat),
                expr: Box::new(expr),
                body: loop_block,
                colon_token: lbl.as_ref().map(|p| Colon((p.1).0)),
                label: lbl.map(|p| p.0),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprLoop {
        named!(parse -> Self, do_parse!(
            lbl: option!(tuple!(syn!(Lifetime), syn!(Colon))) >>
            loop_: syn!(Loop) >>
            loop_block: syn!(Block) >>
            (ExprLoop {
                loop_token: loop_,
                body: loop_block,
                colon_token: lbl.as_ref().map(|p| Colon((p.1).0)),
                label: lbl.map(|p| p.0),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprMatch {
        named!(parse -> Self, do_parse!(
            match_: syn!(Match) >>
            obj: expr_no_struct >>
            res: braces!(many0!(syn!(Arm))) >>
            ({
                let (arms, brace) = res;
                ExprMatch {
                    expr: Box::new(obj),
                    match_token: match_,
                    brace_token: brace,
                    arms: arms,
                }
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprCatch {
        named!(parse -> Self, do_parse!(
            do_: syn!(Do) >>
            catch_: syn!(Catch) >>
            catch_block: syn!(Block) >>
            (ExprCatch {
                block: catch_block,
                do_token: do_,
                catch_token: catch_,
            }.into())
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprYield {
        named!(parse -> Self, do_parse!(
            yield_: syn!(Yield) >>
            expr: option!(syn!(Expr)) >>
            (ExprYield {
                yield_token: yield_,
                expr: expr.map(Box::new),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for Arm {
        named!(parse -> Self, do_parse!(
            attrs: many0!(call!(Attribute::parse_outer)) >>
            pats: call!(Delimited::parse_separated_nonempty) >>
            guard: option!(tuple!(syn!(If), syn!(Expr))) >>
            rocket: syn!(Rocket) >>
            body: do_parse!(
                expr: alt!(expr_nosemi | syn!(Expr)) >>
                comma1: cond!(arm_expr_requires_comma(&expr), alt!(
                    map!(input_end!(), |_| None)
                    |
                    map!(syn!(Comma), Some)
                )) >>
                comma2: cond!(!arm_expr_requires_comma(&expr), option!(syn!(Comma))) >>
                (expr, comma1.and_then(|x| x).or(comma2.and_then(|x| x)))
            ) >>
            (Arm {
                rocket_token: rocket,
                if_token: guard.as_ref().map(|p| If((p.0).0)),
                attrs: attrs,
                pats: pats,
                guard: guard.map(|p| Box::new(p.1)),
                body: Box::new(body.0),
                comma: body.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(expr_closure(allow_struct: bool) -> ExprKind, do_parse!(
        capture: syn!(CaptureBy) >>
        or1: syn!(Or) >>
        inputs: call!(Delimited::parse_terminated_with, fn_arg) >>
        or2: syn!(Or) >>
        ret_and_body: alt!(
            do_parse!(
                arrow: syn!(RArrow) >>
                ty: syn!(Ty) >>
                body: syn!(Block) >>
                (FunctionRetTy::Ty(ty, arrow),
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
            or1_token: or1,
            or2_token: or2,
            decl: Box::new(FnDecl {
                inputs: inputs,
                output: ret_and_body.0,
                variadic: false,
                dot_tokens: None,
                fn_token: tokens::Fn_::default(),
                generics: Generics::default(),
                paren_token: tokens::Paren::default(),
            }),
            body: Box::new(ret_and_body.1),
        }.into())
    ));

    #[cfg(feature = "full")]
    named!(fn_arg -> FnArg, do_parse!(
        pat: syn!(Pat) >>
        ty: option!(tuple!(syn!(Colon), syn!(Ty))) >>
        ({
            let (colon, ty) = ty.unwrap_or_else(|| {
                (Colon::default(), TyInfer {
                    underscore_token: Underscore::default(),
                }.into())
            });
            ArgCaptured {
                pat: pat,
                colon_token: colon,
                ty: ty,
            }.into()
        })
    ));

    #[cfg(feature = "full")]
    impl Synom for ExprWhile {
        named!(parse -> Self, do_parse!(
            lbl: option!(tuple!(syn!(Lifetime), syn!(Colon))) >>
            while_: syn!(While) >>
            cond: expr_no_struct >>
            while_block: syn!(Block) >>
            (ExprWhile {
                while_token: while_,
                colon_token: lbl.as_ref().map(|p| Colon((p.1).0)),
                cond: Box::new(cond),
                body: while_block,
                label: lbl.map(|p| p.0),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprWhileLet {
        named!(parse -> Self, do_parse!(
            lbl: option!(tuple!(syn!(Lifetime), syn!(Colon))) >>
            while_: syn!(While) >>
            let_: syn!(Let) >>
            pat: syn!(Pat) >>
            eq: syn!(Eq) >>
            value: expr_no_struct >>
            while_block: syn!(Block) >>
            (ExprWhileLet {
                eq_token: eq,
                let_token: let_,
                while_token: while_,
                colon_token: lbl.as_ref().map(|p| Colon((p.1).0)),
                pat: Box::new(pat),
                expr: Box::new(value),
                body: while_block,
                label: lbl.map(|p| p.0),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprContinue {
        named!(parse -> Self, do_parse!(
            cont: syn!(Continue) >>
            lbl: option!(syn!(Lifetime)) >>
            (ExprContinue {
                continue_token: cont,
                label: lbl,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(expr_break(allow_struct: bool) -> ExprKind, do_parse!(
        break_: syn!(Break) >>
        lbl: option!(syn!(Lifetime)) >>
        // We can't allow blocks after a `break` expression when we wouldn't
        // allow structs, as this expression is ambiguous.
        val: opt_ambiguous_expr!(allow_struct) >>
        (ExprBreak {
            label: lbl,
            expr: val.map(Box::new),
            break_token: break_,
        }.into())
    ));

    #[cfg(feature = "full")]
    named!(expr_ret(allow_struct: bool) -> ExprKind, do_parse!(
        return_: syn!(Return) >>
        // NOTE: return is greedy and eats blocks after it even when in a
        // position where structs are not allowed, such as in if statement
        // conditions. For example:
        //
        // if return { println!("A") } { } // Prints "A"
        ret_value: option!(ambiguous_expr!(allow_struct)) >>
        (ExprRet {
            expr: ret_value.map(Box::new),
            return_token: return_,
        }.into())
    ));

    #[cfg(feature = "full")]
    impl Synom for ExprStruct {
        named!(parse -> Self, do_parse!(
            path: syn!(Path) >>
            data: braces!(do_parse!(
                fields: call!(Delimited::parse_terminated) >>
                base: option!(
                    cond!(fields.is_empty() || fields.trailing_delim(),
                        do_parse!(
                            dots: syn!(Dot2) >>
                            base: syn!(Expr) >>
                            (dots, base)
                        )
                    )
                ) >>
                (fields, base)
            )) >>
            ({
                let ((fields, base), brace) = data;
                let (dots, rest) = match base.and_then(|b| b) {
                    Some((dots, base)) => (Some(dots), Some(base)),
                    None => (None, None),
                };
                ExprStruct {
                    brace_token: brace,
                    path: path,
                    fields: fields,
                    dot2_token: dots,
                    rest: rest.map(Box::new),
                }
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for FieldValue {
        named!(parse -> Self, alt!(
            do_parse!(
                ident: field_ident >>
                colon: syn!(Colon) >>
                value: syn!(Expr) >>
                (FieldValue {
                    ident: ident,
                    expr: value,
                    is_shorthand: false,
                    attrs: Vec::new(),
                    colon_token: Some(colon),
                })
            )
            |
            map!(syn!(Ident), |name| FieldValue {
                ident: name.clone(),
                expr: ExprKind::Path(ExprPath { qself: None, path: name.into() }).into(),
                is_shorthand: true,
                attrs: Vec::new(),
                colon_token: None,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprRepeat {
        named!(parse -> Self, do_parse!(
            data: brackets!(do_parse!(
                value: syn!(Expr) >>
                semi: syn!(Semi) >>
                times: syn!(Expr) >>
                (value, semi, times)
            )) >>
            (ExprRepeat {
                expr: Box::new((data.0).0),
                amt: Box::new((data.0).2),
                bracket_token: data.1,
                semi_token: (data.0).1,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprBlock {
        named!(parse -> Self, do_parse!(
            rules: syn!(Unsafety) >>
            b: syn!(Block) >>
            (ExprBlock {
                unsafety: rules,
                block: b,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(expr_range(allow_struct: bool) -> ExprKind, do_parse!(
        limits: syn!(RangeLimits) >>
        hi: opt_ambiguous_expr!(allow_struct) >>
        (ExprRange { from: None, to: hi.map(Box::new), limits: limits }.into())
    ));

    #[cfg(feature = "full")]
    impl Synom for RangeLimits {
        named!(parse -> Self, alt!(
            // Must come before Dot2
            syn!(Dot3) => { RangeLimits::Closed }
            |
            syn!(Dot2) => { RangeLimits::HalfOpen }
        ));
    }

    impl Synom for ExprPath {
        named!(parse -> Self, do_parse!(
            pair: qpath >>
            (ExprPath {
                qself: pair.0,
                path: pair.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(and_field -> (Ident, Dot),
           map!(tuple!(syn!(Dot), syn!(Ident)), |(a, b)| (b, a)));

    #[cfg(feature = "full")]
    named!(and_tup_field -> (Lit, Dot),
           map!(tuple!(syn!(Dot), syn!(Lit)), |(a, b)| (b, a)));

    named!(and_index -> (Expr, tokens::Bracket), brackets!(syn!(Expr)));

    #[cfg(feature = "full")]
    impl Synom for Block {
        named!(parse -> Self, do_parse!(
            stmts: braces!(call!(Block::parse_within)) >>
            (Block {
                stmts: stmts.0,
                brace_token: stmts.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Block {
        named!(pub parse_within -> Vec<Stmt>, do_parse!(
            many0!(syn!(Semi)) >>
            mut standalone: many0!(terminated!(syn!(Stmt), many0!(syn!(Semi)))) >>
            last: option!(do_parse!(
                attrs: many0!(call!(Attribute::parse_outer)) >>
                mut e: syn!(Expr) >>
                ({
                    e.attrs = attrs;
                    Stmt::Expr(Box::new(e))
                })
            )) >>
            (match last {
                None => standalone,
                Some(last) => {
                    standalone.push(last);
                    standalone
                }
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for Stmt {
        named!(parse -> Self, alt!(
            stmt_mac
            |
            stmt_local
            |
            stmt_item
            |
            stmt_blockexpr
            |
            stmt_expr
        ));
    }

    #[cfg(feature = "full")]
    named!(stmt_mac -> Stmt, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        what: syn!(Path) >>
        bang: syn!(Bang) >>
    // Only parse braces here; paren and bracket will get parsed as
    // expression statements
        data: braces!(syn!(TokenStream)) >>
        semi: option!(syn!(Semi)) >>
        (Stmt::Mac(Box::new((
            Mac {
                path: what,
                bang_token: bang,
                ident: None,
                tokens: vec![TokenTree(proc_macro2::TokenTree {
                    span: ((data.1).0).0,
                    kind: TokenNode::Group(Delimiter::Brace, data.0),
                })],
            },
            match semi {
                Some(semi) => MacStmtStyle::Semicolon(semi),
                None => MacStmtStyle::Braces,
            },
            attrs,
        ))))
    ));

    #[cfg(feature = "full")]
    named!(stmt_local -> Stmt, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        let_: syn!(Let) >>
        pat: syn!(Pat) >>
        ty: option!(tuple!(syn!(Colon), syn!(Ty))) >>
        init: option!(tuple!(syn!(Eq), syn!(Expr))) >>
        semi: syn!(Semi) >>
        (Stmt::Local(Box::new(Local {
            let_token: let_,
            semi_token: semi,
            colon_token: ty.as_ref().map(|p| Colon((p.0).0)),
            eq_token: init.as_ref().map(|p| Eq((p.0).0)),
            pat: Box::new(pat),
            ty: ty.map(|p| Box::new(p.1)),
            init: init.map(|p| Box::new(p.1)),
            attrs: attrs,
        })))
    ));

    #[cfg(feature = "full")]
    named!(stmt_item -> Stmt, map!(syn!(Item), |i| Stmt::Item(Box::new(i))));

    #[cfg(feature = "full")]
    named!(stmt_blockexpr -> Stmt, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        mut e: expr_nosemi >>
        // If the next token is a `.` or a `?` it is special-cased to parse as
        // an expression instead of a blockexpression.
        not!(syn!(Dot)) >>
        not!(syn!(Question)) >>
        semi: option!(syn!(Semi)) >>
        ({
            e.attrs = attrs;
            if let Some(semi) = semi {
                Stmt::Semi(Box::new(e), semi)
            } else {
                Stmt::Expr(Box::new(e))
            }
        })
    ));

    #[cfg(feature = "full")]
    named!(stmt_expr -> Stmt, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        mut e: syn!(Expr) >>
        semi: syn!(Semi) >>
        ({
            e.attrs = attrs;
            Stmt::Semi(Box::new(e), semi)
        })
    ));

    #[cfg(feature = "full")]
    impl Synom for Pat {
        named!(parse -> Self, alt!(
            syn!(PatWild) => { Pat::Wild } // must be before pat_ident
            |
            syn!(PatBox) => { Pat::Box }  // must be before pat_ident
            |
            syn!(PatRange) => { Pat::Range } // must be before pat_lit
            |
            syn!(PatTupleStruct) => { Pat::TupleStruct }  // must be before pat_ident
            |
            syn!(PatStruct) => { Pat::Struct } // must be before pat_ident
            |
            syn!(Mac) => { Pat::Mac } // must be before pat_ident
            |
            syn!(PatLit) => { Pat::Lit } // must be before pat_ident
            |
            syn!(PatIdent) => { Pat::Ident } // must be before pat_path
            |
            syn!(PatPath) => { Pat::Path }
            |
            syn!(PatTuple) => { Pat::Tuple }
            |
            syn!(PatRef) => { Pat::Ref }
            |
            syn!(PatSlice) => { Pat::Slice }
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatWild {
        named!(parse -> Self, map!(
            syn!(Underscore),
            |u| PatWild { underscore_token: u }
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatBox {
        named!(parse -> Self, do_parse!(
            boxed: syn!(Box_) >>
            pat: syn!(Pat) >>
            (PatBox {
                pat: Box::new(pat),
                box_token: boxed,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatIdent {
        named!(parse -> Self, do_parse!(
            mode: option!(syn!(Ref)) >>
            mutability: syn!(Mutability) >>
            name: alt!(
                syn!(Ident)
                |
                syn!(Self_) => { Into::into }
            ) >>
            not!(syn!(Lt)) >>
            not!(syn!(Colon2)) >>
            subpat: option!(tuple!(syn!(At), syn!(Pat))) >>
            (PatIdent {
                mode: match mode {
                    Some(mode) => BindingMode::ByRef(mode, mutability),
                    None => BindingMode::ByValue(mutability),
                },
                ident: name,
                at_token: subpat.as_ref().map(|p| At((p.0).0)),
                subpat: subpat.map(|p| Box::new(p.1)),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatTupleStruct {
        named!(parse -> Self, do_parse!(
            path: syn!(Path) >>
            tuple: syn!(PatTuple) >>
            (PatTupleStruct {
                path: path,
                pat: tuple,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatStruct {
        named!(parse -> Self, do_parse!(
            path: syn!(Path) >>
            data: braces!(do_parse!(
                fields: call!(Delimited::parse_terminated) >>
                base: option!(
                    cond!(fields.is_empty() || fields.trailing_delim(),
                          syn!(Dot2))
                ) >>
                (fields, base)
            )) >>
            (PatStruct {
                path: path,
                fields: (data.0).0,
                brace_token: data.1,
                dot2_token: (data.0).1.and_then(|m| m),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for FieldPat {
        named!(parse -> Self, alt!(
            do_parse!(
                ident: field_ident >>
                colon: syn!(Colon) >>
                pat: syn!(Pat) >>
                (FieldPat {
                    ident: ident,
                    pat: Box::new(pat),
                    is_shorthand: false,
                    attrs: Vec::new(),
                    colon_token: Some(colon),
                })
            )
            |
            do_parse!(
                boxed: option!(syn!(Box_)) >>
                mode: option!(syn!(Ref)) >>
                mutability: syn!(Mutability) >>
                ident: syn!(Ident) >>
                ({
                    let mut pat: Pat = PatIdent {
                        mode: if let Some(mode) = mode {
                            BindingMode::ByRef(mode, mutability)
                        } else {
                            BindingMode::ByValue(mutability)
                        },
                        ident: ident.clone(),
                        subpat: None,
                        at_token: None,
                    }.into();
                    if let Some(boxed) = boxed {
                        pat = PatBox {
                            pat: Box::new(pat),
                            box_token: boxed,
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
    }

    #[cfg(feature = "full")]
    named!(field_ident -> Ident, alt!(
        syn!(Ident)
        |
        do_parse!(
            lit: syn!(Lit) >>
            ({
                let s = lit.to_string();
                if s.parse::<usize>().is_ok() {
                    Ident::new(Term::intern(&s), lit.span)
                } else {
                    return parse_error();
                }
            })
        )
    ));

    #[cfg(feature = "full")]
    impl Synom for PatPath {
        named!(parse -> Self, map!(
            syn!(ExprPath),
            |p| PatPath { qself: p.qself, path: p.path }
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatTuple {
        named!(parse -> Self, do_parse!(
            data: parens!(do_parse!(
                elems: call!(Delimited::parse_terminated) >>
                dotdot: map!(cond!(
                    elems.is_empty() || elems.trailing_delim(),
                    option!(do_parse!(
                        dots: syn!(Dot2) >>
                        trailing: option!(syn!(Comma)) >>
                        (dots, trailing)
                    ))
                ), |x| x.and_then(|x| x)) >>
                rest: cond!(match dotdot {
                                Some((_, Some(_))) => true,
                                _ => false,
                            },
                            call!(Delimited::parse_terminated)) >>
                (elems, dotdot, rest)
            )) >>
            ({
                let ((mut elems, dotdot, rest), parens) = data;
                let (dotdot, trailing) = match dotdot {
                    Some((a, b)) => (Some(a), Some(b)),
                    None => (None, None),
                };
                PatTuple {
                    paren_token: parens,
                    dots_pos: dotdot.as_ref().map(|_| elems.len()),
                    dot2_token: dotdot,
                    comma_token: trailing.and_then(|b| b),
                    pats: {
                        if let Some(rest) = rest {
                            for elem in rest {
                                elems.push(elem);
                            }
                        }
                        elems
                    },
                }
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatRef {
        named!(parse -> Self, do_parse!(
            and: syn!(And) >>
            mutability: syn!(Mutability) >>
            pat: syn!(Pat) >>
            (PatRef {
                pat: Box::new(pat),
                mutbl: mutability,
                and_token: and,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatLit {
        named!(parse -> Self, do_parse!(
            lit: pat_lit_expr >>
            (if let ExprKind::Path(_) = lit.node {
                return parse_error(); // these need to be parsed by pat_path
            } else {
                PatLit {
                    expr: Box::new(lit),
                }
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatRange {
        named!(parse -> Self, do_parse!(
            lo: pat_lit_expr >>
            limits: syn!(RangeLimits) >>
            hi: pat_lit_expr >>
            (PatRange {
                lo: Box::new(lo),
                hi: Box::new(hi),
                limits: limits,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(pat_lit_expr -> Expr, do_parse!(
        neg: option!(syn!(Sub)) >>
        v: alt!(
            syn!(Lit) => { ExprKind::Lit }
            |
            syn!(ExprPath) => { ExprKind::Path }
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

    #[cfg(feature = "full")]
    impl Synom for PatSlice {
        named!(parse -> Self, map!(
            brackets!(do_parse!(
                before: call!(Delimited::parse_terminated) >>
                middle: option!(do_parse!(
                    dots: syn!(Dot2) >>
                    trailing: option!(syn!(Comma)) >>
                    (dots, trailing)
                )) >>
                after: cond!(
                    match middle {
                        Some((_, ref trailing)) => trailing.is_some(),
                        _ => false,
                    },
                    call!(Delimited::parse_terminated)
                ) >>
                (before, middle, after)
            )),
            |((before, middle, after), brackets)| {
                let mut before: Delimited<Pat, tokens::Comma> = before;
                let after: Option<Delimited<Pat, tokens::Comma>> = after;
                let middle: Option<(Dot2, Option<Comma>)> = middle;
                PatSlice {
                    dot2_token: middle.as_ref().map(|m| Dot2((m.0).0)),
                    comma_token: middle.as_ref().and_then(|m| {
                        m.1.as_ref().map(|m| Comma(m.0))
                    }),
                    bracket_token: brackets,
                    middle: middle.and_then(|_| {
                        if !before.is_empty() && !before.trailing_delim() {
                            Some(Box::new(before.pop().unwrap().into_item()))
                        } else {
                            None
                        }
                    }),
                    front: before,
                    back: after.unwrap_or_default(),
                }
            }
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for CaptureBy {
        named!(parse -> Self, alt!(
            syn!(Move) => { CaptureBy::Value }
            |
            epsilon!() => { |_| CaptureBy::Ref }
        ));
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    #[cfg(feature = "full")]
    use attr::FilterAttrs;
    use quote::{Tokens, ToTokens};

    /// If the given expression is a bare `ExprStruct`, wraps it in parenthesis
    /// before appending it to `Tokens`.
    #[cfg(feature = "full")]
    fn wrap_bare_struct(tokens: &mut Tokens, e: &Expr) {
        if let ExprKind::Struct(_) = e.node {
            tokens::Paren::default().surround(tokens, |tokens| {
                e.to_tokens(tokens);
            });
        } else {
            e.to_tokens(tokens);
        }
    }

    impl ToTokens for Expr {
        #[cfg(feature = "full")]
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.node.to_tokens(tokens)
        }

        #[cfg(not(feature = "full"))]
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.node.to_tokens(tokens)
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprBox {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.box_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprInPlace {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match self.kind {
                InPlaceKind::Arrow(ref arrow) => {
                    self.place.to_tokens(tokens);
                    arrow.to_tokens(tokens);
                    self.value.to_tokens(tokens);
                }
                InPlaceKind::In(ref _in) => {
                    _in.to_tokens(tokens);
                    self.place.to_tokens(tokens);
                    // NOTE: The second operand must be in a block, add one if
                    // it is not present.
                    if let ExprKind::Block(_) = self.value.node {
                        self.value.to_tokens(tokens);
                    } else {
                        tokens::Brace::default().surround(tokens, |tokens| {
                            self.value.to_tokens(tokens);
                        })
                    }
                }
            }
        }
    }

    #[cfg(feature = "full")]
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

    #[cfg(feature = "full")]
    impl ToTokens for ExprMethodCall {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.method.to_tokens(tokens);
            if !self.typarams.is_empty() {
                TokensOrDefault(&self.colon2_token).to_tokens(tokens);
                TokensOrDefault(&self.lt_token).to_tokens(tokens);
                self.typarams.to_tokens(tokens);
                TokensOrDefault(&self.gt_token).to_tokens(tokens);
            }
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprTup {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
                // If we only have one argument, we need a trailing comma to
                // distinguish ExprTup from ExprParen.
                if self.args.len() == 1 && !self.args.trailing_delim() {
                    tokens::Comma::default().to_tokens(tokens);
                }
                // XXX: Not sure how to handle this, but we never parse it yet.
                // Is this for an expression like (0,)? Can't we use the
                // trailing delimiter on Delimited for that? (,) isn't a valid
                // expression as far as I know.
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

    #[cfg(feature = "full")]
    fn maybe_wrap_else(tokens: &mut Tokens,
                       else_token: &Option<tokens::Else>,
                       if_false: &Option<Box<Expr>>)
    {
        if let Some(ref if_false) = *if_false {
            TokensOrDefault(&else_token).to_tokens(tokens);

            // If we are not one of the valid expressions to exist in an else
            // clause, wrap ourselves in a block.
            match if_false.node {
                ExprKind::If(_) |
                ExprKind::IfLet(_) |
                ExprKind::Block(_) => {
                    if_false.to_tokens(tokens);
                }
                _ => {
                    tokens::Brace::default().surround(tokens, |tokens| {
                        if_false.to_tokens(tokens);
                    });
                }
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprIf {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.if_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.cond);
            self.if_true.to_tokens(tokens);
            maybe_wrap_else(tokens, &self.else_token, &self.if_false);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprIfLet {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.if_token.to_tokens(tokens);
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.if_true.to_tokens(tokens);
            maybe_wrap_else(tokens, &self.else_token, &self.if_false);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprWhile {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if self.label.is_some() {
                self.label.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.while_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.cond);
            self.body.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprWhileLet {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if self.label.is_some() {
                self.label.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.while_token.to_tokens(tokens);
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.body.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprForLoop {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if self.label.is_some() {
                self.label.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.for_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.in_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.body.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprLoop {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if self.label.is_some() {
                self.label.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.loop_token.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprMatch {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.match_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.brace_token.surround(tokens, |tokens| {
                for (i,  arm) in self.arms.iter().enumerate() {
                    arm.to_tokens(tokens);
                    // Ensure that we have a comma after a non-block arm, except
                    // for the last one.
                    let is_last = i == self.arms.len() - 1;
                    if !is_last && arm_expr_requires_comma(&arm.body) && arm.comma.is_none() {
                        tokens::Comma::default().to_tokens(tokens);
                    }
                }
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprCatch {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.do_token.to_tokens(tokens);
            self.catch_token.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprYield {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.yield_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
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

    #[cfg(feature = "full")]
    impl ToTokens for ExprBlock {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.unsafety.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprAssign {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.left.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprAssignOp {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprField {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            // XXX: I don't think we can do anything if someone shoves a
            // nonsense Lit in here.
            self.field.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
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

    #[cfg(feature = "full")]
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

    #[cfg(feature = "full")]
    impl ToTokens for ExprAddrOf {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.and_token.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprBreak {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.break_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprContinue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.continue_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprRet {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.return_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprStruct {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
                if self.rest.is_some() {
                    TokensOrDefault(&self.dot2_token).to_tokens(tokens);
                    self.rest.to_tokens(tokens);
                }
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprRepeat {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bracket_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
                self.amt.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprGroup {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.group_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ExprParen {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprTry {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
            self.question_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for FieldValue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            // XXX: Override self.is_shorthand if expr is not an IdentExpr with
            // the ident self.ident?
            if !self.is_shorthand {
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
                self.expr.to_tokens(tokens);
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Arm {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(&self.attrs);
            self.pats.to_tokens(tokens);
            if self.guard.is_some() {
                TokensOrDefault(&self.if_token).to_tokens(tokens);
                self.guard.to_tokens(tokens);
            }
            self.rocket_token.to_tokens(tokens);
            self.body.to_tokens(tokens);
            self.comma.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatWild {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.underscore_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatIdent {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.mode.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            if self.subpat.is_some() {
                TokensOrDefault(&self.at_token).to_tokens(tokens);
                self.subpat.to_tokens(tokens);
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatStruct {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
                // NOTE: We need a comma before the dot2 token if it is present.
                if !self.fields.empty_or_trailing() && self.dot2_token.is_some() {
                    tokens::Comma::default().to_tokens(tokens);
                }
                self.dot2_token.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatTupleStruct {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatPath {
        fn to_tokens(&self, tokens: &mut Tokens) {
            ::PathTokens(&self.qself, &self.path).to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatTuple {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                for (i, token) in self.pats.iter().enumerate() {
                    if Some(i) == self.dots_pos {
                        TokensOrDefault(&self.dot2_token).to_tokens(tokens);
                        TokensOrDefault(&self.comma_token).to_tokens(tokens);
                    }
                    token.to_tokens(tokens);
                }

                if Some(self.pats.len()) == self.dots_pos {
                    // Ensure there is a comma before the .. token.
                    if !self.pats.empty_or_trailing() {
                        tokens::Comma::default().to_tokens(tokens);
                    }
                    self.dot2_token.to_tokens(tokens);
                }
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatBox {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.box_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatRef {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.and_token.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatLit {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatRange {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.lo.to_tokens(tokens);
            self.limits.to_tokens(tokens);
            self.hi.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatSlice {
        fn to_tokens(&self, tokens: &mut Tokens) {
            // XXX: This is a mess, and it will be so easy to screw it up. How
            // do we make this correct itself better?
            self.bracket_token.surround(tokens, |tokens| {
                self.front.to_tokens(tokens);

                // If we need a comma before the middle or standalone .. token,
                // then make sure it's present.
                if !self.front.empty_or_trailing() &&
                    (self.middle.is_some() || self.dot2_token.is_some())
                {
                    tokens::Comma::default().to_tokens(tokens);
                }

                // If we have an identifier, we always need a .. token.
                if self.middle.is_some() {
                    self.middle.to_tokens(tokens);
                    TokensOrDefault(&self.dot2_token).to_tokens(tokens);
                } else if self.dot2_token.is_some() {
                    self.dot2_token.to_tokens(tokens);
                }

                // Make sure we have a comma before the back half.
                if !self.back.is_empty() {
                    TokensOrDefault(&self.comma_token).to_tokens(tokens);
                    self.back.to_tokens(tokens);
                } else {
                    self.comma_token.to_tokens(tokens);
                }
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for RangeLimits {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                RangeLimits::HalfOpen(ref t) => t.to_tokens(tokens),
                RangeLimits::Closed(ref t) => t.to_tokens(tokens),
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for FieldPat {
        fn to_tokens(&self, tokens: &mut Tokens) {
            // XXX: Override is_shorthand if it was wrong?
            if !self.is_shorthand {
                self.ident.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.pat.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
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

    #[cfg(feature = "full")]
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

    #[cfg(feature = "full")]
    impl ToTokens for Block {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
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

    #[cfg(feature = "full")]
    impl ToTokens for Local {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            if self.ty.is_some() {
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
                self.ty.to_tokens(tokens);
            }
            if self.init.is_some() {
                TokensOrDefault(&self.eq_token).to_tokens(tokens);
                self.init.to_tokens(tokens);
            }
            self.semi_token.to_tokens(tokens);
        }
    }
}
