use super::*;
use delimited::Delimited;
#[cfg(feature = "full")]
use proc_macro2::Span;
#[cfg(feature = "full")]
use std::hash::{Hash, Hasher};

ast_enum_of_structs! {
    /// An expression.
    pub enum Expr {
        /// A `box x` expression.
        pub Box(ExprBox #full {
            pub attrs: Vec<Attribute>,
            pub box_token: Token![box],
            pub expr: Box<Expr>,
        }),

        /// E.g. 'place <- val' or `in place { val }`.
        pub InPlace(ExprInPlace #full {
            pub attrs: Vec<Attribute>,
            pub place: Box<Expr>,
            pub kind: InPlaceKind,
            pub value: Box<Expr>,
        }),

        /// An array, e.g. `[a, b, c, d]`.
        pub Array(ExprArray #full {
            pub attrs: Vec<Attribute>,
            pub bracket_token: token::Bracket,
            pub exprs: Delimited<Expr, Token![,]>,
        }),

        /// A function call.
        pub Call(ExprCall {
            pub attrs: Vec<Attribute>,
            pub func: Box<Expr>,
            pub paren_token: token::Paren,
            pub args: Delimited<Expr, Token![,]>,
        }),

        /// A method call (`x.foo::<Bar, Baz>(a, b, c, d)`)
        ///
        /// The `Ident` is the identifier for the method name.
        /// The vector of `Type`s are the ascripted type parameters for the method
        /// (within the angle brackets).
        pub MethodCall(ExprMethodCall #full {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub dot_token: Token![.],
            pub method: Ident,
            pub colon2_token: Option<Token![::]>,
            pub lt_token: Option<Token![<]>,
            pub typarams: Delimited<Type, Token![,]>,
            pub gt_token: Option<Token![>]>,
            pub paren_token: token::Paren,
            pub args: Delimited<Expr, Token![,]>,
        }),

        /// A tuple, e.g. `(a, b, c, d)`.
        pub Tuple(ExprTuple #full {
            pub attrs: Vec<Attribute>,
            pub paren_token: token::Paren,
            pub args: Delimited<Expr, Token![,]>,
        }),

        /// A binary operation, e.g. `a + b`, `a * b`.
        pub Binary(ExprBinary {
            pub attrs: Vec<Attribute>,
            pub left: Box<Expr>,
            pub op: BinOp,
            pub right: Box<Expr>,
        }),

        /// A unary operation, e.g. `!x`, `*x`.
        pub Unary(ExprUnary {
            pub attrs: Vec<Attribute>,
            pub op: UnOp,
            pub expr: Box<Expr>,
        }),

        /// A literal, e.g. `1`, `"foo"`.
        pub Lit(ExprLit {
            pub attrs: Vec<Attribute>,
            pub lit: Lit,
        }),

        /// A cast, e.g. `foo as f64`.
        pub Cast(ExprCast {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub as_token: Token![as],
            pub ty: Box<Type>,
        }),

        /// A type ascription, e.g. `foo: f64`.
        pub Type(ExprType {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub colon_token: Token![:],
            pub ty: Box<Type>,
        }),

        /// An `if` block, with an optional else block
        ///
        /// E.g., `if expr { block } else { expr }`
        pub If(ExprIf #full {
            pub attrs: Vec<Attribute>,
            pub if_token: Token![if],
            pub cond: Box<Expr>,
            pub if_true: Block,
            pub else_token: Option<Token![else]>,
            pub if_false: Option<Box<Expr>>,
        }),

        /// An `if let` expression with an optional else block
        ///
        /// E.g., `if let pat = expr { block } else { expr }`
        ///
        /// This is desugared to a `match` expression.
        pub IfLet(ExprIfLet #full {
            pub attrs: Vec<Attribute>,
            pub if_token: Token![if],
            pub let_token: Token![let],
            pub pat: Box<Pat>,
            pub eq_token: Token![=],
            pub expr: Box<Expr>,
            pub if_true: Block,
            pub else_token: Option<Token![else]>,
            pub if_false: Option<Box<Expr>>,
        }),

        /// A while loop, with an optional label
        ///
        /// E.g., `'label: while expr { block }`
        pub While(ExprWhile #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Lifetime>,
            pub colon_token: Option<Token![:]>,
            pub while_token: Token![while],
            pub cond: Box<Expr>,
            pub body: Block,
        }),

        /// A while-let loop, with an optional label.
        ///
        /// E.g., `'label: while let pat = expr { block }`
        ///
        /// This is desugared to a combination of `loop` and `match` expressions.
        pub WhileLet(ExprWhileLet #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Lifetime>,
            pub colon_token: Option<Token![:]>,
            pub while_token: Token![while],
            pub let_token: Token![let],
            pub pat: Box<Pat>,
            pub eq_token: Token![=],
            pub expr: Box<Expr>,
            pub body: Block,
        }),

        /// A for loop, with an optional label.
        ///
        /// E.g., `'label: for pat in expr { block }`
        ///
        /// This is desugared to a combination of `loop` and `match` expressions.
        pub ForLoop(ExprForLoop #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Lifetime>,
            pub colon_token: Option<Token![:]>,
            pub for_token: Token![for],
            pub pat: Box<Pat>,
            pub in_token: Token![in],
            pub expr: Box<Expr>,
            pub body: Block,
        }),

        /// Conditionless loop with an optional label.
        ///
        /// E.g. `'label: loop { block }`
        pub Loop(ExprLoop #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Lifetime>,
            pub colon_token: Option<Token![:]>,
            pub loop_token: Token![loop],
            pub body: Block,
        }),

        /// A `match` block.
        pub Match(ExprMatch #full {
            pub attrs: Vec<Attribute>,
            pub match_token: Token![match],
            pub expr: Box<Expr>,
            pub brace_token: token::Brace,
            pub arms: Vec<Arm>,
        }),

        /// A closure (for example, `move |a, b, c| a + b + c`)
        pub Closure(ExprClosure #full {
            pub attrs: Vec<Attribute>,
            pub capture: CaptureBy,
            pub or1_token: Token![|],
            pub inputs: Delimited<FnArg, Token![,]>,
            pub or2_token: Token![|],
            pub output: ReturnType,
            pub body: Box<Expr>,
        }),

        /// An unsafe block (`unsafe { ... }`)
        pub Unsafe(ExprUnsafe #full {
            pub attrs: Vec<Attribute>,
            pub unsafe_token: Token![unsafe],
            pub block: Block,
        }),

        /// A block (`{ ... }`)
        pub Block(ExprBlock #full {
            pub attrs: Vec<Attribute>,
            pub block: Block,
        }),

        /// An assignment (`a = foo()`)
        pub Assign(ExprAssign #full {
            pub attrs: Vec<Attribute>,
            pub left: Box<Expr>,
            pub eq_token: Token![=],
            pub right: Box<Expr>,
        }),

        /// An assignment with an operator
        ///
        /// For example, `a += 1`.
        pub AssignOp(ExprAssignOp #full {
            pub attrs: Vec<Attribute>,
            pub left: Box<Expr>,
            pub op: BinOp,
            pub right: Box<Expr>,
        }),

        /// Access of a named struct field (`obj.foo`) or unnamed tuple struct
        /// field (`obj.0`).
        pub Field(ExprField #full {
            pub attrs: Vec<Attribute>,
            pub base: Box<Expr>,
            pub dot_token: Token![.],
            pub member: Member,
        }),

        /// An indexing operation (`foo[2]`)
        pub Index(ExprIndex {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub bracket_token: token::Bracket,
            pub index: Box<Expr>,
        }),

        /// A range (`1..2`, `1..`, `..2`, `1..=2`, `..=2`)
        pub Range(ExprRange #full {
            pub attrs: Vec<Attribute>,
            pub from: Option<Box<Expr>>,
            pub limits: RangeLimits,
            pub to: Option<Box<Expr>>,
        }),

        /// Variable reference, possibly containing `::` and/or type
        /// parameters, e.g. foo::bar::<baz>.
        ///
        /// Optionally "qualified",
        /// E.g. `<Vec<T> as SomeTrait>::SomeType`.
        pub Path(ExprPath {
            pub attrs: Vec<Attribute>,
            pub qself: Option<QSelf>,
            pub path: Path,
        }),

        /// A referencing operation (`&a` or `&mut a`)
        pub AddrOf(ExprAddrOf #full {
            pub attrs: Vec<Attribute>,
            pub and_token: Token![&],
            pub mutbl: Mutability,
            pub expr: Box<Expr>,
        }),

        /// A `break`, with an optional label to break, and an optional expression
        pub Break(ExprBreak #full {
            pub attrs: Vec<Attribute>,
            pub break_token: Token![break],
            pub label: Option<Lifetime>,
            pub expr: Option<Box<Expr>>,
        }),

        /// A `continue`, with an optional label
        pub Continue(ExprContinue #full {
            pub attrs: Vec<Attribute>,
            pub continue_token: Token![continue],
            pub label: Option<Lifetime>,
        }),

        /// A `return`, with an optional value to be returned
        pub Return(ExprReturn #full {
            pub attrs: Vec<Attribute>,
            pub return_token: Token![return],
            pub expr: Option<Box<Expr>>,
        }),

        /// A macro invocation; pre-expansion
        pub Macro(ExprMacro #full {
            pub attrs: Vec<Attribute>,
            pub mac: Macro,
        }),

        /// A struct literal expression.
        ///
        /// For example, `Foo {x: 1, y: 2}`, or
        /// `Foo {x: 1, .. base}`, where `base` is the `Option<Expr>`.
        pub Struct(ExprStruct #full {
            pub attrs: Vec<Attribute>,
            pub path: Path,
            pub brace_token: token::Brace,
            pub fields: Delimited<FieldValue, Token![,]>,
            pub dot2_token: Option<Token![..]>,
            pub rest: Option<Box<Expr>>,
        }),

        /// An array literal constructed from one repeated element.
        ///
        /// For example, `[1; 5]`. The first expression is the element
        /// to be repeated; the second is the number of times to repeat it.
        pub Repeat(ExprRepeat #full {
            pub attrs: Vec<Attribute>,
            pub bracket_token: token::Bracket,
            pub expr: Box<Expr>,
            pub semi_token: Token![;],
            pub amt: Box<Expr>,
        }),

        /// No-op: used solely so we can pretty-print faithfully
        pub Paren(ExprParen #full {
            pub attrs: Vec<Attribute>,
            pub paren_token: token::Paren,
            pub expr: Box<Expr>,
        }),

        /// No-op: used solely so we can pretty-print faithfully
        ///
        /// A `group` represents a `None`-delimited span in the input
        /// `TokenStream` which affects the precidence of the resulting
        /// expression. They are used for macro hygiene.
        pub Group(ExprGroup #full {
            pub attrs: Vec<Attribute>,
            pub group_token: token::Group,
            pub expr: Box<Expr>,
        }),

        /// `expr?`
        pub Try(ExprTry #full {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub question_token: Token![?],
        }),

        /// A catch expression.
        ///
        /// E.g. `do catch { block }`
        pub Catch(ExprCatch #full {
            pub attrs: Vec<Attribute>,
            pub do_token: Token![do],
            pub catch_token: Token![catch],
            pub block: Block,
        }),

        /// A yield expression.
        ///
        /// E.g. `yield expr`
        pub Yield(ExprYield #full {
            pub attrs: Vec<Attribute>,
            pub yield_token: Token![yield],
            pub expr: Option<Box<Expr>>,
        }),
    }
}

impl Expr {
    // Not public API.
    #[doc(hidden)]
    #[cfg(feature = "full")]
    pub fn attrs_mut(&mut self) -> &mut Vec<Attribute> {
        match *self {
            Expr::Box(ExprBox { ref mut attrs, .. }) |
            Expr::InPlace(ExprInPlace { ref mut attrs, .. }) |
            Expr::Array(ExprArray { ref mut attrs, .. }) |
            Expr::Call(ExprCall { ref mut attrs, .. }) |
            Expr::MethodCall(ExprMethodCall { ref mut attrs, .. }) |
            Expr::Tuple(ExprTuple { ref mut attrs, .. }) |
            Expr::Binary(ExprBinary { ref mut attrs, .. }) |
            Expr::Unary(ExprUnary { ref mut attrs, .. }) |
            Expr::Lit(ExprLit { ref mut attrs, .. }) |
            Expr::Cast(ExprCast { ref mut attrs, .. }) |
            Expr::Type(ExprType { ref mut attrs, .. }) |
            Expr::If(ExprIf { ref mut attrs, .. }) |
            Expr::IfLet(ExprIfLet { ref mut attrs, .. }) |
            Expr::While(ExprWhile { ref mut attrs, .. }) |
            Expr::WhileLet(ExprWhileLet { ref mut attrs, .. }) |
            Expr::ForLoop(ExprForLoop { ref mut attrs, .. }) |
            Expr::Loop(ExprLoop { ref mut attrs, .. }) |
            Expr::Match(ExprMatch { ref mut attrs, .. }) |
            Expr::Closure(ExprClosure { ref mut attrs, .. }) |
            Expr::Unsafe(ExprUnsafe { ref mut attrs, .. }) |
            Expr::Block(ExprBlock { ref mut attrs, .. }) |
            Expr::Assign(ExprAssign { ref mut attrs, .. }) |
            Expr::AssignOp(ExprAssignOp { ref mut attrs, .. }) |
            Expr::Field(ExprField { ref mut attrs, .. }) |
            Expr::Index(ExprIndex { ref mut attrs, .. }) |
            Expr::Range(ExprRange { ref mut attrs, .. }) |
            Expr::Path(ExprPath { ref mut attrs, .. }) |
            Expr::AddrOf(ExprAddrOf { ref mut attrs, .. }) |
            Expr::Break(ExprBreak { ref mut attrs, .. }) |
            Expr::Continue(ExprContinue { ref mut attrs, .. }) |
            Expr::Return(ExprReturn { ref mut attrs, .. }) |
            Expr::Macro(ExprMacro { ref mut attrs, .. }) |
            Expr::Struct(ExprStruct { ref mut attrs, .. }) |
            Expr::Repeat(ExprRepeat { ref mut attrs, .. }) |
            Expr::Paren(ExprParen { ref mut attrs, .. }) |
            Expr::Group(ExprGroup { ref mut attrs, .. }) |
            Expr::Try(ExprTry { ref mut attrs, .. }) |
            Expr::Catch(ExprCatch { ref mut attrs, .. }) |
            Expr::Yield(ExprYield { ref mut attrs, .. }) => attrs,
        }
    }
}

#[cfg(feature = "full")]
ast_enum! {
    /// A struct or tuple struct field accessed in a struct literal or field
    /// expression.
    pub enum Member {
        /// A named field like `self.x`.
        Named(Ident),
        /// An unnamed field like `self.0`.
        Unnamed(Index),
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// The index of an unnamed tuple struct field.
    pub struct Index #manual_extra_traits {
        pub index: u32,
        pub span: Span,
    }
}

#[cfg(feature = "full")]
impl Eq for Index {}

#[cfg(feature = "full")]
impl PartialEq for Index {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

#[cfg(feature = "full")]
impl Hash for Index {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A field-value pair in a struct literal.
    pub struct FieldValue {
        /// Attributes tagged on the field.
        pub attrs: Vec<Attribute>,

        /// Name or index of the field.
        pub member: Member,

        pub colon_token: Option<Token![:]>,

        /// Value of the field.
        pub expr: Expr,

        /// Whether this is a shorthand field, e.g. `Struct { x }`
        /// instead of `Struct { x: x }`.
        pub is_shorthand: bool,
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A Block (`{ .. }`).
    ///
    /// E.g. `{ .. }` as in `fn foo() { .. }`
    pub struct Block {
        pub brace_token: token::Brace,
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
        Semi(Box<Expr>, Token![;]),
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// Local represents a `let` statement, e.g., `let <pat>:<ty> = <expr>;`
    pub struct Local {
        pub attrs: Vec<Attribute>,
        pub let_token: Token![let],
        pub pat: Box<Pat>,
        pub colon_token: Option<Token![:]>,
        pub ty: Option<Box<Type>>,
        pub eq_token: Option<Token![=]>,
        /// Initializer expression to set the value, if any
        pub init: Option<Box<Expr>>,
        pub semi_token: Token![;],
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
            pub underscore_token: Token![_],
        }),

        /// A `Pat::Ident` may either be a new bound variable (`ref mut binding @ OPT_SUBPATTERN`),
        /// or a unit struct/variant pattern, or a const pattern (in the last two cases the third
        /// field must be `None`). Disambiguation cannot be done with parser alone, so it happens
        /// during name resolution.
        pub Ident(PatIdent {
            pub mode: BindingMode,
            pub ident: Ident,
            pub at_token: Option<Token![@]>,
            pub subpat: Option<Box<Pat>>,
        }),

        /// A struct or struct variant pattern, e.g. `Variant {x, y, ..}`.
        /// The `bool` is `true` in the presence of a `..`.
        pub Struct(PatStruct {
            pub path: Path,
            pub brace_token: token::Brace,
            pub fields: Delimited<FieldPat, Token![,]>,
            pub dot2_token: Option<Token![..]>,
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
            pub paren_token: token::Paren,
            pub pats: Delimited<Pat, Token![,]>,
            pub comma_token: Option<Token![,]>,
            pub dots_pos: Option<usize>,
            pub dot2_token: Option<Token![..]>,
        }),
        /// A `box` pattern
        pub Box(PatBox {
            pub box_token: Token![box],
            pub pat: Box<Pat>,
        }),
        /// A reference pattern, e.g. `&mut (a, b)`
        pub Ref(PatRef {
            pub and_token: Token![&],
            pub mutbl: Mutability,
            pub pat: Box<Pat>,
        }),
        /// A literal
        pub Lit(PatLit {
            pub expr: Box<Expr>,
        }),
        /// A range pattern, e.g. `1..=2`
        pub Range(PatRange {
            pub lo: Box<Expr>,
            pub limits: RangeLimits,
            pub hi: Box<Expr>,
        }),
        /// `[a, b, i.., y, z]` is represented as:
        pub Slice(PatSlice {
            pub bracket_token: token::Bracket,
            pub front: Delimited<Pat, Token![,]>,
            pub middle: Option<Box<Pat>>,
            pub comma_token: Option<Token![,]>,
            pub dot2_token: Option<Token![..]>,
            pub back: Delimited<Pat, Token![,]>,
        }),
        /// A macro pattern; pre-expansion
        pub Macro(Macro),
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// An arm of a 'match'.
    ///
    /// E.g. `0..=10 => { println!("match!") }` as in
    ///
    /// ```rust
    /// # #![feature(dotdoteq_in_patterns)]
    /// #
    /// # fn main() {
    /// #     let n = 0;
    /// match n {
    ///     0..=10 => { println!("match!") }
    ///     // ..
    ///     # _ => {}
    /// }
    /// # }
    /// ```
    pub struct Arm {
        pub attrs: Vec<Attribute>,
        pub pats: Delimited<Pat, Token![|]>,
        pub if_token: Option<Token![if]>,
        pub guard: Option<Box<Expr>>,
        pub rocket_token: Token![=>],
        pub body: Box<Expr>,
        pub comma: Option<Token![,]>,
    }
}

#[cfg(feature = "full")]
ast_enum! {
    /// A capture clause
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum CaptureBy {
        Value(Token![move]),
        Ref,
    }
}

#[cfg(feature = "full")]
ast_enum! {
    /// Limit types of a range (inclusive or exclusive)
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum RangeLimits {
        /// Inclusive at the beginning, exclusive at the end
        HalfOpen(Token![..]),
        /// Inclusive at the beginning and end
        Closed(Token![..=]),
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
        pub attrs: Vec<Attribute>,
        /// The identifier for the field
        pub member: Member,
        pub colon_token: Option<Token![:]>,
        /// The pattern the field is destructured to
        pub pat: Box<Pat>,
        pub is_shorthand: bool,
    }
}

#[cfg(feature = "full")]
ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum BindingMode {
        ByRef(Token![ref], Mutability),
        ByValue(Mutability),
    }
}

#[cfg(feature = "full")]
ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum InPlaceKind {
        Arrow(Token![<-]),
        In(Token![in]),
    }
}

#[cfg(any(feature = "parsing", feature = "printing"))]
#[cfg(feature = "full")]
fn arm_expr_requires_comma(expr: &Expr) -> bool {
    // see https://github.com/rust-lang/rust/blob/eb8f2586e
    //                       /src/libsyntax/parse/classify.rs#L17-L37
    match *expr {
        Expr::Unsafe(..)
        | Expr::Block(..)
        | Expr::If(..)
        | Expr::IfLet(..)
        | Expr::Match(..)
        | Expr::While(..)
        | Expr::WhileLet(..)
        | Expr::Loop(..)
        | Expr::ForLoop(..)
        | Expr::Catch(..) => false,
        _ => true,
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use ty::parsing::qpath;

    #[cfg(feature = "full")]
    use proc_macro2::{Delimiter, Span, TokenNode, TokenStream};
    use synom::Synom;
    use cursor::Cursor;
    #[cfg(feature = "full")]
    use parse_error;
    use synom::PResult;

    // When we're parsing expressions which occur before blocks, like in an if
    // statement's condition, we cannot parse a struct literal.
    //
    // Struct literals are ambiguous in certain positions
    // https://github.com/rust-lang/rfcs/pull/92
    macro_rules! ambiguous_expr {
        ($i:expr, $allow_struct:ident) => {
            ambiguous_expr($i, $allow_struct, true)
        };
    }

    // When we are parsing an optional suffix expression, we cannot allow blocks
    // if structs are not allowed.
    //
    // Example:
    //
    //     if break {} {}
    //
    // is ambiguous between:
    //
    //     if (break {}) {}
    //     if (break) {} {}
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

    // Parse an arbitrary expression.
    #[cfg(feature = "full")]
    fn ambiguous_expr(i: Cursor, allow_struct: bool, allow_block: bool) -> PResult<Expr> {
        call!(i, assign_expr, allow_struct, allow_block)
    }

    #[cfg(not(feature = "full"))]
    fn ambiguous_expr(i: Cursor, allow_struct: bool, allow_block: bool) -> PResult<Expr> {
        // NOTE: We intentionally skip assign_expr, placement_expr, and
        // range_expr, as they are not parsed in non-full mode.
        call!(i, or_expr, allow_struct, allow_block)
    }

    // Parse a left-associative binary operator.
    macro_rules! binop {
        (
            $name: ident,
            $next: ident,
            $submac: ident!( $($args:tt)* )
        ) => {
            named!($name(allow_struct: bool, allow_block: bool) -> Expr, do_parse!(
                mut e: call!($next, allow_struct, allow_block) >>
                many0!(do_parse!(
                    op: $submac!($($args)*) >>
                    rhs: call!($next, allow_struct, true) >>
                    ({
                        e = ExprBinary {
                            attrs: Vec::new(),
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

    // <placement> = <placement> ..
    // <placement> += <placement> ..
    // <placement> -= <placement> ..
    // <placement> *= <placement> ..
    // <placement> /= <placement> ..
    // <placement> %= <placement> ..
    // <placement> ^= <placement> ..
    // <placement> &= <placement> ..
    // <placement> |= <placement> ..
    // <placement> <<= <placement> ..
    // <placement> >>= <placement> ..
    //
    // NOTE: This operator is right-associative.
    #[cfg(feature = "full")]
    named!(assign_expr(allow_struct: bool, allow_block: bool) -> Expr, do_parse!(
        mut e: call!(placement_expr, allow_struct, allow_block) >>
        alt!(
            do_parse!(
                eq: punct!(=) >>
                // Recurse into self to parse right-associative operator.
                rhs: call!(assign_expr, allow_struct, true) >>
                ({
                    e = ExprAssign {
                        attrs: Vec::new(),
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
                        attrs: Vec::new(),
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

    // <range> <- <range> ..
    //
    // NOTE: The `in place { expr }` version of this syntax is parsed in
    // `atom_expr`, not here.
    //
    // NOTE: This operator is right-associative.
    #[cfg(feature = "full")]
    named!(placement_expr(allow_struct: bool, allow_block: bool) -> Expr, do_parse!(
        mut e: call!(range_expr, allow_struct, allow_block) >>
        alt!(
            do_parse!(
                arrow: punct!(<-) >>
                // Recurse into self to parse right-associative operator.
                rhs: call!(placement_expr, allow_struct, true) >>
                ({
                    e = ExprInPlace {
                        attrs: Vec::new(),
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

    // <or> ... <or> ..
    // <or> .. <or> ..
    // <or> ..
    //
    // NOTE: This is currently parsed oddly - I'm not sure of what the exact
    // rules are for parsing these expressions are, but this is not correct.
    // For example, `a .. b .. c` is not a legal expression. It should not
    // be parsed as either `(a .. b) .. c` or `a .. (b .. c)` apparently.
    //
    // NOTE: The form of ranges which don't include a preceding expression are
    // parsed by `atom_expr`, rather than by this function.
    #[cfg(feature = "full")]
    named!(range_expr(allow_struct: bool, allow_block: bool) -> Expr, do_parse!(
        mut e: call!(or_expr, allow_struct, allow_block) >>
        many0!(do_parse!(
            limits: syn!(RangeLimits) >>
            // We don't want to allow blocks here if we don't allow structs. See
            // the reasoning for `opt_ambiguous_expr!` above.
            hi: option!(call!(or_expr, allow_struct, allow_struct)) >>
            ({
                e = ExprRange {
                    attrs: Vec::new(),
                    from: Some(Box::new(e.into())),
                    limits: limits,
                    to: hi.map(|e| Box::new(e.into())),
                }.into();
            })
        )) >>
        (e)
    ));

    // <and> || <and> ...
    binop!(or_expr, and_expr, map!(punct!(||), BinOp::Or));

    // <compare> && <compare> ...
    binop!(and_expr, compare_expr, map!(punct!(&&), BinOp::And));

    // <bitor> == <bitor> ...
    // <bitor> != <bitor> ...
    // <bitor> >= <bitor> ...
    // <bitor> <= <bitor> ...
    // <bitor> > <bitor> ...
    // <bitor> < <bitor> ...
    //
    // NOTE: This operator appears to be parsed as left-associative, but errors
    // if it is used in a non-associative manner.
    binop!(
        compare_expr,
        bitor_expr,
        alt!(
        punct!(==) => { BinOp::Eq }
        |
        punct!(!=) => { BinOp::Ne }
        |
        // must be above Lt
        punct!(<=) => { BinOp::Le }
        |
        // must be above Gt
        punct!(>=) => { BinOp::Ge }
        |
        do_parse!(
            // Make sure that we don't eat the < part of a <- operator
            not!(punct!(<-)) >>
            t: punct!(<) >>
            (BinOp::Lt(t))
        )
        |
        punct!(>) => { BinOp::Gt }
    )
    );

    // <bitxor> | <bitxor> ...
    binop!(
        bitor_expr,
        bitxor_expr,
        do_parse!(not!(punct!(||)) >> not!(punct!(|=)) >> t: punct!(|) >> (BinOp::BitOr(t)))
    );

    // <bitand> ^ <bitand> ...
    binop!(
        bitxor_expr,
        bitand_expr,
        do_parse!(
            // NOTE: Make sure we aren't looking at ^=.
            not!(punct!(^=)) >> t: punct!(^) >> (BinOp::BitXor(t))
        )
    );

    // <shift> & <shift> ...
    binop!(
        bitand_expr,
        shift_expr,
        do_parse!(
            // NOTE: Make sure we aren't looking at && or &=.
            not!(punct!(&&)) >> not!(punct!(&=)) >> t: punct!(&) >> (BinOp::BitAnd(t))
        )
    );

    // <arith> << <arith> ...
    // <arith> >> <arith> ...
    binop!(
        shift_expr,
        arith_expr,
        alt!(
        punct!(<<) => { BinOp::Shl }
        |
        punct!(>>) => { BinOp::Shr }
    )
    );

    // <term> + <term> ...
    // <term> - <term> ...
    binop!(
        arith_expr,
        term_expr,
        alt!(
        punct!(+) => { BinOp::Add }
        |
        punct!(-) => { BinOp::Sub }
    )
    );

    // <cast> * <cast> ...
    // <cast> / <cast> ...
    // <cast> % <cast> ...
    binop!(
        term_expr,
        cast_expr,
        alt!(
        punct!(*) => { BinOp::Mul }
        |
        punct!(/) => { BinOp::Div }
        |
        punct!(%) => { BinOp::Rem }
    )
    );

    // <unary> as <ty>
    // <unary> : <ty>
    named!(cast_expr(allow_struct: bool, allow_block: bool) -> Expr, do_parse!(
        mut e: call!(unary_expr, allow_struct, allow_block) >>
        many0!(alt!(
            do_parse!(
                as_: keyword!(as) >>
                // We can't accept `A + B` in cast expressions, as it's
                // ambiguous with the + expression.
                ty: call!(Type::without_plus) >>
                ({
                    e = ExprCast {
                        attrs: Vec::new(),
                        expr: Box::new(e.into()),
                        as_token: as_,
                        ty: Box::new(ty),
                    }.into();
                })
            )
            |
            do_parse!(
                colon: punct!(:) >>
                // We can't accept `A + B` in cast expressions, as it's
                // ambiguous with the + expression.
                ty: call!(Type::without_plus) >>
                ({
                    e = ExprType {
                        attrs: Vec::new(),
                        expr: Box::new(e.into()),
                        colon_token: colon,
                        ty: Box::new(ty),
                    }.into();
                })
            )
        )) >>
        (e)
    ));

    // <UnOp> <trailer>
    // & <trailer>
    // &mut <trailer>
    // box <trailer>
    #[cfg(feature = "full")]
    named!(unary_expr(allow_struct: bool, allow_block: bool) -> Expr, alt!(
        do_parse!(
            op: syn!(UnOp) >>
            expr: call!(unary_expr, allow_struct, true) >>
            (ExprUnary {
                attrs: Vec::new(),
                op: op,
                expr: Box::new(expr.into()),
            }.into())
        )
        |
        do_parse!(
            and: punct!(&) >>
            mutability: syn!(Mutability) >>
            expr: call!(unary_expr, allow_struct, true) >>
            (ExprAddrOf {
                attrs: Vec::new(),
                and_token: and,
                mutbl: mutability,
                expr: Box::new(expr.into()),
            }.into())
        )
        |
        do_parse!(
            box_: keyword!(box) >>
            expr: call!(unary_expr, allow_struct, true) >>
            (ExprBox {
                attrs: Vec::new(),
                box_token: box_,
                expr: Box::new(expr.into()),
            }.into())
        )
        |
        call!(trailer_expr, allow_struct, allow_block)
    ));

    // XXX: This duplication is ugly
    #[cfg(not(feature = "full"))]
    named!(unary_expr(allow_struct: bool, allow_block: bool) -> Expr, alt!(
        do_parse!(
            op: syn!(UnOp) >>
            expr: call!(unary_expr, allow_struct, true) >>
            (ExprUnary {
                attrs: Vec::new(),
                op: op,
                expr: Box::new(expr.into()),
            }.into())
        )
        |
        call!(trailer_expr, allow_struct, allow_block)
    ));

    // <atom> (..<args>) ...
    // <atom> . <ident> (..<args>) ...
    // <atom> . <ident> ...
    // <atom> . <lit> ...
    // <atom> [ <expr> ] ...
    // <atom> ? ...
    #[cfg(feature = "full")]
    named!(trailer_expr(allow_struct: bool, allow_block: bool) -> Expr, do_parse!(
        mut e: call!(atom_expr, allow_struct, allow_block) >>
        many0!(alt!(
            tap!(args: and_call => {
                let (args, paren) = args;
                e = ExprCall {
                    attrs: Vec::new(),
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
                let (token, member) = field;
                e = ExprField {
                    attrs: Vec::new(),
                    base: Box::new(e.into()),
                    dot_token: token,
                    member: member,
                }.into();
            })
            |
            tap!(i: and_index => {
                let (i, token) = i;
                e = ExprIndex {
                    attrs: Vec::new(),
                    expr: Box::new(e.into()),
                    bracket_token: token,
                    index: Box::new(i),
                }.into();
            })
            |
            tap!(question: punct!(?) => {
                e = ExprTry {
                    attrs: Vec::new(),
                    expr: Box::new(e.into()),
                    question_token: question,
                }.into();
            })
        )) >>
        (e)
    ));

    // XXX: Duplication == ugly
    #[cfg(not(feature = "full"))]
    named!(trailer_expr(allow_struct: bool, allow_block: bool) -> Expr, do_parse!(
        mut e: call!(atom_expr, allow_struct, allow_block) >>
        many0!(alt!(
            tap!(args: and_call => {
                let (args, paren) = args;
                e = ExprCall {
                    attrs: Vec::new(),
                    func: Box::new(e.into()),
                    args: args,
                    paren_token: paren,
                }.into();
            })
            |
            tap!(i: and_index => {
                let (i, token) = i;
                e = ExprIndex {
                    attrs: Vec::new(),
                    expr: Box::new(e.into()),
                    bracket_token: token,
                    index: Box::new(i),
                }.into();
            })
        )) >>
        (e)
    ));

    // Parse all atomic expressions which don't have to worry about precidence
    // interactions, as they are fully contained.
    #[cfg(feature = "full")]
    named!(atom_expr(allow_struct: bool, allow_block: bool) -> Expr, alt!(
        syn!(ExprGroup) => { Expr::Group } // must be placed first
        |
        syn!(ExprLit) => { Expr::Lit } // must be before expr_struct
        |
        // must be before expr_path
        cond_reduce!(allow_struct, map!(syn!(ExprStruct), Expr::Struct))
        |
        syn!(ExprParen) => { Expr::Paren } // must be before expr_tup
        |
        syn!(ExprMacro) => { Expr::Macro } // must be before expr_path
        |
        call!(expr_break, allow_struct) // must be before expr_path
        |
        syn!(ExprContinue) => { Expr::Continue } // must be before expr_path
        |
        call!(expr_ret, allow_struct) // must be before expr_path
        |
        // NOTE: The `in place { expr }` form. `place <- expr` is parsed above.
        syn!(ExprInPlace) => { Expr::InPlace }
        |
        syn!(ExprArray) => { Expr::Array }
        |
        syn!(ExprTuple) => { Expr::Tuple }
        |
        syn!(ExprIf) => { Expr::If }
        |
        syn!(ExprIfLet) => { Expr::IfLet }
        |
        syn!(ExprWhile) => { Expr::While }
        |
        syn!(ExprWhileLet) => { Expr::WhileLet }
        |
        syn!(ExprForLoop) => { Expr::ForLoop }
        |
        syn!(ExprLoop) => { Expr::Loop }
        |
        syn!(ExprMatch) => { Expr::Match }
        |
        syn!(ExprCatch) => { Expr::Catch }
        |
        syn!(ExprYield) => { Expr::Yield }
        |
        syn!(ExprUnsafe) => { Expr::Unsafe }
        |
        call!(expr_closure, allow_struct)
        |
        cond_reduce!(allow_block, map!(syn!(ExprBlock), Expr::Block))
        |
        // NOTE: This is the prefix-form of range
        call!(expr_range, allow_struct)
        |
        syn!(ExprPath) => { Expr::Path }
        |
        syn!(ExprRepeat) => { Expr::Repeat }
    ));

    #[cfg(not(feature = "full"))]
    named!(atom_expr(_allow_struct: bool, _allow_block: bool) -> Expr, alt!(
        syn!(ExprLit) => { Expr::Lit }
        |
        syn!(ExprPath) => { Expr::Path }
    ));

    #[cfg(feature = "full")]
    named!(expr_nosemi -> Expr, map!(alt!(
        syn!(ExprIf) => { Expr::If }
        |
        syn!(ExprIfLet) => { Expr::IfLet }
        |
        syn!(ExprWhile) => { Expr::While }
        |
        syn!(ExprWhileLet) => { Expr::WhileLet }
        |
        syn!(ExprForLoop) => { Expr::ForLoop }
        |
        syn!(ExprLoop) => { Expr::Loop }
        |
        syn!(ExprMatch) => { Expr::Match }
        |
        syn!(ExprCatch) => { Expr::Catch }
        |
        syn!(ExprYield) => { Expr::Yield }
        |
        syn!(ExprUnsafe) => { Expr::Unsafe }
        |
        syn!(ExprBlock) => { Expr::Block }
    ), Expr::from));

    impl Synom for ExprLit {
        named!(parse -> Self, do_parse!(
            lit: syn!(Lit) >>
            (ExprLit {
                attrs: Vec::new(),
                lit: lit,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprMacro {
        named!(parse -> Self, do_parse!(
            mac: syn!(Macro) >>
            (ExprMacro {
                attrs: Vec::new(),
                mac: mac,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprGroup {
        named!(parse -> Self, do_parse!(
            e: grouped!(syn!(Expr)) >>
            (ExprGroup {
                attrs: Vec::new(),
                expr: Box::new(e.0),
                group_token: e.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprParen {
        named!(parse -> Self, do_parse!(
            e: parens!(syn!(Expr)) >>
            (ExprParen {
                attrs: Vec::new(),
                expr: Box::new(e.0),
                paren_token: e.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprInPlace {
        named!(parse -> Self, do_parse!(
            in_: keyword!(in) >>
            place: expr_no_struct >>
            value: braces!(call!(Block::parse_within)) >>
            (ExprInPlace {
                attrs: Vec::new(),
                place: Box::new(place),
                kind: InPlaceKind::In(in_),
                value: Box::new(ExprBlock {
                    attrs: Vec::new(),
                    block: Block {
                        stmts: value.0,
                        brace_token: value.1,
                    },
                }.into()),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprArray {
        named!(parse -> Self, do_parse!(
            elems: brackets!(call!(Delimited::parse_terminated)) >>
            (ExprArray {
                attrs: Vec::new(),
                exprs: elems.0,
                bracket_token: elems.1,
            })
        ));
    }

    named!(and_call -> (Delimited<Expr, Token![,]>, token::Paren),
           parens!(call!(Delimited::parse_terminated)));

    #[cfg(feature = "full")]
    named!(and_method_call -> ExprMethodCall, do_parse!(
        dot: punct!(.) >>
        method: syn!(Ident) >>
        typarams: option!(do_parse!(
            colon2: punct!(::) >>
            lt: punct!(<) >>
            tys: call!(Delimited::parse_terminated) >>
            gt: punct!(>) >>
            (colon2, lt, tys, gt)
        )) >>
        args: parens!(call!(Delimited::parse_terminated)) >>
        ({
            let (colon2, lt, tys, gt) = match typarams {
                Some((a, b, c, d)) => (Some(a), Some(b), Some(c), Some(d)),
                None => (None, None, None, None),
            };
            ExprMethodCall {
                attrs: Vec::new(),
                // this expr will get overwritten after being returned
                expr: Box::new(Expr::Lit(ExprLit {
                    attrs: Vec::new(),
                    lit: Lit {
                        span: Span::default(),
                        value: LitKind::Bool(false),
                    },
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
    impl Synom for ExprTuple {
        named!(parse -> Self, do_parse!(
            elems: parens!(call!(Delimited::parse_terminated)) >>
            (ExprTuple {
                attrs: Vec::new(),
                args: elems.0,
                paren_token: elems.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprIfLet {
        named!(parse -> Self, do_parse!(
            if_: keyword!(if) >>
            let_: keyword!(let) >>
            pat: syn!(Pat) >>
            eq: punct!(=) >>
            cond: expr_no_struct >>
            then_block: braces!(call!(Block::parse_within)) >>
            else_block: option!(else_block) >>
            (ExprIfLet {
                attrs: Vec::new(),
                pat: Box::new(pat),
                let_token: let_,
                eq_token: eq,
                expr: Box::new(cond),
                if_true: Block {
                    stmts: then_block.0,
                    brace_token: then_block.1,
                },
                if_token: if_,
                else_token: else_block.as_ref().map(|p| Token![else]((p.0).0)),
                if_false: else_block.map(|p| Box::new(p.1.into())),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprIf {
        named!(parse -> Self, do_parse!(
            if_: keyword!(if) >>
            cond: expr_no_struct >>
            then_block: braces!(call!(Block::parse_within)) >>
            else_block: option!(else_block) >>
            (ExprIf {
                attrs: Vec::new(),
                cond: Box::new(cond),
                if_true: Block {
                    stmts: then_block.0,
                    brace_token: then_block.1,
                },
                if_token: if_,
                else_token: else_block.as_ref().map(|p| Token![else]((p.0).0)),
                if_false: else_block.map(|p| Box::new(p.1.into())),
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(else_block -> (Token![else], Expr), do_parse!(
        else_: keyword!(else) >>
        expr: alt!(
            syn!(ExprIf) => { Expr::If }
            |
            syn!(ExprIfLet) => { Expr::IfLet }
            |
            do_parse!(
                else_block: braces!(call!(Block::parse_within)) >>
                (Expr::Block(ExprBlock {
                    attrs: Vec::new(),
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
            lbl: option!(tuple!(syn!(Lifetime), punct!(:))) >>
            for_: keyword!(for) >>
            pat: syn!(Pat) >>
            in_: keyword!(in) >>
            expr: expr_no_struct >>
            loop_block: syn!(Block) >>
            (ExprForLoop {
                attrs: Vec::new(),
                for_token: for_,
                in_token: in_,
                pat: Box::new(pat),
                expr: Box::new(expr),
                body: loop_block,
                colon_token: lbl.as_ref().map(|p| Token![:]((p.1).0)),
                label: lbl.map(|p| p.0),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprLoop {
        named!(parse -> Self, do_parse!(
            lbl: option!(tuple!(syn!(Lifetime), punct!(:))) >>
            loop_: keyword!(loop) >>
            loop_block: syn!(Block) >>
            (ExprLoop {
                attrs: Vec::new(),
                loop_token: loop_,
                body: loop_block,
                colon_token: lbl.as_ref().map(|p| Token![:]((p.1).0)),
                label: lbl.map(|p| p.0),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprMatch {
        named!(parse -> Self, do_parse!(
            match_: keyword!(match) >>
            obj: expr_no_struct >>
            res: braces!(many0!(Arm::parse)) >>
            ({
                let (arms, brace) = res;
                ExprMatch {
                    attrs: Vec::new(),
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
            do_: keyword!(do) >>
            catch_: keyword!(catch) >>
            catch_block: syn!(Block) >>
            (ExprCatch {
                attrs: Vec::new(),
                block: catch_block,
                do_token: do_,
                catch_token: catch_,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprYield {
        named!(parse -> Self, do_parse!(
            yield_: keyword!(yield) >>
            expr: option!(syn!(Expr)) >>
            (ExprYield {
                attrs: Vec::new(),
                yield_token: yield_,
                expr: expr.map(Box::new),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for Arm {
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::parse_outer) >>
            pats: call!(Delimited::parse_separated_nonempty) >>
            guard: option!(tuple!(keyword!(if), syn!(Expr))) >>
            rocket: punct!(=>) >>
            body: do_parse!(
                expr: alt!(expr_nosemi | syn!(Expr)) >>
                comma1: cond!(arm_expr_requires_comma(&expr), alt!(
                    map!(input_end!(), |_| None)
                    |
                    map!(punct!(,), Some)
                )) >>
                comma2: cond!(!arm_expr_requires_comma(&expr), option!(punct!(,))) >>
                (expr, comma1.and_then(|x| x).or_else(|| comma2.and_then(|x| x)))
            ) >>
            (Arm {
                rocket_token: rocket,
                if_token: guard.as_ref().map(|p| Token![if]((p.0).0)),
                attrs: attrs,
                pats: pats,
                guard: guard.map(|p| Box::new(p.1)),
                body: Box::new(body.0),
                comma: body.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(expr_closure(allow_struct: bool) -> Expr, do_parse!(
        capture: syn!(CaptureBy) >>
        or1: punct!(|) >>
        inputs: call!(Delimited::parse_terminated_with, fn_arg) >>
        or2: punct!(|) >>
        ret_and_body: alt!(
            do_parse!(
                arrow: punct!(->) >>
                ty: syn!(Type) >>
                body: syn!(Block) >>
                (ReturnType::Type(arrow, Box::new(ty)),
                 Expr::Block(ExprBlock {
                     attrs: Vec::new(),
                    block: body,
                }).into())
            )
            |
            map!(ambiguous_expr!(allow_struct), |e| (ReturnType::Default, e))
        ) >>
        (ExprClosure {
            attrs: Vec::new(),
            capture: capture,
            or1_token: or1,
            inputs: inputs,
            or2_token: or2,
            output: ret_and_body.0,
            body: Box::new(ret_and_body.1),
        }.into())
    ));

    #[cfg(feature = "full")]
    named!(fn_arg -> FnArg, do_parse!(
        pat: syn!(Pat) >>
        ty: option!(tuple!(punct!(:), syn!(Type))) >>
        ({
            if let Some((colon, ty)) = ty {
                FnArg::Captured(ArgCaptured {
                    pat: pat,
                    colon_token: colon,
                    ty: ty,
                })
            } else {
                FnArg::Inferred(pat)
            }
        })
    ));

    #[cfg(feature = "full")]
    impl Synom for ExprWhile {
        named!(parse -> Self, do_parse!(
            lbl: option!(tuple!(syn!(Lifetime), punct!(:))) >>
            while_: keyword!(while) >>
            cond: expr_no_struct >>
            while_block: syn!(Block) >>
            (ExprWhile {
                attrs: Vec::new(),
                while_token: while_,
                colon_token: lbl.as_ref().map(|p| Token![:]((p.1).0)),
                cond: Box::new(cond),
                body: while_block,
                label: lbl.map(|p| p.0),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprWhileLet {
        named!(parse -> Self, do_parse!(
            lbl: option!(tuple!(syn!(Lifetime), punct!(:))) >>
            while_: keyword!(while) >>
            let_: keyword!(let) >>
            pat: syn!(Pat) >>
            eq: punct!(=) >>
            value: expr_no_struct >>
            while_block: syn!(Block) >>
            (ExprWhileLet {
                attrs: Vec::new(),
                eq_token: eq,
                let_token: let_,
                while_token: while_,
                colon_token: lbl.as_ref().map(|p| Token![:]((p.1).0)),
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
            cont: keyword!(continue) >>
            lbl: option!(syn!(Lifetime)) >>
            (ExprContinue {
                attrs: Vec::new(),
                continue_token: cont,
                label: lbl,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(expr_break(allow_struct: bool) -> Expr, do_parse!(
        break_: keyword!(break) >>
        lbl: option!(syn!(Lifetime)) >>
        // We can't allow blocks after a `break` expression when we wouldn't
        // allow structs, as this expression is ambiguous.
        val: opt_ambiguous_expr!(allow_struct) >>
        (ExprBreak {
            attrs: Vec::new(),
            label: lbl,
            expr: val.map(Box::new),
            break_token: break_,
        }.into())
    ));

    #[cfg(feature = "full")]
    named!(expr_ret(allow_struct: bool) -> Expr, do_parse!(
        return_: keyword!(return) >>
        // NOTE: return is greedy and eats blocks after it even when in a
        // position where structs are not allowed, such as in if statement
        // conditions. For example:
        //
        // if return { println!("A") } {} // Prints "A"
        ret_value: option!(ambiguous_expr!(allow_struct)) >>
        (ExprReturn {
            attrs: Vec::new(),
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
                            dots: punct!(..) >>
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
                    attrs: Vec::new(),
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
                member: syn!(Member) >>
                colon: punct!(:) >>
                value: syn!(Expr) >>
                (FieldValue {
                    member: member,
                    expr: value,
                    is_shorthand: false,
                    attrs: Vec::new(),
                    colon_token: Some(colon),
                })
            )
            |
            map!(syn!(Ident), |name| FieldValue {
                member: Member::Named(name),
                expr: Expr::Path(ExprPath {
                    attrs: Vec::new(),
                    qself: None,
                    path: name.into(),
                }).into(),
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
                semi: punct!(;) >>
                times: syn!(Expr) >>
                (value, semi, times)
            )) >>
            (ExprRepeat {
                attrs: Vec::new(),
                expr: Box::new((data.0).0),
                amt: Box::new((data.0).2),
                bracket_token: data.1,
                semi_token: (data.0).1,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprUnsafe {
        named!(parse -> Self, do_parse!(
            unsafe_: keyword!(unsafe) >>
            b: syn!(Block) >>
            (ExprUnsafe {
                attrs: Vec::new(),
                unsafe_token: unsafe_,
                block: b,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprBlock {
        named!(parse -> Self, do_parse!(
            b: syn!(Block) >>
            (ExprBlock {
                attrs: Vec::new(),
                block: b,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(expr_range(allow_struct: bool) -> Expr, do_parse!(
        limits: syn!(RangeLimits) >>
        hi: opt_ambiguous_expr!(allow_struct) >>
        (ExprRange {
            attrs: Vec::new(),
            from: None,
            to: hi.map(Box::new),
            limits: limits,
        }.into())
    ));

    #[cfg(feature = "full")]
    impl Synom for RangeLimits {
        named!(parse -> Self, alt!(
            // Must come before Dot2
            punct!(..=) => { RangeLimits::Closed }
            |
            // Must come before Dot2
            punct!(...) => { |dot3| RangeLimits::Closed(Token![..=](dot3.0)) }
            |
            punct!(..) => { RangeLimits::HalfOpen }
        ));
    }

    impl Synom for ExprPath {
        named!(parse -> Self, do_parse!(
            pair: qpath >>
            (ExprPath {
                attrs: Vec::new(),
                qself: pair.0,
                path: pair.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    named!(and_field -> (Token![.], Member), tuple!(punct!(.), syn!(Member)));

    named!(and_index -> (Expr, token::Bracket), brackets!(syn!(Expr)));

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
            many0!(punct!(;)) >>
            mut standalone: many0!(terminated!(syn!(Stmt), many0!(punct!(;)))) >>
            last: option!(do_parse!(
                attrs: many0!(Attribute::parse_outer) >>
                mut e: syn!(Expr) >>
                ({
                    *e.attrs_mut() = attrs;
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
        attrs: many0!(Attribute::parse_outer) >>
        what: syn!(Path) >>
        bang: punct!(!) >>
    // Only parse braces here; paren and bracket will get parsed as
    // expression statements
        data: braces!(syn!(TokenStream)) >>
        semi: option!(punct!(;)) >>
        (Stmt::Item(Box::new(Item::Macro(ItemMacro {
            attrs: attrs,
            ident: None,
            mac: Macro {
                path: what,
                bang_token: bang,
                tokens: proc_macro2::TokenTree {
                    span: (data.1).0,
                    kind: TokenNode::Group(Delimiter::Brace, data.0),
                },
            },
            semi_token: semi,
        }))))
    ));

    #[cfg(feature = "full")]
    named!(stmt_local -> Stmt, do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        let_: keyword!(let) >>
        pat: syn!(Pat) >>
        ty: option!(tuple!(punct!(:), syn!(Type))) >>
        init: option!(tuple!(punct!(=), syn!(Expr))) >>
        semi: punct!(;) >>
        (Stmt::Local(Box::new(Local {
            let_token: let_,
            semi_token: semi,
            colon_token: ty.as_ref().map(|p| Token![:]((p.0).0)),
            eq_token: init.as_ref().map(|p| Token![=]((p.0).0)),
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
        attrs: many0!(Attribute::parse_outer) >>
        mut e: expr_nosemi >>
        // If the next token is a `.` or a `?` it is special-cased to parse as
        // an expression instead of a blockexpression.
        not!(punct!(.)) >>
        not!(punct!(?)) >>
        semi: option!(punct!(;)) >>
        ({
            *e.attrs_mut() = attrs;
            if let Some(semi) = semi {
                Stmt::Semi(Box::new(e), semi)
            } else {
                Stmt::Expr(Box::new(e))
            }
        })
    ));

    #[cfg(feature = "full")]
    named!(stmt_expr -> Stmt, do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        mut e: syn!(Expr) >>
        semi: punct!(;) >>
        ({
            *e.attrs_mut() = attrs;
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
            syn!(Macro) => { Pat::Macro } // must be before pat_ident
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
            punct!(_),
            |u| PatWild { underscore_token: u }
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatBox {
        named!(parse -> Self, do_parse!(
            boxed: keyword!(box) >>
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
            mode: option!(keyword!(ref)) >>
            mutability: syn!(Mutability) >>
            name: alt!(
                syn!(Ident)
                |
                keyword!(self) => { Into::into }
            ) >>
            not!(punct!(<)) >>
            not!(punct!(::)) >>
            subpat: option!(tuple!(punct!(@), syn!(Pat))) >>
            (PatIdent {
                mode: match mode {
                    Some(mode) => BindingMode::ByRef(mode, mutability),
                    None => BindingMode::ByValue(mutability),
                },
                ident: name,
                at_token: subpat.as_ref().map(|p| Token![@]((p.0).0)),
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
                          punct!(..))
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
                member: syn!(Member) >>
                colon: punct!(:) >>
                pat: syn!(Pat) >>
                (FieldPat {
                    member: member,
                    pat: Box::new(pat),
                    is_shorthand: false,
                    attrs: Vec::new(),
                    colon_token: Some(colon),
                })
            )
            |
            do_parse!(
                boxed: option!(keyword!(box)) >>
                mode: option!(keyword!(ref)) >>
                mutability: syn!(Mutability) >>
                ident: syn!(Ident) >>
                ({
                    let mut pat: Pat = PatIdent {
                        mode: if let Some(mode) = mode {
                            BindingMode::ByRef(mode, mutability)
                        } else {
                            BindingMode::ByValue(mutability)
                        },
                        ident: ident,
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
                        member: Member::Named(ident),
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
    impl Synom for Member {
        named!(parse -> Self, alt!(
            syn!(Ident) => { Member::Named }
            |
            syn!(Index) => { Member::Unnamed }
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for Index {
        named!(parse -> Self, do_parse!(
            lit: syn!(Lit) >>
            ({
                if let Ok(i) = lit.value.to_string().parse() {
                    Index { index: i, span: lit.span }
                } else {
                    return parse_error();
                }
            })
        ));
    }

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
                        dots: punct!(..) >>
                        trailing: option!(punct!(,)) >>
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
            and: punct!(&) >>
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
            (if let Expr::Path(_) = lit {
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
        neg: option!(punct!(-)) >>
        v: alt!(
            syn!(ExprLit) => { Expr::Lit }
            |
            syn!(ExprPath) => { Expr::Path }
        ) >>
        (if let Some(neg) = neg {
            Expr::Unary(ExprUnary {
                attrs: Vec::new(),
                op: UnOp::Neg(neg),
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
                    dots: punct!(..) >>
                    trailing: option!(punct!(,)) >>
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
                let mut before: Delimited<Pat, Token![,]> = before;
                let after: Option<Delimited<Pat, Token![,]>> = after;
                let middle: Option<(Token![..], Option<Token![,]>)> = middle;
                PatSlice {
                    dot2_token: middle.as_ref().map(|m| Token![..]((m.0).0)),
                    comma_token: middle.as_ref().and_then(|m| {
                        m.1.as_ref().map(|m| Token![,](m.0))
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
            keyword!(move) => { CaptureBy::Value }
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
    use quote::{ToTokens, Tokens};
    #[cfg(feature = "full")]
    use proc_macro2::{TokenTree, TokenNode, Literal};

    // If the given expression is a bare `ExprStruct`, wraps it in parenthesis
    // before appending it to `Tokens`.
    #[cfg(feature = "full")]
    fn wrap_bare_struct(tokens: &mut Tokens, e: &Expr) {
        if let Expr::Struct(_) = *e {
            token::Paren::default().surround(tokens, |tokens| {
                e.to_tokens(tokens);
            });
        } else {
            e.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    fn attrs_to_tokens(attrs: &[Attribute], tokens: &mut Tokens) {
        tokens.append_all(attrs.outer());
    }

    #[cfg(not(feature = "full"))]
    fn attrs_to_tokens(_attrs: &[Attribute], _tokens: &mut Tokens) {
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprBox {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.box_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprInPlace {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
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
                    if let Expr::Block(_) = *self.value {
                        self.value.to_tokens(tokens);
                    } else {
                        token::Brace::default().surround(tokens, |tokens| {
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
            tokens.append_all(self.attrs.outer());
            self.bracket_token.surround(tokens, |tokens| {
                self.exprs.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprCall {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.func.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprMethodCall {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
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
    impl ToTokens for ExprTuple {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
                // If we only have one argument, we need a trailing comma to
                // distinguish ExprTuple from ExprParen.
                if self.args.len() == 1 && !self.args.trailing_delim() {
                    <Token![,]>::default().to_tokens(tokens);
                }
            })
        }
    }

    impl ToTokens for ExprBinary {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprUnary {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.op.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprLit {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.lit.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprCast {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.expr.to_tokens(tokens);
            self.as_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprType {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.expr.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    fn maybe_wrap_else(
        tokens: &mut Tokens,
        else_token: &Option<Token![else]>,
        if_false: &Option<Box<Expr>>,
    ) {
        if let Some(ref if_false) = *if_false {
            TokensOrDefault(else_token).to_tokens(tokens);

            // If we are not one of the valid expressions to exist in an else
            // clause, wrap ourselves in a block.
            match **if_false {
                Expr::If(_) | Expr::IfLet(_) | Expr::Block(_) => {
                    if_false.to_tokens(tokens);
                }
                _ => {
                    token::Brace::default().surround(tokens, |tokens| {
                        if_false.to_tokens(tokens);
                    });
                }
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprIf {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.if_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.cond);
            self.if_true.to_tokens(tokens);
            maybe_wrap_else(tokens, &self.else_token, &self.if_false);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprIfLet {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
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
            tokens.append_all(self.attrs.outer());
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
            tokens.append_all(self.attrs.outer());
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
            tokens.append_all(self.attrs.outer());
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
            tokens.append_all(self.attrs.outer());
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
            tokens.append_all(self.attrs.outer());
            self.match_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.brace_token.surround(tokens, |tokens| {
                for (i, arm) in self.arms.iter().enumerate() {
                    arm.to_tokens(tokens);
                    // Ensure that we have a comma after a non-block arm, except
                    // for the last one.
                    let is_last = i == self.arms.len() - 1;
                    if !is_last && arm_expr_requires_comma(&arm.body) && arm.comma.is_none() {
                        <Token![,]>::default().to_tokens(tokens);
                    }
                }
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprCatch {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.do_token.to_tokens(tokens);
            self.catch_token.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprYield {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.yield_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprClosure {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.capture.to_tokens(tokens);
            self.or1_token.to_tokens(tokens);
            for item in self.inputs.iter() {
                match **item.item() {
                    FnArg::Captured(ArgCaptured {
                        ref pat,
                        ty: Type::Infer(_),
                        ..
                    }) => {
                        pat.to_tokens(tokens);
                    }
                    _ => item.item().to_tokens(tokens),
                }
                item.delimiter().to_tokens(tokens);
            }
            self.or2_token.to_tokens(tokens);
            self.output.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprUnsafe {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.unsafe_token.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprBlock {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.block.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprAssign {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.left.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprAssignOp {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprField {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.base.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.member.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Member {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Member::Named(ident) => ident.to_tokens(tokens),
                Member::Unnamed(ref index) => index.to_tokens(tokens),
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Index {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append(TokenTree {
                span: self.span,
                kind: TokenNode::Literal(Literal::integer(i64::from(self.index))),
            });
        }
    }

    impl ToTokens for ExprIndex {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.expr.to_tokens(tokens);
            self.bracket_token.surround(tokens, |tokens| {
                self.index.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprRange {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.from.to_tokens(tokens);
            match self.limits {
                RangeLimits::HalfOpen(ref t) => t.to_tokens(tokens),
                RangeLimits::Closed(ref t) => t.to_tokens(tokens),
            }
            self.to.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprPath {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            ::PathTokens(&self.qself, &self.path).to_tokens(tokens)
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprAddrOf {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.and_token.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprBreak {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.break_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprContinue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.continue_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprReturn {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.return_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprMacro {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.mac.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprStruct {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
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
            tokens.append_all(self.attrs.outer());
            self.bracket_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
                self.amt.to_tokens(tokens);
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprGroup {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.group_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprParen {
        fn to_tokens(&self, tokens: &mut Tokens) {
            attrs_to_tokens(&self.attrs, tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprTry {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.expr.to_tokens(tokens);
            self.question_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for FieldValue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.member.to_tokens(tokens);
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
                    <Token![,]>::default().to_tokens(tokens);
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
                        <Token![,]>::default().to_tokens(tokens);
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
            match self.limits {
                RangeLimits::HalfOpen(ref t) => t.to_tokens(tokens),
                RangeLimits::Closed(ref t) => Token![...](t.0).to_tokens(tokens),
            }
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
                if !self.front.empty_or_trailing()
                    && (self.middle.is_some() || self.dot2_token.is_some())
                {
                    <Token![,]>::default().to_tokens(tokens);
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
    impl ToTokens for FieldPat {
        fn to_tokens(&self, tokens: &mut Tokens) {
            // XXX: Override is_shorthand if it was wrong?
            if !self.is_shorthand {
                self.member.to_tokens(tokens);
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
