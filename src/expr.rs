use super::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// A `box x` expression.
    Box(Box<Expr>),
    /// An array (`[a, b, c, d]`)
    Vec(Vec<Expr>),
    /// A function call
    ///
    /// The first field resolves to the function itself,
    /// and the second field is the list of arguments
    Call(Box<Expr>, Vec<Expr>),
    /// A method call (`x.foo::<Bar, Baz>(a, b, c, d)`)
    ///
    /// The `Ident` is the identifier for the method name.
    /// The vector of `Ty`s are the ascripted type parameters for the method
    /// (within the angle brackets).
    ///
    /// The first element of the vector of `Expr`s is the expression that evaluates
    /// to the object on which the method is being called on (the receiver),
    /// and the remaining elements are the rest of the arguments.
    ///
    /// Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
    /// `ExprKind::MethodCall(foo, [Bar, Baz], [x, a, b, c, d])`.
    MethodCall(Ident, Vec<Ty>, Vec<Expr>),
    /// A tuple (`(a, b, c, d)`)
    Tup(Vec<Expr>),
    /// A binary operation (For example: `a + b`, `a * b`)
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// A unary operation (For example: `!x`, `*x`)
    Unary(UnOp, Box<Expr>),
    /// A literal (For example: `1`, `"foo"`)
    Lit(Lit),
    /// A cast (`foo as f64`)
    Cast(Box<Expr>, Box<Ty>),
    /// Type ascription (`foo: f64`)
    Type(Box<Expr>, Box<Ty>),
    /// An `if` block, with an optional else block
    ///
    /// `if expr { block } else { expr }`
    If(Box<Expr>, Box<Block>, Option<Box<Expr>>),
    /// An `if let` expression with an optional else block
    ///
    /// `if let pat = expr { block } else { expr }`
    ///
    /// This is desugared to a `match` expression.
    IfLet(Box<Pat>, Box<Expr>, Box<Block>, Option<Box<Expr>>),
    /// A while loop, with an optional label
    ///
    /// `'label: while expr { block }`
    While(Box<Expr>, Box<Block>, Option<Ident>),
    /// A while-let loop, with an optional label
    ///
    /// `'label: while let pat = expr { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    WhileLet(Box<Pat>, Box<Expr>, Box<Block>, Option<Ident>),
    /// A for loop, with an optional label
    ///
    /// `'label: for pat in expr { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    ForLoop(Box<Pat>, Box<Expr>, Box<Block>, Option<Ident>),
    /// Conditionless loop (can be exited with break, continue, or return)
    ///
    /// `'label: loop { block }`
    Loop(Box<Block>, Option<Ident>),
    /// A `match` block.
    Match(Box<Expr>, Vec<Arm>),
    /// A closure (for example, `move |a, b, c| {a + b + c}`)
    Closure(CaptureBy, Box<FnDecl>, Box<Block>),
    /// A block (`{ ... }`)
    Block(Box<Block>),

    /// An assignment (`a = foo()`)
    Assign(Box<Expr>, Box<Expr>),
    /// An assignment with an operator
    ///
    /// For example, `a += 1`.
    AssignOp(BinOp, Box<Expr>, Box<Expr>),
    /// Access of a named struct field (`obj.foo`)
    Field(Box<Expr>, Ident),
    /// Access of an unnamed field of a struct or tuple-struct
    ///
    /// For example, `foo.0`.
    TupField(Box<Expr>, usize),
    /// An indexing operation (`foo[2]`)
    Index(Box<Expr>, Box<Expr>),
    /// A range (`1..2`, `1..`, `..2`, `1...2`, `1...`, `...2`)
    Range(Option<Box<Expr>>, Option<Box<Expr>>, RangeLimits),

    /// Variable reference, possibly containing `::` and/or type
    /// parameters, e.g. foo::bar::<baz>.
    ///
    /// Optionally "qualified",
    /// E.g. `<Vec<T> as SomeTrait>::SomeType`.
    Path(Option<QSelf>, Path),

    /// A referencing operation (`&a` or `&mut a`)
    AddrOf(Mutability, Box<Expr>),
    /// A `break`, with an optional label to break
    Break(Option<Ident>),
    /// A `continue`, with an optional label
    Continue(Option<Ident>),
    /// A `return`, with an optional value to be returned
    Ret(Option<Box<Expr>>),

    /// A macro invocation; pre-expansion
    Mac(Mac),

    /// A struct literal expression.
    ///
    /// For example, `Foo {x: 1, y: 2}`, or
    /// `Foo {x: 1, .. base}`, where `base` is the `Option<Expr>`.
    Struct(Path, Vec<Field>, Option<Box<Expr>>),

    /// An array literal constructed from one repeated element.
    ///
    /// For example, `[1; 5]`. The first expression is the element
    /// to be repeated; the second is the number of times to repeat it.
    Repeat(Box<Expr>, Box<Expr>),

    /// No-op: used solely so we can pretty-print faithfully
    Paren(Box<Expr>),

    /// `expr?`
    Try(Box<Expr>),
}

/// A Block (`{ .. }`).
///
/// E.g. `{ .. }` as in `fn foo() { .. }`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block {
    /// Statements in a block
    pub stmts: Vec<Stmt>,
    /// Distinguishes between `unsafe { ... }` and `{ ... }`
    pub rules: BlockCheckMode,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BlockCheckMode {
    Default,
    Unsafe,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    /// A local (let) binding.
    Local(Box<Local>),

    /// An item definition.
    Item(Box<Item>),

    /// Expr without trailing semi-colon.
    Expr(Box<Expr>),

    Semi(Box<Expr>),

    Mac(Box<(Mac, MacStmtStyle, Vec<Attribute>)>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MacStmtStyle {
    /// The macro statement had a trailing semicolon, e.g. `foo! { ... };`
    /// `foo!(...);`, `foo![...];`
    Semicolon,
    /// The macro statement had braces; e.g. foo! { ... }
    Braces,
    /// The macro statement had parentheses or brackets and no semicolon; e.g.
    /// `foo!(...)`. All of these will end up being converted into macro
    /// expressions.
    NoBraces,
}

/// Local represents a `let` statement, e.g., `let <pat>:<ty> = <expr>;`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Local {
    pub pat: Box<Pat>,
    pub ty: Option<Box<Ty>>,
    /// Initializer expression to set the value, if any
    pub init: Option<Box<Expr>>,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinOp {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Rem,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `^` operator (bitwise xor)
    BitXor,
    /// The `&` operator (bitwise and)
    BitAnd,
    /// The `|` operator (bitwise or)
    BitOr,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnOp {
    /// The `*` operator for dereferencing
    Deref,
    /// The `!` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pat {
    /// Represents a wildcard pattern (`_`)
    Wild,

    /// A `Pat::Ident` may either be a new bound variable (`ref mut binding @ OPT_SUBPATTERN`),
    /// or a unit struct/variant pattern, or a const pattern (in the last two cases the third
    /// field must be `None`). Disambiguation cannot be done with parser alone, so it happens
    /// during name resolution.
    Ident(BindingMode, Ident, Option<Box<Pat>>),

    /// A struct or struct variant pattern, e.g. `Variant {x, y, ..}`.
    /// The `bool` is `true` in the presence of a `..`.
    Struct(Path, Vec<FieldPat>, bool),

    /// A tuple struct/variant pattern `Variant(x, y, .., z)`.
    /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
    /// 0 <= position <= subpats.len()
    TupleStruct(Path, Vec<Pat>, Option<usize>),

    /// A possibly qualified path pattern.
    /// Unquailfied path patterns `A::B::C` can legally refer to variants, structs, constants
    /// or associated constants. Quailfied path patterns `<A>::B::C`/`<A as Trait>::B::C` can
    /// only legally refer to associated constants.
    Path(Option<QSelf>, Path),

    /// A tuple pattern `(a, b)`.
    /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
    /// 0 <= position <= subpats.len()
    Tuple(Vec<Pat>, Option<usize>),
    /// A `box` pattern
    Box(Box<Pat>),
    /// A reference pattern, e.g. `&mut (a, b)`
    Ref(Box<Pat>, Mutability),
    /// A literal
    Lit(Box<Expr>),
    /// A range pattern, e.g. `1...2`
    Range(Box<Expr>, Box<Expr>),
    /// `[a, b, ..i, y, z]` is represented as:
    ///     `Pat::Vec(box [a, b], Some(i), box [y, z])`
    Vec(Vec<Pat>, Option<Box<Pat>>, Vec<Pat>),
    /// A macro pattern; pre-expansion
    Mac(Mac),
}

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
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Arm {
    pub attrs: Vec<Attribute>,
    pub pats: Vec<Pat>,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
}

/// A capture clause
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CaptureBy {
    Value,
    Ref,
}

/// Limit types of a range (inclusive or exclusive)
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum RangeLimits {
    /// Inclusive at the beginning, exclusive at the end
    HalfOpen,
    /// Inclusive at the beginning and end
    Closed,
}

/// A single field in a struct pattern
///
/// Patterns like the fields of Foo `{ x, ref y, ref mut z }`
/// are treated the same as `x: x, y: ref y, z: ref mut z`,
/// except `is_shorthand` is true
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FieldPat {
    /// The identifier for the field
    pub ident: Ident,
    /// The pattern the field is destructured to
    pub pat: Box<Pat>,
    pub is_shorthand: bool,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BindingMode {
    ByRef(Mutability),
    ByValue(Mutability),
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use {Ident, Ty};
    use generics::parsing::lifetime;
    use ident::parsing::ident;
    use lit::parsing::lit;
    use nom::multispace;
    use ty::parsing::ty;

    named!(pub expr -> Expr, do_parse!(
        mut e: alt!(
            expr_box
            |
            expr_vec
            |
            expr_tup
            |
            expr_unary
            |
            expr_lit
            |
            expr_if
            // TODO: IfLet
            |
            expr_while
            // TODO: WhileLet
            // TODO: ForLoop
            // TODO: Loop
            // TODO: ForLoop
            |
            expr_loop
            // TODO: Match
            // TODO: Closure
            |
            expr_block
            // TODO: Path
            // TODO: AddrOf
            // TODO: Break
            // TODO: Continue
            // TODO: Ret
            // TODO: Mac
            // TODO: Struct
            // TODO: Repeat
            // TODO: Pparen
        ) >>
        many0!(alt!(
            tap!(args: and_call => {
                e = Expr::Call(Box::new(e), args);
            })
            |
            tap!(more: and_method_call => {
                let (method, ascript, mut args) = more;
                args.insert(0, e);
                e = Expr::MethodCall(method, ascript, args);
            })
            |
            tap!(more: and_binary => {
                let (op, other) = more;
                e = Expr::Binary(op, Box::new(e), Box::new(other));
            })
            |
            tap!(ty: and_cast => {
                e = Expr::Cast(Box::new(e), Box::new(ty));
            })
            |
            tap!(ty: and_ascription => {
                e = Expr::Type(Box::new(e), Box::new(ty));
            })
            // TODO: Assign
            // TODO: AssignOp
            // TODO: Field
            // TODO: TupField
            // TODO: Index
            // TODO: Range
            // TODO: Try
        )) >>
        (e)
    ));

    named!(expr_box -> Expr, do_parse!(
        punct!("box") >>
        multispace >>
        inner: expr >>
        (Expr::Box(Box::new(inner)))
    ));

    named!(expr_vec -> Expr, do_parse!(
        punct!("[") >>
        elems: separated_list!(punct!(","), expr) >>
        punct!("]") >>
        (Expr::Vec(elems))
    ));

    named!(and_call -> Vec<Expr>, do_parse!(
        punct!("(") >>
        args: separated_list!(punct!(","), expr) >>
        punct!(")") >>
        (args)
    ));

    named!(and_method_call -> (Ident, Vec<Ty>, Vec<Expr>), do_parse!(
        punct!(".") >>
        method: ident >>
        ascript: opt_vec!(delimited!(
            punct!("<"),
            separated_list!(punct!(","), ty),
            punct!(">")
        )) >>
        punct!("(") >>
        args: separated_list!(punct!(","), expr) >>
        punct!(")") >>
        (method, ascript, args)
    ));

    named!(expr_tup -> Expr, do_parse!(
        punct!("(") >>
        elems: separated_list!(punct!(","), expr) >>
        punct!(")") >>
        (Expr::Tup(elems))
    ));

    named!(and_binary -> (BinOp, Expr), tuple!(
        alt!(
            punct!("&&") => { |_| BinOp::And }
            |
            punct!("||") => { |_| BinOp::Or }
            |
            punct!("<<") => { |_| BinOp::Shl }
            |
            punct!(">>") => { |_| BinOp::Shr }
            |
            punct!("==") => { |_| BinOp::Eq }
            |
            punct!("<=") => { |_| BinOp::Le }
            |
            punct!("!=") => { |_| BinOp::Ne }
            |
            punct!(">=") => { |_| BinOp::Ge }
            |
            punct!("+") => { |_| BinOp::Add }
            |
            punct!("-") => { |_| BinOp::Sub }
            |
            punct!("*") => { |_| BinOp::Mul }
            |
            punct!("/") => { |_| BinOp::Div }
            |
            punct!("%") => { |_| BinOp::Rem }
            |
            punct!("^") => { |_| BinOp::BitXor }
            |
            punct!("&") => { |_| BinOp::BitAnd }
            |
            punct!("|") => { |_| BinOp::BitOr }
            |
            punct!("<") => { |_| BinOp::Lt }
            |
            punct!(">") => { |_| BinOp::Gt }
        ),
        expr
    ));

    named!(expr_unary -> Expr, do_parse!(
        operator: alt!(
            punct!("*") => { |_| UnOp::Deref }
            |
            punct!("!") => { |_| UnOp::Not }
            |
            punct!("-") => { |_| UnOp::Neg }
        ) >>
        operand: expr >>
        (Expr::Unary(operator, Box::new(operand)))
    ));

    named!(expr_lit -> Expr, map!(lit, Expr::Lit));

    named!(and_cast -> Ty, do_parse!(
        punct!("as") >>
        multispace >>
        ty: ty >>
        (ty)
    ));

    named!(and_ascription -> Ty, preceded!(punct!(":"), ty));

    named!(expr_if -> Expr, do_parse!(
        punct!("if") >>
        multispace >>
        cond: expr >>
        punct!("{") >>
        then_block: within_block >>
        punct!("}") >>
        else_block: option!(preceded!(
            punct!("else"),
            alt!(
                expr_if
                |
                do_parse!(
                    punct!("{") >>
                    else_block: within_block >>
                    punct!("}") >>
                    (Expr::Block(Box::new(Block {
                        stmts: else_block,
                        rules: BlockCheckMode::Default,
                    })))
                )
            )
        )) >>
        (Expr::If(
            Box::new(cond),
            Box::new(Block {
                stmts: then_block,
                rules: BlockCheckMode::Default,
            }),
            else_block.map(Box::new),
        ))
    ));

    named!(expr_loop -> Expr, do_parse!(
        lt: option!(terminated!(lifetime, punct!(":"))) >>
        punct!("loop") >>
        loop_block: block >>
        (Expr::Loop(
            Box::new(loop_block),
            lt.map(|lt| lt.ident),
        ))
    ));

    named!(expr_while -> Expr, do_parse!(
        lt: option!(terminated!(lifetime, punct!(":"))) >>
        punct!("while") >>
        cond: expr >>
        while_block: block >>
        (Expr::While(
            Box::new(cond),
            Box::new(while_block),
            lt.map(|lt| lt.ident),
        ))
    ));

    named!(expr_block -> Expr, map!(block, |b| Expr::Block(Box::new(b))));

    named!(block -> Block, do_parse!(
        rules: block_check_mode >>
        punct!("{") >>
        stmts: within_block >>
        punct!("}") >>
        (Block {
            stmts: stmts,
            rules: rules,
        })
    ));

    named!(block_check_mode -> BlockCheckMode, alt!(
        punct!("unsafe") => { |_| BlockCheckMode::Unsafe }
        |
        epsilon!() => { |_| BlockCheckMode::Default }
    ));

    named!(within_block -> Vec<Stmt>, do_parse!(
        mut most: many0!(standalone_stmt) >>
        last: option!(expr) >>
        (match last {
            None => most,
            Some(last) => {
                most.push(Stmt::Expr(Box::new(last)));
                most
            }
        })
    ));

    named!(standalone_stmt -> Stmt, alt!(
        // TODO: local
        // TODO: item
        // TODO: expr
        stmt_semi
        // TODO: mac
    ));

    named!(stmt_semi -> Stmt, do_parse!(
        e: expr >>
        punct!(";") >>
        (Stmt::Semi(Box::new(e)))
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Expr {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Expr::Lit(ref lit) => lit.to_tokens(tokens),
                _ => unimplemented!(),
            }
        }
    }
}
