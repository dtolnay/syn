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
    If(Box<Expr>, Block, Option<Box<Expr>>),
    /// An `if let` expression with an optional else block
    ///
    /// `if let pat = expr { block } else { expr }`
    ///
    /// This is desugared to a `match` expression.
    IfLet(Box<Pat>, Box<Expr>, Block, Option<Box<Expr>>),
    /// A while loop, with an optional label
    ///
    /// `'label: while expr { block }`
    While(Box<Expr>, Block, Option<Ident>),
    /// A while-let loop, with an optional label
    ///
    /// `'label: while let pat = expr { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    WhileLet(Box<Pat>, Box<Expr>, Block, Option<Ident>),
    /// A for loop, with an optional label
    ///
    /// `'label: for pat in expr { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    ForLoop(Box<Pat>, Box<Expr>, Block, Option<Ident>),
    /// Conditionless loop (can be exited with break, continue, or return)
    ///
    /// `'label: loop { block }`
    Loop(Block, Option<Ident>),
    /// A `match` block.
    Match(Box<Expr>, Vec<Arm>),
    /// A closure (for example, `move |a, b, c| {a + b + c}`)
    Closure(CaptureBy, Box<FnDecl>, Block),
    /// A block (`{ ... }` or `unsafe { ... }`)
    Block(BlockCheckMode, Block),

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
    Struct(Path, Vec<FieldValue>, Option<Box<Expr>>),

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FieldValue {
    pub ident: Ident,
    pub expr: Expr,
}

/// A Block (`{ .. }`).
///
/// E.g. `{ .. }` as in `fn foo() { .. }`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block {
    /// Statements in a block
    pub stmts: Vec<Stmt>,
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
    use {FnArg, FnDecl, FunctionRetTy, Ident, Lifetime, Ty};
    use attr::parsing::outer_attr;
    use generics::parsing::lifetime;
    use ident::parsing::ident;
    use item::parsing::item;
    use lit::parsing::lit;
    use ty::parsing::{mutability, path, qpath, ty};

    named!(pub expr -> Expr, do_parse!(
        mut e: alt!(
            expr_lit // needs to be before expr_struct
            |
            expr_struct // needs to be before expr_path
            |
            expr_paren // needs to be before expr_tup
            |
            expr_box
            |
            expr_vec
            |
            expr_tup
            |
            expr_unary
            |
            expr_if
            |
            expr_while
            // TODO: WhileLet
            // TODO: ForLoop
            // TODO: Loop
            // TODO: ForLoop
            |
            expr_loop
            |
            expr_match
            |
            expr_closure
            |
            expr_block
            |
            expr_path
            |
            expr_addr_of
            |
            expr_break
            |
            expr_continue
            |
            expr_ret
            // TODO: Mac
            |
            expr_repeat
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
            |
            tap!(_try: punct!("?") => {
                e = Expr::Try(Box::new(e));
            })
        )) >>
        (e)
    ));

    named!(expr_paren -> Expr, do_parse!(
        punct!("(") >>
        e: expr >>
        punct!(")") >>
        (Expr::Paren(Box::new(e)))
    ));

    named!(expr_box -> Expr, do_parse!(
        keyword!("box") >>
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
        option!(punct!(",")) >>
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
        keyword!("as") >>
        ty: ty >>
        (ty)
    ));

    named!(and_ascription -> Ty, preceded!(punct!(":"), ty));

    enum IfCond {
        Let(Pat, Expr),
        Expr(Expr),
    }

    named!(expr_if -> Expr, do_parse!(
        keyword!("if") >>
        cond: alt!(
            do_parse!(
                keyword!("let") >>
                pat: pat >>
                punct!("=") >>
                value: expr >>
                (IfCond::Let(pat, value))
            )
            |
            map!(expr, IfCond::Expr)
        ) >>
        punct!("{") >>
        then_block: within_block >>
        punct!("}") >>
        else_block: option!(preceded!(
            keyword!("else"),
            alt!(
                expr_if
                |
                do_parse!(
                    punct!("{") >>
                    else_block: within_block >>
                    punct!("}") >>
                    (Expr::Block(BlockCheckMode::Default, Block {
                        stmts: else_block,
                    }))
                )
            )
        )) >>
        (match cond {
            IfCond::Let(pat, expr) => Expr::IfLet(
                Box::new(pat),
                Box::new(expr),
                Block {
                    stmts: then_block,
                },
                else_block.map(Box::new),
            ),
            IfCond::Expr(cond) => Expr::If(
                Box::new(cond),
                Block {
                    stmts: then_block,
                },
                else_block.map(Box::new),
            ),
        })
    ));

    named!(expr_loop -> Expr, do_parse!(
        lbl: option!(terminated!(label, punct!(":"))) >>
        keyword!("loop") >>
        loop_block: block >>
        (Expr::Loop(loop_block, lbl))
    ));

    named!(expr_match -> Expr, do_parse!(
        keyword!("match") >>
        obj: expr >>
        punct!("{") >>
        arms: many0!(do_parse!(
            attrs: many0!(outer_attr) >>
            pats: separated_nonempty_list!(punct!("|"), pat) >>
            guard: option!(preceded!(keyword!("if"), expr)) >>
            punct!("=>") >>
            body: alt!(
                terminated!(expr, punct!(","))
                |
                map!(block, |blk| Expr::Block(BlockCheckMode::Default, blk))
            ) >>
            (Arm {
                attrs: attrs,
                pats: pats,
                guard: guard.map(Box::new),
                body: Box::new(body),
            })
        )) >>
        punct!("}") >>
        (Expr::Match(Box::new(obj), arms))
    ));

    named!(expr_closure -> Expr, do_parse!(
        capture: capture_by >>
        punct!("|") >>
        inputs: separated_list!(punct!(","), closure_arg) >>
        punct!("|") >>
        ret_and_body: alt!(
            do_parse!(
                punct!("->") >>
                ty: ty >>
                body: block >>
                ((FunctionRetTy::Ty(ty), body))
            )
            |
            map!(expr, |e| (
                FunctionRetTy::Default,
                Block {
                    stmts: vec![Stmt::Expr(Box::new(e))],
                },
            ))
        ) >>
        (Expr::Closure(
            capture,
            Box::new(FnDecl {
                inputs: inputs,
                output: ret_and_body.0,
            }),
            ret_and_body.1,
        ))
    ));

    named!(closure_arg -> FnArg, do_parse!(
        pat: pat >>
        ty: option!(preceded!(punct!(":"), ty)) >>
        (FnArg {
            pat: pat,
            ty: ty.unwrap_or(Ty::Infer),
        })
    ));

    named!(expr_while -> Expr, do_parse!(
        lbl: option!(terminated!(label, punct!(":"))) >>
        keyword!("while") >>
        cond: expr >>
        while_block: block >>
        (Expr::While(
            Box::new(cond),
            while_block,
            lbl,
        ))
    ));

    named!(expr_continue -> Expr, do_parse!(
        keyword!("continue") >>
        lbl: option!(label) >>
        (Expr::Continue(lbl))
    ));

    named!(expr_break -> Expr, do_parse!(
        keyword!("break") >>
        lbl: option!(label) >>
        (Expr::Break(lbl))
    ));

    named!(expr_ret -> Expr, do_parse!(
        keyword!("return") >>
        ret_value: option!(expr) >>
        (Expr::Ret(ret_value.map(Box::new)))
    ));

    named!(expr_struct -> Expr, do_parse!(
        path: path >>
        punct!("{") >>
        fields: separated_list!(punct!(","), field_value) >>
        base: option!(do_parse!(
            cond!(!fields.is_empty(), punct!(",")) >>
            punct!("..") >>
            base: expr >>
            (base)
        )) >>
        punct!("}") >>
        (Expr::Struct(path, fields, base.map(Box::new)))
    ));

    named!(field_value -> FieldValue, do_parse!(
        name: ident >>
        punct!(":") >>
        value: expr >>
        (FieldValue {
            ident: name,
            expr: value,
        })
    ));

    named!(expr_repeat -> Expr, do_parse!(
        punct!("[") >>
        value: expr >>
        punct!(";") >>
        times: expr >>
        punct!("]") >>
        (Expr::Repeat(Box::new(value), Box::new(times)))
    ));

    named!(expr_block -> Expr, do_parse!(
        rules: block_check_mode >>
        b: block >>
        (Expr::Block(rules, Block {
            stmts: b.stmts,
        }))
    ));

    named!(expr_path -> Expr, map!(qpath, |(qself, path)| Expr::Path(qself, path)));

    named!(expr_addr_of -> Expr, do_parse!(
        punct!("&") >>
        mutability: mutability >>
        expr: expr >>
        (Expr::AddrOf(mutability, Box::new(expr)))
    ));

    named!(pub block -> Block, do_parse!(
        punct!("{") >>
        stmts: within_block >>
        punct!("}") >>
        (Block {
            stmts: stmts,
        })
    ));

    named!(block_check_mode -> BlockCheckMode, alt!(
        keyword!("unsafe") => { |_| BlockCheckMode::Unsafe }
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
        stmt_local
        |
        stmt_item
        |
        stmt_semi
        // TODO: mac
    ));

    named!(stmt_local -> Stmt, do_parse!(
        attrs: many0!(outer_attr) >>
        keyword!("let") >>
        pat: pat >>
        ty: option!(preceded!(punct!(":"), ty)) >>
        init: option!(preceded!(punct!("="), expr)) >>
        punct!(";") >>
        (Stmt::Local(Box::new(Local {
            pat: Box::new(pat),
            ty: ty.map(Box::new),
            init: init.map(Box::new),
            attrs: attrs,
        })))
    ));

    named!(stmt_item -> Stmt, map!(item, |i| Stmt::Item(Box::new(i))));

    named!(stmt_semi -> Stmt, do_parse!(
        e: expr >>
        punct!(";") >>
        (Stmt::Semi(Box::new(e)))
    ));

    named!(pub pat -> Pat, alt!(
        pat_wild
        |
        pat_ident
        // TODO: Struct
        // TODO: TupleStruct
        |
        pat_path
        // TODO: Tuple
        // TODO: Box
        // TODO: Ref
        // TODO: Lit
        // TODO: Range
        // TODO: Vec
        // TODO: Mac
    ));

    named!(pat_wild -> Pat, map!(keyword!("_"), |_| Pat::Wild));

    named!(pat_ident -> Pat, do_parse!(
        mode: option!(keyword!("ref")) >>
        mutability: mutability >>
        name: ident >>
        subpat: option!(preceded!(punct!("@"), pat)) >>
        (Pat::Ident(
            if mode.is_some() {
                BindingMode::ByRef(mutability)
            } else {
                BindingMode::ByValue(mutability)
            },
            name,
            subpat.map(Box::new),
        ))
    ));

    named!(pat_path -> Pat, map!(qpath, |(qself, path)| Pat::Path(qself, path)));

    named!(capture_by -> CaptureBy, alt!(
        keyword!("move") => { |_| CaptureBy::Value }
        |
        epsilon!() => { |_| CaptureBy::Ref }
    ));

    named!(label -> Ident, map!(lifetime, |lt: Lifetime| lt.ident));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use {FunctionRetTy, Mutability, Ty};
    use quote::{Tokens, ToTokens};

    impl ToTokens for Expr {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Expr::Box(ref _inner) => unimplemented!(),
                Expr::Vec(ref _inner) => unimplemented!(),
                Expr::Call(ref func, ref args) => {
                    func.to_tokens(tokens);
                    tokens.append("(");
                    tokens.append_separated(args, ",");
                    tokens.append(")");
                }
                Expr::MethodCall(ref ident, ref ascript, ref args) => {
                    args[0].to_tokens(tokens);
                    tokens.append(".");
                    ident.to_tokens(tokens);
                    if ascript.len() > 0 {
                        tokens.append("::");
                        tokens.append("<");
                        tokens.append_separated(ascript, ",");
                        tokens.append(">");
                    }
                    tokens.append("(");
                    tokens.append_separated(&args[1..], ",");
                    tokens.append(")");
                }
                Expr::Tup(ref fields) => {
                    tokens.append("(");
                    tokens.append_separated(fields, ",");
                    if fields.len() == 1 {
                        tokens.append(",");
                    }
                    tokens.append(")");
                }
                Expr::Binary(op, ref left, ref right) => {
                    left.to_tokens(tokens);
                    op.to_tokens(tokens);
                    right.to_tokens(tokens);
                }
                Expr::Unary(op, ref expr) => {
                    op.to_tokens(tokens);
                    expr.to_tokens(tokens);
                }
                Expr::Lit(ref lit) => lit.to_tokens(tokens),
                Expr::Cast(ref expr, ref ty) => {
                    expr.to_tokens(tokens);
                    tokens.append("as");
                    ty.to_tokens(tokens);
                }
                Expr::Type(ref expr, ref ty) => {
                    expr.to_tokens(tokens);
                    tokens.append(":");
                    ty.to_tokens(tokens);
                }
                Expr::If(ref cond, ref then_block, ref else_block) => {
                    tokens.append("if");
                    cond.to_tokens(tokens);
                    then_block.to_tokens(tokens);
                    if let Some(ref else_block) = *else_block {
                        tokens.append("else");
                        else_block.to_tokens(tokens);
                    }
                }
                Expr::IfLet(ref pat, ref expr, ref then_block, ref else_block) => {
                    tokens.append("if");
                    tokens.append("let");
                    pat.to_tokens(tokens);
                    tokens.append("=");
                    expr.to_tokens(tokens);
                    then_block.to_tokens(tokens);
                    if let Some(ref else_block) = *else_block {
                        tokens.append("else");
                        else_block.to_tokens(tokens);
                    }
                }
                Expr::While(ref _cond, ref _body, ref _label) => unimplemented!(),
                Expr::WhileLet(ref _pat, ref _expr, ref _body, ref _label) => unimplemented!(),
                Expr::ForLoop(ref _pat, ref _expr, ref _body, ref _label) => unimplemented!(),
                Expr::Loop(ref _body, ref _label) => unimplemented!(),
                Expr::Match(ref expr, ref arms) => {
                    tokens.append("match");
                    expr.to_tokens(tokens);
                    tokens.append("{");
                    tokens.append_separated(arms, ",");
                    tokens.append("}");
                }
                Expr::Closure(capture, ref decl, ref body) => {
                    capture.to_tokens(tokens);
                    tokens.append("|");
                    for (i, input) in decl.inputs.iter().enumerate() {
                        if i > 0 {
                            tokens.append(",");
                        }
                        input.pat.to_tokens(tokens);
                        match input.ty {
                            Ty::Infer => { /* nothing */ }
                            _ => {
                                tokens.append(":");
                                input.ty.to_tokens(tokens);
                            }
                        }
                    }
                    tokens.append("|");
                    match decl.output {
                        FunctionRetTy::Default => {
                            if body.stmts.len() == 1 {
                                if let Stmt::Expr(ref expr) = body.stmts[0] {
                                    expr.to_tokens(tokens);
                                } else {
                                    body.to_tokens(tokens);
                                }
                            } else {
                                body.to_tokens(tokens);
                            }
                        }
                        FunctionRetTy::Ty(ref ty) => {
                            tokens.append("->");
                            ty.to_tokens(tokens);
                            body.to_tokens(tokens);
                        }
                    }
                }
                Expr::Block(rules, ref block) => {
                    rules.to_tokens(tokens);
                    block.to_tokens(tokens);
                }
                Expr::Assign(ref _var, ref _expr) => unimplemented!(),
                Expr::AssignOp(_op, ref _var, ref _expr) => unimplemented!(),
                Expr::Field(ref _expr, ref _field) => unimplemented!(),
                Expr::TupField(ref _expr, _field) => unimplemented!(),
                Expr::Index(ref _expr, ref _index) => unimplemented!(),
                Expr::Range(ref _from, ref _to, _limits) => unimplemented!(),
                Expr::Path(None, ref path) => {
                    path.to_tokens(tokens);
                }
                Expr::Path(Some(ref qself), ref path) => {
                    tokens.append("<");
                    qself.ty.to_tokens(tokens);
                    if qself.position > 0 {
                        tokens.append("as");
                        for (i, segment) in path.segments.iter()
                                                .take(qself.position)
                                                .enumerate()
                        {
                            if i > 0 || path.global {
                                tokens.append("::");
                            }
                            segment.to_tokens(tokens);
                        }
                    }
                    tokens.append(">");
                    for segment in path.segments.iter().skip(qself.position) {
                        tokens.append("::");
                        segment.to_tokens(tokens);
                    }
                }
                Expr::AddrOf(mutability, ref expr) => {
                    tokens.append("&");
                    mutability.to_tokens(tokens);
                    expr.to_tokens(tokens);
                }
                Expr::Break(ref opt_label) => {
                    tokens.append("break");
                    opt_label.to_tokens(tokens);
                }
                Expr::Continue(ref opt_label) => {
                    tokens.append("continue");
                    opt_label.to_tokens(tokens);
                }
                Expr::Ret(ref opt_expr) => {
                    tokens.append("return");
                    opt_expr.to_tokens(tokens);
                }
                Expr::Mac(ref _mac) => unimplemented!(),
                Expr::Struct(ref path, ref fields, ref base) => {
                    path.to_tokens(tokens);
                    tokens.append("{");
                    tokens.append_separated(fields, ",");
                    if let Some(ref base) = *base {
                        if !fields.is_empty() {
                            tokens.append(",");
                        }
                        tokens.append("..");
                        base.to_tokens(tokens);
                    }
                    tokens.append("}");
                }
                Expr::Repeat(ref expr, ref times) => {
                    tokens.append("[");
                    expr.to_tokens(tokens);
                    tokens.append(";");
                    times.to_tokens(tokens);
                    tokens.append("]");
                }
                Expr::Paren(ref expr) => {
                    tokens.append("(");
                    expr.to_tokens(tokens);
                    tokens.append(")");
                }
                Expr::Try(ref expr) => {
                    expr.to_tokens(tokens);
                    tokens.append("?");
                }
            }
        }
    }

    impl ToTokens for BinOp {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                BinOp::Add => tokens.append("+"),
                BinOp::Sub => tokens.append("-"),
                BinOp::Mul => tokens.append("*"),
                BinOp::Div => tokens.append("/"),
                BinOp::Rem => tokens.append("%"),
                BinOp::And => tokens.append("&&"),
                BinOp::Or => tokens.append("||"),
                BinOp::BitXor => tokens.append("^"),
                BinOp::BitAnd => tokens.append("&"),
                BinOp::BitOr => tokens.append("|"),
                BinOp::Shl => tokens.append("<<"),
                BinOp::Shr => tokens.append(">>"),
                BinOp::Eq => tokens.append("=="),
                BinOp::Lt => tokens.append("<"),
                BinOp::Le => tokens.append("<="),
                BinOp::Ne => tokens.append("!="),
                BinOp::Ge => tokens.append(">="),
                BinOp::Gt => tokens.append(">"),
            }
        }
    }

    impl ToTokens for UnOp {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                UnOp::Deref => tokens.append("*"),
                UnOp::Not => tokens.append("!"),
                UnOp::Neg => tokens.append("-"),
            }
        }
    }

    impl ToTokens for FieldValue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            tokens.append(":");
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for Arm {
        fn to_tokens(&self, tokens: &mut Tokens) {
            for attr in &self.attrs {
                attr.to_tokens(tokens);
            }
            tokens.append_separated(&self.pats, "|");
            if let Some(ref guard) = self.guard {
                tokens.append("if");
                guard.to_tokens(tokens);
            }
            tokens.append("=>");
            self.body.to_tokens(tokens);
            match *self.body {
                Expr::Block(_, _) => { /* no comma */ }
                _ => tokens.append(","),
            }
        }
    }

    impl ToTokens for Pat {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Pat::Wild => tokens.append("_"),
                Pat::Ident(mode, ref ident, ref subpat) => {
                    mode.to_tokens(tokens);
                    ident.to_tokens(tokens);
                    if let Some(ref subpat) = *subpat {
                        tokens.append("@");
                        subpat.to_tokens(tokens);
                    }
                }
                Pat::Struct(ref _path, ref _fields, _dots) => unimplemented!(),
                Pat::TupleStruct(ref _path, ref _pats, _dotpos) => unimplemented!(),
                Pat::Path(ref _qself, ref _path) => unimplemented!(),
                Pat::Tuple(ref _pats, _dotpos) => unimplemented!(),
                Pat::Box(ref _inner) => unimplemented!(),
                Pat::Ref(ref _target, _mutability) => unimplemented!(),
                Pat::Lit(ref _expr) => unimplemented!(),
                Pat::Range(ref _lower, ref _upper) => unimplemented!(),
                Pat::Vec(ref _before, ref _dots, ref _after) => unimplemented!(),
                Pat::Mac(ref _mac) => unimplemented!(),
            }
        }
    }

    impl ToTokens for BindingMode {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                BindingMode::ByRef(Mutability::Immutable) => {
                    tokens.append("ref");
                }
                BindingMode::ByRef(Mutability::Mutable) => {
                    tokens.append("ref");
                    tokens.append("mut");
                }
                BindingMode::ByValue(Mutability::Immutable) => {}
                BindingMode::ByValue(Mutability::Mutable) => {
                    tokens.append("mut");
                }
            }
        }
    }

    impl ToTokens for CaptureBy {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                CaptureBy::Value => tokens.append("move"),
                CaptureBy::Ref => { /* nothing */ }
            }
        }
    }

    impl ToTokens for Block {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("{");
            for stmt in &self.stmts {
                stmt.to_tokens(tokens);
            }
            tokens.append("}");
        }
    }

    impl ToTokens for BlockCheckMode {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                BlockCheckMode::Default => { /* nothing */ }
                BlockCheckMode::Unsafe => tokens.append("unsafe"),
            }
        }
    }

    impl ToTokens for Stmt {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Stmt::Local(ref local) => local.to_tokens(tokens),
                Stmt::Item(ref item) => item.to_tokens(tokens),
                Stmt::Expr(ref expr) => expr.to_tokens(tokens),
                Stmt::Semi(ref expr) => {
                    expr.to_tokens(tokens);
                    tokens.append(";");
                }
                Stmt::Mac(ref _mac) => unimplemented!(),
            }
        }
    }

    impl ToTokens for Local {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("let");
            self.pat.to_tokens(tokens);
            if let Some(ref ty) = self.ty {
                tokens.append(":");
                ty.to_tokens(tokens);
            }
            if let Some(ref init) = self.init {
                tokens.append("=");
                init.to_tokens(tokens);
            }
            tokens.append(";");
        }
    }
}
