// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::*;
use proc_macro2::{Span, TokenStream};
use punctuated::Punctuated;
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};
#[cfg(feature = "full")]
use std::mem;
#[cfg(feature = "extra-traits")]
use tt::TokenStreamHelper;

ast_enum_of_structs! {
    /// A Rust expression.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    ///
    /// # Syntax tree enums
    ///
    /// This type is a syntax tree enum. In Syn this and other syntax tree enums
    /// are designed to be traversed using the following rebinding idiom.
    ///
    /// ```
    /// # use syn::Expr;
    /// #
    /// # fn example(expr: Expr) {
    /// # const IGNORE: &str = stringify! {
    /// let expr: Expr = /* ... */;
    /// # };
    /// match expr {
    ///     Expr::MethodCall(expr) => {
    ///         /* ... */
    ///     }
    ///     Expr::Cast(expr) => {
    ///         /* ... */
    ///     }
    ///     Expr::IfLet(expr) => {
    ///         /* ... */
    ///     }
    ///     /* ... */
    ///     # _ => {}
    /// }
    /// # }
    /// ```
    ///
    /// We begin with a variable `expr` of type `Expr` that has no fields
    /// (because it is an enum), and by matching on it and rebinding a variable
    /// with the same name `expr` we effectively imbue our variable with all of
    /// the data fields provided by the variant that it turned out to be. So for
    /// example above if we ended up in the `MethodCall` case then we get to use
    /// `expr.receiver`, `expr.args` etc; if we ended up in the `IfLet` case we
    /// get to use `expr.pat`, `expr.then_branch`, `expr.else_branch`.
    ///
    /// The pattern is similar if the input expression is borrowed:
    ///
    /// ```
    /// # use syn::Expr;
    /// #
    /// # fn example(expr: &Expr) {
    /// match *expr {
    ///     Expr::MethodCall(ref expr) => {
    /// #   }
    /// #   _ => {}
    /// # }
    /// # }
    /// ```
    ///
    /// This approach avoids repeating the variant names twice on every line.
    ///
    /// ```
    /// # use syn::{Expr, ExprMethodCall};
    /// #
    /// # fn example(expr: Expr) {
    /// # match expr {
    /// Expr::MethodCall(ExprMethodCall { method, args, .. }) => { // repetitive
    /// # }
    /// # _ => {}
    /// # }
    /// # }
    /// ```
    ///
    /// In general, the name to which a syntax tree enum variant is bound should
    /// be a suitable name for the complete syntax tree enum type.
    ///
    /// ```
    /// # use syn::{Expr, ExprField};
    /// #
    /// # fn example(discriminant: &ExprField) {
    /// // Binding is called `base` which is the name I would use if I were
    /// // assigning `*discriminant.base` without an `if let`.
    /// if let Expr::Tuple(ref base) = *discriminant.base {
    /// # }
    /// # }
    /// ```
    ///
    /// A sign that you may not be choosing the right variable names is if you
    /// see names getting repeated in your code, like accessing
    /// `receiver.receiver` or `pat.pat` or `cond.cond`.
    pub enum Expr {
        /// A box expression: `box f`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Box(ExprBox #full {
            pub attrs: Vec<Attribute>,
            pub box_token: Token![box],
            pub expr: Box<Expr>,
        }),

        /// A placement expression: `place <- value`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub InPlace(ExprInPlace #full {
            pub attrs: Vec<Attribute>,
            pub place: Box<Expr>,
            pub arrow_token: Token![<-],
            pub value: Box<Expr>,
        }),

        /// A slice literal expression: `[a, b, c, d]`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Array(ExprArray #full {
            pub attrs: Vec<Attribute>,
            pub bracket_token: token::Bracket,
            pub elems: Punctuated<Expr, Token![,]>,
        }),

        /// A function call expression: `invoke(a, b)`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Call(ExprCall {
            pub attrs: Vec<Attribute>,
            pub func: Box<Expr>,
            pub paren_token: token::Paren,
            pub args: Punctuated<Expr, Token![,]>,
        }),

        /// A method call expression: `x.foo::<T>(a, b)`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub MethodCall(ExprMethodCall #full {
            pub attrs: Vec<Attribute>,
            pub receiver: Box<Expr>,
            pub dot_token: Token![.],
            pub method: Ident,
            pub turbofish: Option<MethodTurbofish>,
            pub paren_token: token::Paren,
            pub args: Punctuated<Expr, Token![,]>,
        }),

        /// A tuple expression: `(a, b, c, d)`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Tuple(ExprTuple #full {
            pub attrs: Vec<Attribute>,
            pub paren_token: token::Paren,
            pub elems: Punctuated<Expr, Token![,]>,
        }),

        /// A binary operation: `a + b`, `a * b`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Binary(ExprBinary {
            pub attrs: Vec<Attribute>,
            pub left: Box<Expr>,
            pub op: BinOp,
            pub right: Box<Expr>,
        }),

        /// A unary operation: `!x`, `*x`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Unary(ExprUnary {
            pub attrs: Vec<Attribute>,
            pub op: UnOp,
            pub expr: Box<Expr>,
        }),

        /// A literal in place of an expression: `1`, `"foo"`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Lit(ExprLit {
            pub attrs: Vec<Attribute>,
            pub lit: Lit,
        }),

        /// A cast expression: `foo as f64`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Cast(ExprCast {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub as_token: Token![as],
            pub ty: Box<Type>,
        }),

        /// A type ascription expression: `foo: f64`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Type(ExprType #full {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub colon_token: Token![:],
            pub ty: Box<Type>,
        }),

        /// An `if` expression with an optional `else` block: `if expr { ... }
        /// else { ... }`.
        ///
        /// The `else` branch expression may only be an `If`, `IfLet`, or
        /// `Block` expression, not any of the other types of expression.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub If(ExprIf #full {
            pub attrs: Vec<Attribute>,
            pub if_token: Token![if],
            pub cond: Box<Expr>,
            pub then_branch: Block,
            pub else_branch: Option<(Token![else], Box<Expr>)>,
        }),

        /// An `if let` expression with an optional `else` block: `if let pat =
        /// expr { ... } else { ... }`.
        ///
        /// The `else` branch expression may only be an `If`, `IfLet`, or
        /// `Block` expression, not any of the other types of expression.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub IfLet(ExprIfLet #full {
            pub attrs: Vec<Attribute>,
            pub if_token: Token![if],
            pub let_token: Token![let],
            pub pats: Punctuated<Pat, Token![|]>,
            pub eq_token: Token![=],
            pub expr: Box<Expr>,
            pub then_branch: Block,
            pub else_branch: Option<(Token![else], Box<Expr>)>,
        }),

        /// A while loop: `while expr { ... }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub While(ExprWhile #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Label>,
            pub while_token: Token![while],
            pub cond: Box<Expr>,
            pub body: Block,
        }),

        /// A while-let loop: `while let pat = expr { ... }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub WhileLet(ExprWhileLet #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Label>,
            pub while_token: Token![while],
            pub let_token: Token![let],
            pub pats: Punctuated<Pat, Token![|]>,
            pub eq_token: Token![=],
            pub expr: Box<Expr>,
            pub body: Block,
        }),

        /// A for loop: `for pat in expr { ... }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub ForLoop(ExprForLoop #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Label>,
            pub for_token: Token![for],
            pub pat: Box<Pat>,
            pub in_token: Token![in],
            pub expr: Box<Expr>,
            pub body: Block,
        }),

        /// Conditionless loop: `loop { ... }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Loop(ExprLoop #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Label>,
            pub loop_token: Token![loop],
            pub body: Block,
        }),

        /// A `match` expression: `match n { Some(n) => {}, None => {} }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Match(ExprMatch #full {
            pub attrs: Vec<Attribute>,
            pub match_token: Token![match],
            pub expr: Box<Expr>,
            pub brace_token: token::Brace,
            pub arms: Vec<Arm>,
        }),

        /// A closure expression: `|a, b| a + b`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Closure(ExprClosure #full {
            pub attrs: Vec<Attribute>,
            pub asyncness: Option<Token![async]>,
            pub movability: Option<Token![static]>,
            pub capture: Option<Token![move]>,
            pub or1_token: Token![|],
            pub inputs: Punctuated<FnArg, Token![,]>,
            pub or2_token: Token![|],
            pub output: ReturnType,
            pub body: Box<Expr>,
        }),

        /// An unsafe block: `unsafe { ... }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Unsafe(ExprUnsafe #full {
            pub attrs: Vec<Attribute>,
            pub unsafe_token: Token![unsafe],
            pub block: Block,
        }),

        /// A blocked scope: `{ ... }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Block(ExprBlock #full {
            pub attrs: Vec<Attribute>,
            pub label: Option<Label>,
            pub block: Block,
        }),

        /// An assignment expression: `a = compute()`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Assign(ExprAssign #full {
            pub attrs: Vec<Attribute>,
            pub left: Box<Expr>,
            pub eq_token: Token![=],
            pub right: Box<Expr>,
        }),

        /// A compound assignment expression: `counter += 1`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub AssignOp(ExprAssignOp #full {
            pub attrs: Vec<Attribute>,
            pub left: Box<Expr>,
            pub op: BinOp,
            pub right: Box<Expr>,
        }),

        /// Access of a named struct field (`obj.k`) or unnamed tuple struct
        /// field (`obj.0`).
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Field(ExprField {
            pub attrs: Vec<Attribute>,
            pub base: Box<Expr>,
            pub dot_token: Token![.],
            pub member: Member,
        }),

        /// A square bracketed indexing expression: `vector[2]`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Index(ExprIndex {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub bracket_token: token::Bracket,
            pub index: Box<Expr>,
        }),

        /// A range expression: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Range(ExprRange #full {
            pub attrs: Vec<Attribute>,
            pub from: Option<Box<Expr>>,
            pub limits: RangeLimits,
            pub to: Option<Box<Expr>>,
        }),

        /// A path like `std::mem::replace` possibly containing generic
        /// parameters and a qualified self-type.
        ///
        /// A plain identifier like `x` is a path of length 1.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Path(ExprPath {
            pub attrs: Vec<Attribute>,
            pub qself: Option<QSelf>,
            pub path: Path,
        }),

        /// A referencing operation: `&a` or `&mut a`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Reference(ExprReference #full {
            pub attrs: Vec<Attribute>,
            pub and_token: Token![&],
            pub mutability: Option<Token![mut]>,
            pub expr: Box<Expr>,
        }),

        /// A `break`, with an optional label to break and an optional
        /// expression.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Break(ExprBreak #full {
            pub attrs: Vec<Attribute>,
            pub break_token: Token![break],
            pub label: Option<Lifetime>,
            pub expr: Option<Box<Expr>>,
        }),

        /// A `continue`, with an optional label.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Continue(ExprContinue #full {
            pub attrs: Vec<Attribute>,
            pub continue_token: Token![continue],
            pub label: Option<Lifetime>,
        }),

        /// A `return`, with an optional value to be returned.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Return(ExprReturn #full {
            pub attrs: Vec<Attribute>,
            pub return_token: Token![return],
            pub expr: Option<Box<Expr>>,
        }),

        /// A macro invocation expression: `format!("{}", q)`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Macro(ExprMacro #full {
            pub attrs: Vec<Attribute>,
            pub mac: Macro,
        }),

        /// A struct literal expression: `Point { x: 1, y: 1 }`.
        ///
        /// The `rest` provides the value of the remaining fields as in `S { a:
        /// 1, b: 1, ..rest }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Struct(ExprStruct #full {
            pub attrs: Vec<Attribute>,
            pub path: Path,
            pub brace_token: token::Brace,
            pub fields: Punctuated<FieldValue, Token![,]>,
            pub dot2_token: Option<Token![..]>,
            pub rest: Option<Box<Expr>>,
        }),

        /// An array literal constructed from one repeated element: `[0u8; N]`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Repeat(ExprRepeat #full {
            pub attrs: Vec<Attribute>,
            pub bracket_token: token::Bracket,
            pub expr: Box<Expr>,
            pub semi_token: Token![;],
            pub len: Box<Expr>,
        }),

        /// A parenthesized expression: `(a + b)`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Paren(ExprParen {
            pub attrs: Vec<Attribute>,
            pub paren_token: token::Paren,
            pub expr: Box<Expr>,
        }),

        /// An expression contained within invisible delimiters.
        ///
        /// This variant is important for faithfully representing the precedence
        /// of expressions and is related to `None`-delimited spans in a
        /// `TokenStream`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Group(ExprGroup #full {
            pub attrs: Vec<Attribute>,
            pub group_token: token::Group,
            pub expr: Box<Expr>,
        }),

        /// A try-expression: `expr?`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Try(ExprTry #full {
            pub attrs: Vec<Attribute>,
            pub expr: Box<Expr>,
            pub question_token: Token![?],
        }),

        /// An async block: `async { ... }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Async(ExprAsync #full {
            pub attrs: Vec<Attribute>,
            pub async_token: Token![async],
            pub capture: Option<Token![move]>,
            pub block: Block,
        }),

        /// A try block: `try { ... }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub TryBlock(ExprTryBlock #full {
            pub attrs: Vec<Attribute>,
            pub try_token: Token![try],
            pub block: Block,
        }),

        /// A yield expression: `yield expr`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Yield(ExprYield #full {
            pub attrs: Vec<Attribute>,
            pub yield_token: Token![yield],
            pub expr: Option<Box<Expr>>,
        }),

        /// Tokens in expression position not interpreted by Syn.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Verbatim(ExprVerbatim #manual_extra_traits {
            pub tts: TokenStream,
        }),
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for ExprVerbatim {}

#[cfg(feature = "extra-traits")]
impl PartialEq for ExprVerbatim {
    fn eq(&self, other: &Self) -> bool {
        TokenStreamHelper(&self.tts) == TokenStreamHelper(&other.tts)
    }
}

#[cfg(feature = "extra-traits")]
impl Hash for ExprVerbatim {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        TokenStreamHelper(&self.tts).hash(state);
    }
}

impl Expr {
    // Not public API.
    #[doc(hidden)]
    #[cfg(feature = "full")]
    pub fn replace_attrs(&mut self, new: Vec<Attribute>) -> Vec<Attribute> {
        match *self {
            Expr::Box(ExprBox { ref mut attrs, .. })
            | Expr::InPlace(ExprInPlace { ref mut attrs, .. })
            | Expr::Array(ExprArray { ref mut attrs, .. })
            | Expr::Call(ExprCall { ref mut attrs, .. })
            | Expr::MethodCall(ExprMethodCall { ref mut attrs, .. })
            | Expr::Tuple(ExprTuple { ref mut attrs, .. })
            | Expr::Binary(ExprBinary { ref mut attrs, .. })
            | Expr::Unary(ExprUnary { ref mut attrs, .. })
            | Expr::Lit(ExprLit { ref mut attrs, .. })
            | Expr::Cast(ExprCast { ref mut attrs, .. })
            | Expr::Type(ExprType { ref mut attrs, .. })
            | Expr::If(ExprIf { ref mut attrs, .. })
            | Expr::IfLet(ExprIfLet { ref mut attrs, .. })
            | Expr::While(ExprWhile { ref mut attrs, .. })
            | Expr::WhileLet(ExprWhileLet { ref mut attrs, .. })
            | Expr::ForLoop(ExprForLoop { ref mut attrs, .. })
            | Expr::Loop(ExprLoop { ref mut attrs, .. })
            | Expr::Match(ExprMatch { ref mut attrs, .. })
            | Expr::Closure(ExprClosure { ref mut attrs, .. })
            | Expr::Unsafe(ExprUnsafe { ref mut attrs, .. })
            | Expr::Block(ExprBlock { ref mut attrs, .. })
            | Expr::Assign(ExprAssign { ref mut attrs, .. })
            | Expr::AssignOp(ExprAssignOp { ref mut attrs, .. })
            | Expr::Field(ExprField { ref mut attrs, .. })
            | Expr::Index(ExprIndex { ref mut attrs, .. })
            | Expr::Range(ExprRange { ref mut attrs, .. })
            | Expr::Path(ExprPath { ref mut attrs, .. })
            | Expr::Reference(ExprReference { ref mut attrs, .. })
            | Expr::Break(ExprBreak { ref mut attrs, .. })
            | Expr::Continue(ExprContinue { ref mut attrs, .. })
            | Expr::Return(ExprReturn { ref mut attrs, .. })
            | Expr::Macro(ExprMacro { ref mut attrs, .. })
            | Expr::Struct(ExprStruct { ref mut attrs, .. })
            | Expr::Repeat(ExprRepeat { ref mut attrs, .. })
            | Expr::Paren(ExprParen { ref mut attrs, .. })
            | Expr::Group(ExprGroup { ref mut attrs, .. })
            | Expr::Try(ExprTry { ref mut attrs, .. })
            | Expr::Async(ExprAsync { ref mut attrs, .. })
            | Expr::TryBlock(ExprTryBlock { ref mut attrs, .. })
            | Expr::Yield(ExprYield { ref mut attrs, .. }) => mem::replace(attrs, new),
            Expr::Verbatim(_) => {
                // TODO
                Vec::new()
            }
        }
    }
}

ast_enum! {
    /// A struct or tuple struct field accessed in a struct literal or field
    /// expression.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub enum Member {
        /// A named field like `self.x`.
        Named(Ident),
        /// An unnamed field like `self.0`.
        Unnamed(Index),
    }
}

ast_struct! {
    /// The index of an unnamed tuple struct field.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub struct Index #manual_extra_traits {
        pub index: u32,
        pub span: Span,
    }
}

impl From<usize> for Index {
    fn from(index: usize) -> Index {
        assert!(index < u32::max_value() as usize);
        Index {
            index: index as u32,
            span: Span::call_site(),
        }
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for Index {}

#[cfg(feature = "extra-traits")]
impl PartialEq for Index {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

#[cfg(feature = "extra-traits")]
impl Hash for Index {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// The `::<>` explicit type parameters passed to a method call:
    /// `parse::<u64>()`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct MethodTurbofish {
        pub colon2_token: Token![::],
        pub lt_token: Token![<],
        pub args: Punctuated<GenericMethodArgument, Token![,]>,
        pub gt_token: Token![>],
    }
}

#[cfg(feature = "full")]
ast_enum! {
    /// An individual generic argument to a method, like `T`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub enum GenericMethodArgument {
        /// A type argument.
        Type(Type),
        /// A const expression. Must be inside of a block.
        ///
        /// NOTE: Identity expressions are represented as Type arguments, as
        /// they are indistinguishable syntactically.
        Const(Expr),
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A field-value pair in a struct literal.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct FieldValue {
        /// Attributes tagged on the field.
        pub attrs: Vec<Attribute>,

        /// Name or index of the field.
        pub member: Member,

        /// The colon in `Struct { x: x }`. If written in shorthand like
        /// `Struct { x }`, there is no colon.
        pub colon_token: Option<Token![:]>,

        /// Value of the field.
        pub expr: Expr,
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A lifetime labeling a `for`, `while`, or `loop`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct Label {
        pub name: Lifetime,
        pub colon_token: Token![:],
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A braced block containing Rust statements.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct Block {
        pub brace_token: token::Brace,
        /// Statements in a block
        pub stmts: Vec<Stmt>,
    }
}

#[cfg(feature = "full")]
ast_enum! {
    /// A statement, usually ending in a semicolon.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub enum Stmt {
        /// A local (let) binding.
        Local(Local),

        /// An item definition.
        Item(Item),

        /// Expr without trailing semicolon.
        Expr(Expr),

        /// Expression with trailing semicolon.
        Semi(Expr, Token![;]),
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A local `let` binding: `let x: u64 = s.parse()?`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct Local {
        pub attrs: Vec<Attribute>,
        pub let_token: Token![let],
        pub pats: Punctuated<Pat, Token![|]>,
        pub ty: Option<(Token![:], Box<Type>)>,
        pub init: Option<(Token![=], Box<Expr>)>,
        pub semi_token: Token![;],
    }
}

#[cfg(feature = "full")]
ast_enum_of_structs! {
    /// A pattern in a local binding, function signature, match expression, or
    /// various other places.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    ///
    /// # Syntax tree enum
    ///
    /// This type is a [syntax tree enum].
    ///
    /// [syntax tree enum]: enum.Expr.html#syntax-tree-enums
    // Clippy false positive
    // https://github.com/Manishearth/rust-clippy/issues/1241
    #[cfg_attr(feature = "cargo-clippy", allow(enum_variant_names))]
    pub enum Pat {
        /// A pattern that matches any value: `_`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Wild(PatWild {
            pub underscore_token: Token![_],
        }),

        /// A pattern that binds a new variable: `ref mut binding @ SUBPATTERN`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Ident(PatIdent {
            pub by_ref: Option<Token![ref]>,
            pub mutability: Option<Token![mut]>,
            pub ident: Ident,
            pub subpat: Option<(Token![@], Box<Pat>)>,
        }),

        /// A struct or struct variant pattern: `Variant { x, y, .. }`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Struct(PatStruct {
            pub path: Path,
            pub brace_token: token::Brace,
            pub fields: Punctuated<FieldPat, Token![,]>,
            pub dot2_token: Option<Token![..]>,
        }),

        /// A tuple struct or tuple variant pattern: `Variant(x, y, .., z)`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub TupleStruct(PatTupleStruct {
            pub path: Path,
            pub pat: PatTuple,
        }),

        /// A path pattern like `Color::Red`, optionally qualified with a
        /// self-type.
        ///
        /// Unquailfied path patterns can legally refer to variants, structs,
        /// constants or associated constants. Quailfied path patterns like
        /// `<A>::B::C` and `<A as Trait>::B::C` can only legally refer to
        /// associated constants.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Path(PatPath {
            pub qself: Option<QSelf>,
            pub path: Path,
        }),

        /// A tuple pattern: `(a, b)`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Tuple(PatTuple {
            pub paren_token: token::Paren,
            pub front: Punctuated<Pat, Token![,]>,
            pub dot2_token: Option<Token![..]>,
            pub comma_token: Option<Token![,]>,
            pub back: Punctuated<Pat, Token![,]>,
        }),

        /// A box pattern: `box v`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Box(PatBox {
            pub box_token: Token![box],
            pub pat: Box<Pat>,
        }),

        /// A reference pattern: `&mut (first, second)`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Ref(PatRef {
            pub and_token: Token![&],
            pub mutability: Option<Token![mut]>,
            pub pat: Box<Pat>,
        }),

        /// A literal pattern: `0`.
        ///
        /// This holds an `Expr` rather than a `Lit` because negative numbers
        /// are represented as an `Expr::Unary`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Lit(PatLit {
            pub expr: Box<Expr>,
        }),

        /// A range pattern: `1..=2`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Range(PatRange {
            pub lo: Box<Expr>,
            pub limits: RangeLimits,
            pub hi: Box<Expr>,
        }),

        /// A dynamically sized slice pattern: `[a, b, i.., y, z]`.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Slice(PatSlice {
            pub bracket_token: token::Bracket,
            pub front: Punctuated<Pat, Token![,]>,
            pub middle: Option<Box<Pat>>,
            pub dot2_token: Option<Token![..]>,
            pub comma_token: Option<Token![,]>,
            pub back: Punctuated<Pat, Token![,]>,
        }),

        /// A macro in expression position.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Macro(PatMacro {
            pub mac: Macro,
        }),

        /// Tokens in pattern position not interpreted by Syn.
        ///
        /// *This type is available if Syn is built with the `"full"` feature.*
        pub Verbatim(PatVerbatim #manual_extra_traits {
            pub tts: TokenStream,
        }),
    }
}

#[cfg(all(feature = "full", feature = "extra-traits"))]
impl Eq for PatVerbatim {}

#[cfg(all(feature = "full", feature = "extra-traits"))]
impl PartialEq for PatVerbatim {
    fn eq(&self, other: &Self) -> bool {
        TokenStreamHelper(&self.tts) == TokenStreamHelper(&other.tts)
    }
}

#[cfg(all(feature = "full", feature = "extra-traits"))]
impl Hash for PatVerbatim {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        TokenStreamHelper(&self.tts).hash(state);
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// One arm of a `match` expression: `0...10 => { return true; }`.
    ///
    /// As in:
    ///
    /// ```rust
    /// # fn f() -> bool {
    /// #     let n = 0;
    /// match n {
    ///     0...10 => {
    ///         return true;
    ///     }
    ///     // ...
    ///     # _ => {}
    /// }
    /// #   false
    /// # }
    /// ```
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct Arm {
        pub attrs: Vec<Attribute>,
        pub leading_vert: Option<Token![|]>,
        pub pats: Punctuated<Pat, Token![|]>,
        pub guard: Option<(Token![if], Box<Expr>)>,
        pub fat_arrow_token: Token![=>],
        pub body: Box<Expr>,
        pub comma: Option<Token![,]>,
    }
}

#[cfg(feature = "full")]
ast_enum! {
    /// Limit types of a range, inclusive or exclusive.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum RangeLimits {
        /// Inclusive at the beginning, exclusive at the end.
        HalfOpen(Token![..]),
        /// Inclusive at the beginning and end.
        Closed(Token![..=]),
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A single field in a struct pattern.
    ///
    /// Patterns like the fields of Foo `{ x, ref y, ref mut z }` are treated
    /// the same as `x: x, y: ref y, z: ref mut z` but there is no colon token.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct FieldPat {
        pub attrs: Vec<Attribute>,
        pub member: Member,
        pub colon_token: Option<Token![:]>,
        pub pat: Box<Pat>,
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
        | Expr::Async(..)
        | Expr::TryBlock(..) => false,
        _ => true,
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use path::parsing::old_mod_style_path_segment;
    #[cfg(feature = "full")]
    use path::parsing::ty_no_eq_after;

    use parse::{Parse, ParseStream, Result};
    #[cfg(feature = "full")]
    use parse_error;
    #[cfg(feature = "full")]
    use proc_macro2::TokenStream;
    use synom::Synom;

    macro_rules! named2 {
        ($name:ident ($($arg:ident : $argty:ty),*) -> $ret:ty, $($rest:tt)*) => {
            fn $name(input: ParseStream $(, $arg : $argty)*) -> Result<$ret> {
                named!(_synom ($($arg : $argty),*) -> $ret, $($rest)*);
                input.step_cursor(|cursor| _synom(*cursor $(, $arg)*))
            }
        };
        ($name:ident -> $ret:ty, $($rest:tt)*) => {
            fn $name(input: ParseStream) -> Result<$ret> {
                named!(_synom -> $ret, $($rest)*);
                input.step_cursor(|cursor| _synom(*cursor))
            }
        };
    }

    #[cfg(feature = "full")]
    macro_rules! ambiguous_expr {
        ($i:expr, $allow_struct:ident) => {
            shim!($i, ambiguous_expr, $allow_struct, AllowBlock(true))
        };
    }

    // When we're parsing expressions which occur before blocks, like in an if
    // statement's condition, we cannot parse a struct literal.
    //
    // Struct literals are ambiguous in certain positions
    // https://github.com/rust-lang/rfcs/pull/92
    #[derive(Copy, Clone)]
    pub struct AllowStruct(bool);

    #[derive(Copy, Clone)]
    pub struct AllowBlock(bool);

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
            option!($i, shim!(ambiguous_expr, $allow_struct, AllowBlock($allow_struct.0)))
        };
    }

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            ambiguous_expr(input, AllowStruct(true), AllowBlock(true))
        }
    }

    #[cfg(feature = "full")]
    fn expr_no_struct(input: ParseStream) -> Result<Expr> {
        ambiguous_expr(input, AllowStruct(false), AllowBlock(true))
    }

    // Parse an arbitrary expression.
    #[cfg(feature = "full")]
    fn ambiguous_expr(input: ParseStream, allow_struct: AllowStruct, allow_block: AllowBlock) -> Result<Expr> {
        assign_expr(input, allow_struct, allow_block)
    }

    #[cfg(not(feature = "full"))]
    fn ambiguous_expr(input: ParseStream, allow_struct: AllowStruct, allow_block: AllowBlock) -> Result<Expr> {
        // NOTE: We intentionally skip assign_expr, placement_expr, and
        // range_expr as they are only parsed in full mode.
        or_expr(input, allow_struct, allow_block)
    }

    // Parse a left-associative binary operator.
    macro_rules! binop {
        ($name:ident, $next:ident, |$var:ident| $parse_op:expr) => {
            fn $name(input: ParseStream, allow_struct: AllowStruct, allow_block: AllowBlock) -> Result<Expr> {
                let $var = input;
                let mut e: Expr = $next(input, allow_struct, allow_block)?;
                while let Some(op) = $parse_op {
                    e = Expr::Binary(ExprBinary {
                        attrs: Vec::new(),
                        left: Box::new(e),
                        op: op,
                        right: Box::new($next(input, allow_struct, AllowBlock(true))?),
                    });
                }
                Ok(e)
            }
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
    named2!(assign_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, do_parse!(
        mut e: shim!(placement_expr, allow_struct, allow_block) >>
        alt!(
            do_parse!(
                eq: punct!(=) >>
                // Recurse into self to parse right-associative operator.
                rhs: shim!(assign_expr, allow_struct, AllowBlock(true)) >>
                ({
                    e = ExprAssign {
                        attrs: Vec::new(),
                        left: Box::new(e),
                        eq_token: eq,
                        right: Box::new(rhs),
                    }.into();
                })
            )
            |
            do_parse!(
                op: shim!(BinOp::parse_assign_op) >>
                // Recurse into self to parse right-associative operator.
                rhs: shim!(assign_expr, allow_struct, AllowBlock(true)) >>
                ({
                    e = ExprAssignOp {
                        attrs: Vec::new(),
                        left: Box::new(e),
                        op: op,
                        right: Box::new(rhs),
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
    named2!(placement_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, do_parse!(
        mut e: shim!(range_expr, allow_struct, allow_block) >>
        alt!(
            do_parse!(
                arrow: punct!(<-) >>
                // Recurse into self to parse right-associative operator.
                rhs: shim!(placement_expr, allow_struct, AllowBlock(true)) >>
                ({
                    e = ExprInPlace {
                        attrs: Vec::new(),
                        // op: BinOp::Place(larrow),
                        place: Box::new(e),
                        arrow_token: arrow,
                        value: Box::new(rhs),
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
    named2!(range_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, do_parse!(
        mut e: shim!(or_expr, allow_struct, allow_block) >>
        many0!(do_parse!(
            limits: syn!(RangeLimits) >>
            // We don't want to allow blocks here if we don't allow structs. See
            // the reasoning for `opt_ambiguous_expr!` above.
            hi: option!(shim!(or_expr, allow_struct, AllowBlock(allow_struct.0))) >>
            ({
                e = ExprRange {
                    attrs: Vec::new(),
                    from: Some(Box::new(e)),
                    limits: limits,
                    to: hi.map(|e| Box::new(e)),
                }.into();
            })
        )) >>
        (e)
    ));

    // <and> || <and> ...
    binop!(or_expr, and_expr, |input| {
        if input.peek(Token![||]) {
            Some(BinOp::Or(input.parse()?))
        } else {
            None
        }
    });

    // <compare> && <compare> ...
    binop!(and_expr, compare_expr, |input| {
        if input.peek(Token![&&]) {
            Some(BinOp::And(input.parse()?))
        } else {
            None
        }
    });

    // <bitor> == <bitor> ...
    // <bitor> != <bitor> ...
    // <bitor> >= <bitor> ...
    // <bitor> <= <bitor> ...
    // <bitor> > <bitor> ...
    // <bitor> < <bitor> ...
    //
    // NOTE: This operator appears to be parsed as left-associative, but errors
    // if it is used in a non-associative manner.
    binop!(compare_expr, bitor_expr, |input| {
        if input.peek(Token![==]) {
            Some(BinOp::Eq(input.parse()?))
        } else if input.peek(Token![!=]) {
            Some(BinOp::Ne(input.parse()?))
        // must be before `<`
        } else if input.peek(Token![<=]) {
            Some(BinOp::Le(input.parse()?))
        // must be before `>`
        } else if input.peek(Token![>=]) {
            Some(BinOp::Ge(input.parse()?))
        } else if input.peek(Token![<]) && !input.peek(Token![<<]) && !input.peek(Token![<-]) {
            Some(BinOp::Lt(input.parse()?))
        } else if input.peek(Token![>]) && !input.peek(Token![>>]) {
            Some(BinOp::Gt(input.parse()?))
        } else {
            None
        }
    });

    // <bitxor> | <bitxor> ...
    binop!(bitor_expr, bitxor_expr, |input| {
        if input.peek(Token![|]) && !input.peek(Token![||]) && !input.peek(Token![|=]) {
            Some(BinOp::BitOr(input.parse()?))
        } else {
            None
        }
    });

    // <bitand> ^ <bitand> ...
    binop!(bitxor_expr, bitand_expr, |input| {
        if input.peek(Token![^]) && !input.peek(Token![^=]) {
            Some(BinOp::BitXor(input.parse()?))
        } else {
            None
        }
    });

    // <shift> & <shift> ...
    binop!(bitand_expr, shift_expr, |input| {
        if input.peek(Token![&]) && !input.peek(Token![&&]) && !input.peek(Token![&=]) {
            Some(BinOp::BitAnd(input.parse()?))
        } else {
            None
        }
    });

    // <arith> << <arith> ...
    // <arith> >> <arith> ...
    binop!(shift_expr, arith_expr, |input| {
        if input.peek(Token![<<]) && !input.peek(Token![<<=]) {
            Some(BinOp::Shl(input.parse()?))
        } else if input.peek(Token![>>]) && !input.peek(Token![>>=]) {
            Some(BinOp::Shr(input.parse()?))
        } else {
            None
        }
    });

    // <term> + <term> ...
    // <term> - <term> ...
    binop!(arith_expr, term_expr, |input| {
        if input.peek(Token![+]) && !input.peek(Token![+=]) {
            Some(BinOp::Add(input.parse()?))
        } else if input.peek(Token![-]) && !input.peek(Token![-=]) {
            Some(BinOp::Sub(input.parse()?))
        } else {
            None
        }
    });

    // <cast> * <cast> ...
    // <cast> / <cast> ...
    // <cast> % <cast> ...
    binop!(term_expr, cast_expr, |input| {
        if input.peek(Token![*]) && !input.peek(Token![*=]) {
            Some(BinOp::Mul(input.parse()?))
        } else if input.peek(Token![/]) && !input.peek(Token![/=]) {
            Some(BinOp::Div(input.parse()?))
        } else if input.peek(Token![%]) && !input.peek(Token![%=]) {
            Some(BinOp::Rem(input.parse()?))
        } else {
            None
        }
    });

    // <unary> as <ty>
    // <unary> : <ty>
    #[cfg(feature = "full")]
    named2!(cast_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, do_parse!(
        mut e: shim!(unary_expr, allow_struct, allow_block) >>
        many0!(alt!(
            do_parse!(
                as_: keyword!(as) >>
                // We can't accept `A + B` in cast expressions, as it's
                // ambiguous with the + expression.
                ty: shim!(Type::without_plus) >>
                ({
                    e = ExprCast {
                        attrs: Vec::new(),
                        expr: Box::new(e),
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
                ty: shim!(Type::without_plus) >>
                ({
                    e = ExprType {
                        attrs: Vec::new(),
                        expr: Box::new(e),
                        colon_token: colon,
                        ty: Box::new(ty),
                    }.into();
                })
            )
        )) >>
        (e)
    ));

    // <unary> as <ty>
    #[cfg(not(feature = "full"))]
    named2!(cast_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, do_parse!(
        mut e: shim!(unary_expr, allow_struct, allow_block) >>
        many0!(do_parse!(
            as_: keyword!(as) >>
            // We can't accept `A + B` in cast expressions, as it's
            // ambiguous with the + expression.
            ty: shim!(Type::without_plus) >>
            ({
                e = ExprCast {
                    attrs: Vec::new(),
                    expr: Box::new(e),
                    as_token: as_,
                    ty: Box::new(ty),
                }.into();
            })
        )) >>
        (e)
    ));

    // <UnOp> <trailer>
    // & <trailer>
    // &mut <trailer>
    // box <trailer>
    #[cfg(feature = "full")]
    named2!(unary_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, alt!(
        do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            op: syn!(UnOp) >>
            expr: shim!(unary_expr, allow_struct, AllowBlock(true)) >>
            (ExprUnary {
                attrs: attrs,
                op: op,
                expr: Box::new(expr),
            }.into())
        )
        |
        do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            and: punct!(&) >>
            mutability: option!(keyword!(mut)) >>
            expr: shim!(unary_expr, allow_struct, AllowBlock(true)) >>
            (ExprReference {
                attrs: attrs,
                and_token: and,
                mutability: mutability,
                expr: Box::new(expr),
            }.into())
        )
        |
        do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            box_: keyword!(box) >>
            expr: shim!(unary_expr, allow_struct, AllowBlock(true)) >>
            (ExprBox {
                attrs: attrs,
                box_token: box_,
                expr: Box::new(expr),
            }.into())
        )
        |
        shim!(trailer_expr, allow_struct, allow_block)
    ));

    // XXX: This duplication is ugly
    #[cfg(not(feature = "full"))]
    named2!(unary_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, alt!(
        do_parse!(
            op: syn!(UnOp) >>
            expr: shim!(unary_expr, allow_struct, AllowBlock(true)) >>
            (ExprUnary {
                attrs: Vec::new(),
                op: op,
                expr: Box::new(expr),
            }.into())
        )
        |
        shim!(trailer_expr, allow_struct, allow_block)
    ));

    #[cfg(feature = "full")]
    fn take_outer(attrs: &mut Vec<Attribute>) -> Vec<Attribute> {
        let mut outer = Vec::new();
        let mut inner = Vec::new();
        for attr in mem::replace(attrs, Vec::new()) {
            match attr.style {
                AttrStyle::Outer => outer.push(attr),
                AttrStyle::Inner(_) => inner.push(attr),
            }
        }
        *attrs = inner;
        outer
    }

    // <atom> (..<args>) ...
    // <atom> . <ident> (..<args>) ...
    // <atom> . <ident> ...
    // <atom> . <lit> ...
    // <atom> [ <expr> ] ...
    // <atom> ? ...
    #[cfg(feature = "full")]
    named2!(trailer_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, do_parse!(
        mut e: shim!(atom_expr, allow_struct, allow_block) >>
        outer_attrs: value!({
            let mut attrs = e.replace_attrs(Vec::new());
            let outer_attrs = take_outer(&mut attrs);
            e.replace_attrs(attrs);
            outer_attrs
        }) >>
        many0!(alt!(
            tap!(args: shim!(and_call) => {
                let (paren, args) = args;
                e = ExprCall {
                    attrs: Vec::new(),
                    func: Box::new(e),
                    args: args,
                    paren_token: paren,
                }.into();
            })
            |
            tap!(more: shim!(and_method_call) => {
                let mut call = more;
                call.receiver = Box::new(e);
                e = call.into();
            })
            |
            tap!(field: shim!(and_field) => {
                let (token, member) = field;
                e = ExprField {
                    attrs: Vec::new(),
                    base: Box::new(e),
                    dot_token: token,
                    member: member,
                }.into();
            })
            |
            tap!(i: shim!(and_index) => {
                let (bracket, i) = i;
                e = ExprIndex {
                    attrs: Vec::new(),
                    expr: Box::new(e),
                    bracket_token: bracket,
                    index: Box::new(i),
                }.into();
            })
            |
            tap!(question: punct!(?) => {
                e = ExprTry {
                    attrs: Vec::new(),
                    expr: Box::new(e),
                    question_token: question,
                }.into();
            })
        )) >>
        ({
            let mut attrs = outer_attrs;
            attrs.extend(e.replace_attrs(Vec::new()));
            e.replace_attrs(attrs);
            e
        })
    ));

    // XXX: Duplication == ugly
    #[cfg(not(feature = "full"))]
    named2!(trailer_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, do_parse!(
        mut e: shim!(atom_expr, allow_struct, allow_block) >>
        many0!(alt!(
            tap!(args: shim!(and_call) => {
                e = ExprCall {
                    attrs: Vec::new(),
                    func: Box::new(e),
                    paren_token: args.0,
                    args: args.1,
                }.into();
            })
            |
            tap!(field: shim!(and_field) => {
                let (token, member) = field;
                e = ExprField {
                    attrs: Vec::new(),
                    base: Box::new(e),
                    dot_token: token,
                    member: member,
                }.into();
            })
            |
            tap!(i: shim!(and_index) => {
                e = ExprIndex {
                    attrs: Vec::new(),
                    expr: Box::new(e),
                    bracket_token: i.0,
                    index: Box::new(i.1),
                }.into();
            })
        )) >>
        (e)
    ));

    // Parse all atomic expressions which don't have to worry about precedence
    // interactions, as they are fully contained.
    #[cfg(feature = "full")]
    named2!(atom_expr(allow_struct: AllowStruct, allow_block: AllowBlock) -> Expr, alt!(
        syn!(ExprGroup) => { Expr::Group } // must be placed first
        |
        syn!(ExprLit) => { Expr::Lit } // must be before expr_struct
        |
        // must be before ExprStruct
        syn!(ExprAsync) => { Expr::Async }
        |
        // must be before ExprStruct
        syn!(ExprTryBlock) => { Expr::TryBlock }
        |
        // must be before expr_path
        cond_reduce!(allow_struct.0, syn!(ExprStruct)) => { Expr::Struct }
        |
        syn!(ExprParen) => { Expr::Paren } // must be before expr_tup
        |
        syn!(ExprMacro) => { Expr::Macro } // must be before expr_path
        |
        shim!(expr_break, allow_struct) // must be before expr_path
        |
        syn!(ExprContinue) => { Expr::Continue } // must be before expr_path
        |
        shim!(expr_ret, allow_struct) // must be before expr_path
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
        syn!(ExprYield) => { Expr::Yield }
        |
        syn!(ExprUnsafe) => { Expr::Unsafe }
        |
        shim!(expr_closure, allow_struct)
        |
        cond_reduce!(allow_block.0, syn!(ExprBlock)) => { Expr::Block }
        |
        // NOTE: This is the prefix-form of range
        shim!(expr_range, allow_struct)
        |
        syn!(ExprPath) => { Expr::Path }
        |
        syn!(ExprRepeat) => { Expr::Repeat }
    ));

    #[cfg(not(feature = "full"))]
    named2!(atom_expr(_allow_struct: AllowStruct, _allow_block: AllowBlock) -> Expr, alt!(
        syn!(ExprLit) => { Expr::Lit }
        |
        syn!(ExprParen) => { Expr::Paren }
        |
        syn!(ExprPath) => { Expr::Path }
    ));

    #[cfg(feature = "full")]
    named2!(expr_nosemi -> Expr, do_parse!(
        nosemi: alt!(
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
            syn!(ExprTryBlock) => { Expr::TryBlock }
            |
            syn!(ExprYield) => { Expr::Yield }
            |
            syn!(ExprUnsafe) => { Expr::Unsafe }
            |
            syn!(ExprBlock) => { Expr::Block }
        ) >>
        // If the next token is a `.` or a `?` it is special-cased to parse
        // as an expression instead of a blockexpression.
        not!(punct!(.)) >>
        not!(punct!(?)) >>
        (nosemi)
    ));

    impl Synom for ExprLit {
        #[cfg(not(feature = "full"))]
        named!(parse -> Self, do_parse!(
            lit: syn!(Lit) >>
            (ExprLit {
                attrs: Vec::new(),
                lit: lit,
            })
        ));

        #[cfg(feature = "full")]
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            lit: syn!(Lit) >>
            (ExprLit {
                attrs: attrs,
                lit: lit,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprMacro {
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            mac: syn!(Macro) >>
            (ExprMacro {
                attrs: attrs,
                mac: mac,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprGroup {
        named!(parse -> Self, do_parse!(
            e: old_grouped!(syn!(Expr)) >>
            (ExprGroup {
                attrs: Vec::new(),
                expr: Box::new(e.1),
                group_token: e.0,
            })
        ));
    }

    impl Synom for ExprParen {
        #[cfg(not(feature = "full"))]
        named!(parse -> Self, do_parse!(
            e: parens!(syn!(Expr)) >>
            (ExprParen {
                attrs: Vec::new(),
                paren_token: e.0,
                expr: Box::new(e.1),
            })
        ));

        #[cfg(feature = "full")]
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            e: parens!(tuple!(
                many0!(Attribute::old_parse_inner),
                syn!(Expr),
            )) >>
            (ExprParen {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((e.1).0);
                    attrs
                },
                paren_token: e.0,
                expr: Box::new((e.1).1),
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprArray {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            elems: brackets!(tuple!(
                many0!(Attribute::old_parse_inner),
                call!(Punctuated::parse_terminated),
            )) >>
            (ExprArray {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((elems.1).0);
                    attrs
                },
                bracket_token: elems.0,
                elems: (elems.1).1,
            })
        ));
    }

    named2!(and_call -> (token::Paren, Punctuated<Expr, Token![,]>),
        parens!(Punctuated::parse_terminated)
    );

    #[cfg(feature = "full")]
    named2!(and_method_call -> ExprMethodCall, do_parse!(
        dot: punct!(.) >>
        method: syn!(Ident) >>
        turbofish: option!(tuple!(
            punct!(::),
            punct!(<),
            call!(Punctuated::parse_terminated),
            punct!(>),
        )) >>
        args: parens!(Punctuated::parse_terminated) >>
        ({
            ExprMethodCall {
                attrs: Vec::new(),
                // this expr will get overwritten after being returned
                receiver: Box::new(Expr::Verbatim(ExprVerbatim {
                    tts: TokenStream::new(),
                })),

                method: method,
                turbofish: turbofish.map(|fish| MethodTurbofish {
                    colon2_token: fish.0,
                    lt_token: fish.1,
                    args: fish.2,
                    gt_token: fish.3,
                }),
                args: args.1,
                paren_token: args.0,
                dot_token: dot,
            }
        })
    ));

    #[cfg(feature = "full")]
    impl Synom for GenericMethodArgument {
        // TODO parse const generics as well
        named!(parse -> Self, map!(ty_no_eq_after, GenericMethodArgument::Type));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprTuple {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            elems: parens!(tuple!(
                many0!(Attribute::old_parse_inner),
                call!(Punctuated::parse_terminated),
            )) >>
            (ExprTuple {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((elems.1).0);
                    attrs
                },
                elems: (elems.1).1,
                paren_token: elems.0,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprIfLet {
        named!(parse -> Self, do_parse!(
            if_: keyword!(if) >>
            let_: keyword!(let) >>
            pats: call!(Punctuated::parse_separated_nonempty) >>
            eq: punct!(=) >>
            cond: shim!(expr_no_struct) >>
            then_block: braces!(Block::old_parse_within) >>
            else_block: option!(shim!(else_block)) >>
            (ExprIfLet {
                attrs: Vec::new(),
                pats: pats,
                let_token: let_,
                eq_token: eq,
                expr: Box::new(cond),
                then_branch: Block {
                    brace_token: then_block.0,
                    stmts: then_block.1,
                },
                if_token: if_,
                else_branch: else_block,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprIf {
        named!(parse -> Self, do_parse!(
            if_: keyword!(if) >>
            cond: shim!(expr_no_struct) >>
            then_block: braces!(Block::old_parse_within) >>
            else_block: option!(shim!(else_block)) >>
            (ExprIf {
                attrs: Vec::new(),
                cond: Box::new(cond),
                then_branch: Block {
                    brace_token: then_block.0,
                    stmts: then_block.1,
                },
                if_token: if_,
                else_branch: else_block,
            })
        ));
    }

    #[cfg(feature = "full")]
    named2!(else_block -> (Token![else], Box<Expr>), do_parse!(
        else_: keyword!(else) >>
        expr: alt!(
            syn!(ExprIf) => { Expr::If }
            |
            syn!(ExprIfLet) => { Expr::IfLet }
            |
            do_parse!(
                else_block: braces!(Block::old_parse_within) >>
                (Expr::Block(ExprBlock {
                    attrs: Vec::new(),
                    label: None,
                    block: Block {
                        brace_token: else_block.0,
                        stmts: else_block.1,
                    },
                }))
            )
        ) >>
        (else_, Box::new(expr))
    ));

    #[cfg(feature = "full")]
    impl Synom for ExprForLoop {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            label: option!(syn!(Label)) >>
            for_: keyword!(for) >>
            pat: syn!(Pat) >>
            in_: keyword!(in) >>
            expr: shim!(expr_no_struct) >>
            block: braces!(tuple!(
                many0!(Attribute::old_parse_inner),
                shim!(Block::parse_within),
            )) >>
            (ExprForLoop {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((block.1).0);
                    attrs
                },
                label: label,
                for_token: for_,
                pat: Box::new(pat),
                in_token: in_,
                expr: Box::new(expr),
                body: Block {
                    brace_token: block.0,
                    stmts: (block.1).1,
                },
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprLoop {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            label: option!(syn!(Label)) >>
            loop_: keyword!(loop) >>
            block: braces!(tuple!(
                many0!(Attribute::old_parse_inner),
                shim!(Block::parse_within),
            )) >>
            (ExprLoop {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((block.1).0);
                    attrs
                },
                label: label,
                loop_token: loop_,
                body: Block {
                    brace_token: block.0,
                    stmts: (block.1).1,
                },
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprMatch {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            match_: keyword!(match) >>
            obj: shim!(expr_no_struct) >>
            braced_content: braces!(tuple!(
                many0!(Attribute::old_parse_inner),
                many0!(syn!(Arm)),
            )) >>
            (ExprMatch {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((braced_content.1).0);
                    attrs
                },
                expr: Box::new(obj),
                match_token: match_,
                brace_token: braced_content.0,
                arms: (braced_content.1).1,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprTryBlock {
        named!(parse -> Self, do_parse!(
            try_token: keyword!(try) >>
            block: syn!(Block) >>
            (ExprTryBlock {
                attrs: Vec::new(),
                try_token: try_token,
                block: block,
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
            attrs: many0!(Attribute::old_parse_outer) >>
            leading_vert: option!(punct!(|)) >>
            pats: call!(Punctuated::parse_separated_nonempty) >>
            guard: option!(tuple!(keyword!(if), syn!(Expr))) >>
            fat_arrow: punct!(=>) >>
            body: do_parse!(
                expr: alt!(shim!(expr_nosemi) | syn!(Expr)) >>
                comma: switch!(value!(arm_expr_requires_comma(&expr)),
                    true => alt!(
                        input_end!() => { |_| None }
                        |
                        punct!(,) => { Some }
                    )
                    |
                    false => option!(punct!(,))
                ) >>
                (expr, comma)
            ) >>
            (Arm {
                fat_arrow_token: fat_arrow,
                attrs: attrs,
                leading_vert: leading_vert,
                pats: pats,
                guard: guard.map(|(if_, guard)| (if_, Box::new(guard))),
                body: Box::new(body.0),
                comma: body.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    named2!(expr_closure(allow_struct: AllowStruct) -> Expr, do_parse!(
        attrs: many0!(Attribute::old_parse_outer) >>
        asyncness: option!(keyword!(async)) >>
        movability: option!(cond_reduce!(asyncness.is_none(), keyword!(static))) >>
        capture: option!(keyword!(move)) >>
        or1: punct!(|) >>
        inputs: call!(Punctuated::parse_terminated_with, fn_arg) >>
        or2: punct!(|) >>
        ret_and_body: alt!(
            do_parse!(
                arrow: punct!(->) >>
                ty: syn!(Type) >>
                body: syn!(Block) >>
                (
                    ReturnType::Type(arrow, Box::new(ty)),
                    Expr::Block(ExprBlock {
                        attrs: Vec::new(),
                        label: None,
                        block: body,
                    },
                ))
            )
            |
            map!(ambiguous_expr!(allow_struct), |e| (ReturnType::Default, e))
        ) >>
        (Expr::Closure(ExprClosure {
            attrs: attrs,
            asyncness: asyncness,
            movability: movability,
            capture: capture,
            or1_token: or1,
            inputs: inputs,
            or2_token: or2,
            output: ret_and_body.0,
            body: Box::new(ret_and_body.1),
        }))
    ));

    #[cfg(feature = "full")]
    impl Synom for ExprAsync {
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            async_token: keyword!(async) >>
            capture: option!(keyword!(move)) >>
            block: syn!(Block) >>
            (ExprAsync {
                attrs: attrs,
                async_token: async_token,
                capture: capture,
                block: block,
            })
        ));
    }

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
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            label: option!(syn!(Label)) >>
            while_: keyword!(while) >>
            cond: shim!(expr_no_struct) >>
            block: braces!(tuple!(
                many0!(Attribute::old_parse_inner),
                shim!(Block::parse_within),
            )) >>
            (ExprWhile {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((block.1).0);
                    attrs
                },
                label: label,
                while_token: while_,
                cond: Box::new(cond),
                body: Block {
                    brace_token: block.0,
                    stmts: (block.1).1,
                },
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprWhileLet {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            label: option!(syn!(Label)) >>
            while_: keyword!(while) >>
            let_: keyword!(let) >>
            pats: call!(Punctuated::parse_separated_nonempty) >>
            eq: punct!(=) >>
            value: shim!(expr_no_struct) >>
            block: braces!(tuple!(
                many0!(Attribute::old_parse_inner),
                shim!(Block::parse_within),
            )) >>
            (ExprWhileLet {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((block.1).0);
                    attrs
                },
                label: label,
                while_token: while_,
                let_token: let_,
                pats: pats,
                eq_token: eq,
                expr: Box::new(value),
                body: Block {
                    brace_token: block.0,
                    stmts: (block.1).1,
                },
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for Label {
        named!(parse -> Self, do_parse!(
            name: syn!(Lifetime) >>
            colon: punct!(:) >>
            (Label {
                name: name,
                colon_token: colon,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprContinue {
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            cont: keyword!(continue) >>
            label: option!(syn!(Lifetime)) >>
            (ExprContinue {
                attrs: attrs,
                continue_token: cont,
                label: label,
            })
        ));
    }

    #[cfg(feature = "full")]
    named2!(expr_break(allow_struct: AllowStruct) -> Expr, do_parse!(
        attrs: many0!(Attribute::old_parse_outer) >>
        break_: keyword!(break) >>
        label: option!(syn!(Lifetime)) >>
        // We can't allow blocks after a `break` expression when we wouldn't
        // allow structs, as this expression is ambiguous.
        val: opt_ambiguous_expr!(allow_struct) >>
        (ExprBreak {
            attrs: attrs,
            label: label,
            expr: val.map(Box::new),
            break_token: break_,
        }.into())
    ));

    #[cfg(feature = "full")]
    named2!(expr_ret(allow_struct: AllowStruct) -> Expr, do_parse!(
        attrs: many0!(Attribute::old_parse_outer) >>
        return_: keyword!(return) >>
        // NOTE: return is greedy and eats blocks after it even when in a
        // position where structs are not allowed, such as in if statement
        // conditions. For example:
        //
        // if return { println!("A") } {} // Prints "A"
        ret_value: option!(ambiguous_expr!(allow_struct)) >>
        (ExprReturn {
            attrs: attrs,
            expr: ret_value.map(Box::new),
            return_token: return_,
        }.into())
    ));

    #[cfg(feature = "full")]
    impl Synom for ExprStruct {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            path: syn!(Path) >>
            data: braces!(do_parse!(
                inner_attrs: many0!(Attribute::old_parse_inner) >>
                fields: call!(Punctuated::parse_terminated) >>
                base: option!(cond!(fields.empty_or_trailing(), do_parse!(
                    dots: punct!(..) >>
                    base: syn!(Expr) >>
                    (dots, base)
                ))) >>
                (inner_attrs, fields, base)
            )) >>
            ({
                let (brace, (inner_attrs, fields, base)) = data;
                let (dots, rest) = match base.and_then(|b| b) {
                    Some((dots, base)) => (Some(dots), Some(base)),
                    None => (None, None),
                };
                ExprStruct {
                    attrs: {
                        let mut attrs = outer_attrs;
                        attrs.extend(inner_attrs);
                        attrs
                    },
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
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            field_value: alt!(
                tuple!(syn!(Member), map!(punct!(:), Some), syn!(Expr))
                |
                map!(syn!(Ident), |name| (
                    Member::Named(name.clone()),
                    None,
                    Expr::Path(ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: name.into(),
                    }),
                ))
            ) >>
            (FieldValue {
                attrs: attrs,
                member: field_value.0,
                colon_token: field_value.1,
                expr: field_value.2,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprRepeat {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            data: brackets!(tuple!(
                many0!(Attribute::old_parse_inner),
                syn!(Expr),
                punct!(;),
                syn!(Expr),
            )) >>
            (ExprRepeat {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((data.1).0);
                    attrs
                },
                expr: Box::new((data.1).1),
                len: Box::new((data.1).3),
                bracket_token: data.0,
                semi_token: (data.1).2,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprUnsafe {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            unsafe_: keyword!(unsafe) >>
            block: braces!(tuple!(
                many0!(Attribute::old_parse_inner),
                shim!(Block::parse_within),
            )) >>
            (ExprUnsafe {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((block.1).0);
                    attrs
                },
                unsafe_token: unsafe_,
                block: Block {
                    brace_token: block.0,
                    stmts: (block.1).1,
                },
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for ExprBlock {
        named!(parse -> Self, do_parse!(
            outer_attrs: many0!(Attribute::old_parse_outer) >>
            label: option!(syn!(Label)) >>
            block: braces!(tuple!(
                many0!(Attribute::old_parse_inner),
                shim!(Block::parse_within),
            )) >>
            (ExprBlock {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend((block.1).0);
                    attrs
                },
                label: label,
                block: Block {
                    brace_token: block.0,
                    stmts: (block.1).1,
                },
            })
        ));
    }

    #[cfg(feature = "full")]
    named2!(expr_range(allow_struct: AllowStruct) -> Expr, do_parse!(
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
            punct!(...) => { |dot3| RangeLimits::Closed(Token![..=](dot3.spans)) }
            |
            punct!(..) => { RangeLimits::HalfOpen }
        ));
    }

    impl Synom for ExprPath {
        #[cfg(not(feature = "full"))]
        named!(parse -> Self, do_parse!(
            pair: shim!(qpath) >>
            (ExprPath {
                attrs: Vec::new(),
                qself: pair.0,
                path: pair.1,
            })
        ));

        #[cfg(feature = "full")]
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::old_parse_outer) >>
            pair: shim!(qpath) >>
            (ExprPath {
                attrs: attrs,
                qself: pair.0,
                path: pair.1,
            })
        ));
    }

    named2!(path -> Path, do_parse!(
        colon: option!(punct!(::)) >>
        segments: call!(Punctuated::<_, Token![::]>::parse_separated_nonempty_with, path_segment) >>
        cond_reduce!(segments.first().map_or(true, |seg| seg.value().ident != "dyn")) >>
        (Path {
            leading_colon: colon,
            segments: segments,
        })
    ));

    named!(path_segment -> PathSegment, alt!(
        do_parse!(
            ident: syn!(Ident) >>
            colon2: punct!(::) >>
            lt: punct!(<) >>
            args: call!(Punctuated::parse_terminated) >>
            gt: punct!(>) >>
            (PathSegment {
                ident: ident,
                arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    colon2_token: Some(colon2),
                    lt_token: lt,
                    args: args,
                    gt_token: gt,
                }),
            })
        )
        |
        old_mod_style_path_segment
    ));

    named2!(qpath -> (Option<QSelf>, Path), alt!(
        map!(shim!(path), |p| (None, p))
        |
        do_parse!(
            lt: punct!(<) >>
            this: syn!(Type) >>
            path: option!(tuple!(keyword!(as), syn!(Path))) >>
            gt: punct!(>) >>
            colon2: punct!(::) >>
            rest: call!(Punctuated::parse_separated_nonempty_with, path_segment) >>
            ({
                let (pos, as_, path) = match path {
                    Some((as_, mut path)) => {
                        let pos = path.segments.len();
                        path.segments.push_punct(colon2);
                        path.segments.extend(rest.into_pairs());
                        (pos, Some(as_), path)
                    }
                    None => {
                        (0, None, Path {
                            leading_colon: Some(colon2),
                            segments: rest,
                        })
                    }
                };
                (Some(QSelf {
                    lt_token: lt,
                    ty: Box::new(this),
                    position: pos,
                    as_token: as_,
                    gt_token: gt,
                }), path)
            })
        )
        |
        map!(keyword!(self), |s| (None, s.into()))
    ));

    named2!(and_field -> (Token![.], Member), tuple!(punct!(.), syn!(Member)));

    named2!(and_index -> (token::Bracket, Expr), brackets!(syn!(Expr)));

    #[cfg(feature = "full")]
    impl Synom for Block {
        named!(parse -> Self, do_parse!(
            stmts: braces!(Block::old_parse_within) >>
            (Block {
                brace_token: stmts.0,
                stmts: stmts.1,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Block {
        pub fn parse_within(input: ParseStream) -> Result<Vec<Stmt>> {
            input.step_cursor(|cursor| Self::old_parse_within(*cursor))
        }

        named!(old_parse_within -> Vec<Stmt>, do_parse!(
            many0!(punct!(;)) >>
            mut standalone: many0!(do_parse!(
                stmt: syn!(Stmt) >>
                many0!(punct!(;)) >>
                (stmt)
            )) >>
            last: option!(do_parse!(
                attrs: many0!(Attribute::old_parse_outer) >>
                mut e: syn!(Expr) >>
                ({
                    e.replace_attrs(attrs);
                    Stmt::Expr(e)
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
            shim!(stmt_mac)
            |
            shim!(stmt_local)
            |
            shim!(stmt_item)
            |
            shim!(stmt_blockexpr)
            |
            shim!(stmt_expr)
        ));
    }

    #[cfg(feature = "full")]
    named2!(stmt_mac -> Stmt, do_parse!(
        attrs: many0!(Attribute::old_parse_outer) >>
        what: call!(Path::old_parse_mod_style) >>
        bang: punct!(!) >>
    // Only parse braces here; paren and bracket will get parsed as
    // expression statements
        data: braces!(syn!(TokenStream)) >>
        semi: option!(punct!(;)) >>
        (Stmt::Item(Item::Macro(ItemMacro {
            attrs: attrs,
            ident: None,
            mac: Macro {
                path: what,
                bang_token: bang,
                delimiter: MacroDelimiter::Brace(data.0),
                tts: data.1,
            },
            semi_token: semi,
        })))
    ));

    #[cfg(feature = "full")]
    named2!(stmt_local -> Stmt, do_parse!(
        attrs: many0!(Attribute::old_parse_outer) >>
        let_: keyword!(let) >>
        pats: call!(Punctuated::parse_separated_nonempty) >>
        ty: option!(tuple!(punct!(:), syn!(Type))) >>
        init: option!(tuple!(punct!(=), syn!(Expr))) >>
        semi: punct!(;) >>
        (Stmt::Local(Local {
            attrs: attrs,
            let_token: let_,
            pats: pats,
            ty: ty.map(|(colon, ty)| (colon, Box::new(ty))),
            init: init.map(|(eq, expr)| (eq, Box::new(expr))),
            semi_token: semi,
        }))
    ));

    #[cfg(feature = "full")]
    named2!(stmt_item -> Stmt, map!(syn!(Item), |i| Stmt::Item(i)));

    #[cfg(feature = "full")]
    named2!(stmt_blockexpr -> Stmt, do_parse!(
        mut attrs: many0!(Attribute::old_parse_outer) >>
        mut e: shim!(expr_nosemi) >>
        semi: option!(punct!(;)) >>
        ({
            attrs.extend(e.replace_attrs(Vec::new()));
            e.replace_attrs(attrs);
            if let Some(semi) = semi {
                Stmt::Semi(e, semi)
            } else {
                Stmt::Expr(e)
            }
        })
    ));

    #[cfg(feature = "full")]
    named2!(stmt_expr -> Stmt, do_parse!(
        mut attrs: many0!(Attribute::old_parse_outer) >>
        mut e: syn!(Expr) >>
        semi: punct!(;) >>
        ({
            attrs.extend(e.replace_attrs(Vec::new()));
            e.replace_attrs(attrs);
            Stmt::Semi(e, semi)
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
            syn!(PatMacro) => { Pat::Macro } // must be before pat_ident
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
            by_ref: option!(keyword!(ref)) >>
            mutability: option!(keyword!(mut)) >>
            name: alt!(
                syn!(Ident)
                |
                keyword!(self) => { Into::into }
            ) >>
            not!(punct!(<)) >>
            not!(punct!(::)) >>
            subpat: option!(tuple!(punct!(@), syn!(Pat))) >>
            (PatIdent {
                by_ref: by_ref,
                mutability: mutability,
                ident: name,
                subpat: subpat.map(|(at, pat)| (at, Box::new(pat))),
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
                fields: call!(Punctuated::parse_terminated) >>
                base: option!(cond!(fields.empty_or_trailing(), punct!(..))) >>
                (fields, base)
            )) >>
            (PatStruct {
                path: path,
                fields: (data.1).0,
                brace_token: data.0,
                dot2_token: (data.1).1.and_then(|m| m),
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
                    attrs: Vec::new(),
                    colon_token: Some(colon),
                })
            )
            |
            do_parse!(
                boxed: option!(keyword!(box)) >>
                by_ref: option!(keyword!(ref)) >>
                mutability: option!(keyword!(mut)) >>
                ident: syn!(Ident) >>
                ({
                    let mut pat: Pat = PatIdent {
                        by_ref: by_ref,
                        mutability: mutability,
                        ident: ident.clone(),
                        subpat: None,
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
                        attrs: Vec::new(),
                        colon_token: None,
                    }
                })
            )
        ));
    }

    impl Synom for Member {
        named!(parse -> Self, alt!(
            syn!(Ident) => { Member::Named }
            |
            syn!(Index) => { Member::Unnamed }
        ));
    }

    impl Synom for Index {
        named!(parse -> Self, do_parse!(
            lit: syn!(LitInt) >>
            ({
                if let IntSuffix::None = lit.suffix() {
                    Index { index: lit.value() as u32, span: lit.span() }
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
                front: call!(Punctuated::parse_terminated) >>
                dotdot: option!(cond_reduce!(front.empty_or_trailing(),
                    tuple!(punct!(..), option!(punct!(,)))
                )) >>
                back: cond!(match dotdot {
                                Some((_, Some(_))) => true,
                                _ => false,
                            },
                            Punctuated::parse_terminated) >>
                (front, dotdot, back)
            )) >>
            ({
                let (parens, (front, dotdot, back)) = data;
                let (dotdot, trailing) = match dotdot {
                    Some((a, b)) => (Some(a), Some(b)),
                    None => (None, None),
                };
                PatTuple {
                    paren_token: parens,
                    front: front,
                    dot2_token: dotdot,
                    comma_token: trailing.unwrap_or_default(),
                    back: back.unwrap_or_default(),
                }
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatRef {
        named!(parse -> Self, do_parse!(
            and: punct!(&) >>
            mutability: option!(keyword!(mut)) >>
            pat: syn!(Pat) >>
            (PatRef {
                pat: Box::new(pat),
                mutability: mutability,
                and_token: and,
            })
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatLit {
        named!(parse -> Self, do_parse!(
            lit: shim!(pat_lit_expr) >>
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
            lo: shim!(pat_lit_expr) >>
            limits: syn!(RangeLimits) >>
            hi: shim!(pat_lit_expr) >>
            (PatRange {
                lo: Box::new(lo),
                hi: Box::new(hi),
                limits: limits,
            })
        ));
    }

    #[cfg(feature = "full")]
    named2!(pat_lit_expr -> Expr, do_parse!(
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
                expr: Box::new(v)
            })
        } else {
            v
        })
    ));

    #[cfg(feature = "full")]
    impl Synom for PatSlice {
        named!(parse -> Self, map!(
            brackets!(do_parse!(
                before: call!(Punctuated::parse_terminated) >>
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
                    Punctuated::parse_terminated
                ) >>
                (before, middle, after)
            )),
            |(brackets, (before, middle, after))| {
                let mut before: Punctuated<Pat, Token![,]> = before;
                let after: Option<Punctuated<Pat, Token![,]>> = after;
                let middle: Option<(Token![..], Option<Token![,]>)> = middle;
                PatSlice {
                    dot2_token: middle.as_ref().map(|m| Token![..](m.0.spans)),
                    comma_token: middle.as_ref().and_then(|m| {
                        m.1.as_ref().map(|m| Token![,](m.spans))
                    }),
                    bracket_token: brackets,
                    middle: middle.and_then(|_| {
                        if before.empty_or_trailing() {
                            None
                        } else {
                            Some(Box::new(before.pop().unwrap().into_value()))
                        }
                    }),
                    front: before,
                    back: after.unwrap_or_default(),
                }
            }
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for PatMacro {
        named!(parse -> Self, map!(syn!(Macro), |mac| PatMacro { mac: mac }));
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    #[cfg(feature = "full")]
    use attr::FilterAttrs;
    use proc_macro2::{Literal, TokenStream};
    use quote::{ToTokens, TokenStreamExt};

    // If the given expression is a bare `ExprStruct`, wraps it in parenthesis
    // before appending it to `TokenStream`.
    #[cfg(feature = "full")]
    fn wrap_bare_struct(tokens: &mut TokenStream, e: &Expr) {
        if let Expr::Struct(_) = *e {
            token::Paren::default().surround(tokens, |tokens| {
                e.to_tokens(tokens);
            });
        } else {
            e.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    fn outer_attrs_to_tokens(attrs: &[Attribute], tokens: &mut TokenStream) {
        tokens.append_all(attrs.outer());
    }

    #[cfg(feature = "full")]
    fn inner_attrs_to_tokens(attrs: &[Attribute], tokens: &mut TokenStream) {
        tokens.append_all(attrs.inner());
    }

    #[cfg(not(feature = "full"))]
    fn outer_attrs_to_tokens(_attrs: &[Attribute], _tokens: &mut TokenStream) {}

    #[cfg(not(feature = "full"))]
    fn inner_attrs_to_tokens(_attrs: &[Attribute], _tokens: &mut TokenStream) {}

    #[cfg(feature = "full")]
    impl ToTokens for ExprBox {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.box_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprInPlace {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.place.to_tokens(tokens);
            self.arrow_token.to_tokens(tokens);
            self.value.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprArray {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.bracket_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                self.elems.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprCall {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.func.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprMethodCall {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.receiver.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.method.to_tokens(tokens);
            self.turbofish.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for MethodTurbofish {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.colon2_token.to_tokens(tokens);
            self.lt_token.to_tokens(tokens);
            self.args.to_tokens(tokens);
            self.gt_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for GenericMethodArgument {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match *self {
                GenericMethodArgument::Type(ref t) => t.to_tokens(tokens),
                GenericMethodArgument::Const(ref c) => c.to_tokens(tokens),
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprTuple {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.paren_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                self.elems.to_tokens(tokens);
                // If we only have one argument, we need a trailing comma to
                // distinguish ExprTuple from ExprParen.
                if self.elems.len() == 1 && !self.elems.trailing_punct() {
                    <Token![,]>::default().to_tokens(tokens);
                }
            })
        }
    }

    impl ToTokens for ExprBinary {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprUnary {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.op.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprLit {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.lit.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprCast {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.expr.to_tokens(tokens);
            self.as_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprType {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.expr.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    fn maybe_wrap_else(tokens: &mut TokenStream, else_: &Option<(Token![else], Box<Expr>)>) {
        if let Some((ref else_token, ref else_)) = *else_ {
            else_token.to_tokens(tokens);

            // If we are not one of the valid expressions to exist in an else
            // clause, wrap ourselves in a block.
            match **else_ {
                Expr::If(_) | Expr::IfLet(_) | Expr::Block(_) => {
                    else_.to_tokens(tokens);
                }
                _ => {
                    token::Brace::default().surround(tokens, |tokens| {
                        else_.to_tokens(tokens);
                    });
                }
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprIf {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.if_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.cond);
            self.then_branch.to_tokens(tokens);
            maybe_wrap_else(tokens, &self.else_branch);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprIfLet {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.if_token.to_tokens(tokens);
            self.let_token.to_tokens(tokens);
            self.pats.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.then_branch.to_tokens(tokens);
            maybe_wrap_else(tokens, &self.else_branch);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprWhile {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.while_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.cond);
            self.body.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.body.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprWhileLet {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.while_token.to_tokens(tokens);
            self.let_token.to_tokens(tokens);
            self.pats.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.body.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.body.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprForLoop {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.for_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.in_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.body.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.body.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprLoop {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.loop_token.to_tokens(tokens);
            self.body.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.body.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprMatch {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.match_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
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
    impl ToTokens for ExprAsync {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.async_token.to_tokens(tokens);
            self.capture.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprTryBlock {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.try_token.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprYield {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.yield_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprClosure {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.asyncness.to_tokens(tokens);
            self.movability.to_tokens(tokens);
            self.capture.to_tokens(tokens);
            self.or1_token.to_tokens(tokens);
            for input in self.inputs.pairs() {
                match **input.value() {
                    FnArg::Captured(ArgCaptured {
                        ref pat,
                        ty: Type::Infer(_),
                        ..
                    }) => {
                        pat.to_tokens(tokens);
                    }
                    _ => input.value().to_tokens(tokens),
                }
                input.punct().to_tokens(tokens);
            }
            self.or2_token.to_tokens(tokens);
            self.output.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprUnsafe {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.unsafe_token.to_tokens(tokens);
            self.block.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.block.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprBlock {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.block.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.block.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprAssign {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.left.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprAssignOp {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprField {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.base.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.member.to_tokens(tokens);
        }
    }

    impl ToTokens for Member {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match *self {
                Member::Named(ref ident) => ident.to_tokens(tokens),
                Member::Unnamed(ref index) => index.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for Index {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let mut lit = Literal::i64_unsuffixed(i64::from(self.index));
            lit.set_span(self.span);
            tokens.append(lit);
        }
    }

    impl ToTokens for ExprIndex {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.expr.to_tokens(tokens);
            self.bracket_token.surround(tokens, |tokens| {
                self.index.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprRange {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.from.to_tokens(tokens);
            match self.limits {
                RangeLimits::HalfOpen(ref t) => t.to_tokens(tokens),
                RangeLimits::Closed(ref t) => t.to_tokens(tokens),
            }
            self.to.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprPath {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            ::PathTokens(&self.qself, &self.path).to_tokens(tokens)
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprReference {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.and_token.to_tokens(tokens);
            self.mutability.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprBreak {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.break_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprContinue {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.continue_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprReturn {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.return_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprMacro {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.mac.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprStruct {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.path.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
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
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.bracket_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                self.expr.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
                self.len.to_tokens(tokens);
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprGroup {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.group_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ExprParen {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.paren_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                self.expr.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprTry {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.expr.to_tokens(tokens);
            self.question_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprVerbatim {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.tts.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Label {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.name.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for FieldValue {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.member.to_tokens(tokens);
            if let Some(ref colon_token) = self.colon_token {
                colon_token.to_tokens(tokens);
                self.expr.to_tokens(tokens);
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Arm {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.leading_vert.to_tokens(tokens);
            self.pats.to_tokens(tokens);
            if let Some((ref if_token, ref guard)) = self.guard {
                if_token.to_tokens(tokens);
                guard.to_tokens(tokens);
            }
            self.fat_arrow_token.to_tokens(tokens);
            self.body.to_tokens(tokens);
            self.comma.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatWild {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.underscore_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatIdent {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.by_ref.to_tokens(tokens);
            self.mutability.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            if let Some((ref at_token, ref subpat)) = self.subpat {
                at_token.to_tokens(tokens);
                subpat.to_tokens(tokens);
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatStruct {
        fn to_tokens(&self, tokens: &mut TokenStream) {
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
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.path.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatPath {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            ::PathTokens(&self.qself, &self.path).to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatTuple {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.front.to_tokens(tokens);
                if let Some(ref dot2_token) = self.dot2_token {
                    if !self.front.empty_or_trailing() {
                        // Ensure there is a comma before the .. token.
                        <Token![,]>::default().to_tokens(tokens);
                    }
                    dot2_token.to_tokens(tokens);
                    self.comma_token.to_tokens(tokens);
                    if self.comma_token.is_none() && !self.back.is_empty() {
                        // Ensure there is a comma after the .. token.
                        <Token![,]>::default().to_tokens(tokens);
                    }
                }
                self.back.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatBox {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.box_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatRef {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.and_token.to_tokens(tokens);
            self.mutability.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatLit {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatRange {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.lo.to_tokens(tokens);
            match self.limits {
                RangeLimits::HalfOpen(ref t) => t.to_tokens(tokens),
                RangeLimits::Closed(ref t) => Token![...](t.spans).to_tokens(tokens),
            }
            self.hi.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatSlice {
        fn to_tokens(&self, tokens: &mut TokenStream) {
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
    impl ToTokens for PatMacro {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.mac.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for PatVerbatim {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.tts.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for FieldPat {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            if let Some(ref colon_token) = self.colon_token {
                self.member.to_tokens(tokens);
                colon_token.to_tokens(tokens);
            }
            self.pat.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Block {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Stmt {
        fn to_tokens(&self, tokens: &mut TokenStream) {
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
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.let_token.to_tokens(tokens);
            self.pats.to_tokens(tokens);
            if let Some((ref colon_token, ref ty)) = self.ty {
                colon_token.to_tokens(tokens);
                ty.to_tokens(tokens);
            }
            if let Some((ref eq_token, ref init)) = self.init {
                eq_token.to_tokens(tokens);
                init.to_tokens(tokens);
            }
            self.semi_token.to_tokens(tokens);
        }
    }
}
