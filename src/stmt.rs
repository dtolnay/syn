use super::*;

ast_struct! {
    /// A braced block containing Rust statements.
    #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
    pub struct Block {
        pub brace_token: token::Brace,
        /// Statements in a block
        pub stmts: Vec<Stmt>,
    }
}

ast_enum! {
    /// A statement, usually ending in a semicolon.
    #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
    pub enum Stmt {
        /// A local (let) binding.
        Local(Local),

        /// An item definition.
        Item(Item),

        /// Expression, with or without trailing semicolon.
        Expr(Expr, Option<Token![;]>),

        /// A macro invocation in statement position.
        ///
        /// Syntactically it's ambiguous which other kind of statement this
        /// macro would expand to. It can be an of local variable (`let`), item,
        /// or expression.
        Macro(StmtMacro),
    }
}

ast_struct! {
    /// A local `let` binding: `let x: u64 = s.parse()?`.
    #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
    pub struct Local {
        pub attrs: Vec<Attribute>,
        pub let_token: Token![let],
        pub pat: Pat,
        pub init: Option<LocalInit>,
        pub semi_token: Token![;],
    }
}

ast_struct! {
    /// The expression assigned in a local `let` binding, including optional
    /// diverging `else` block.
    ///
    /// `LocalInit` represents `= s.parse()?` in `let x: u64 = s.parse()?` and
    /// `= r else { return }` in `let Ok(x) = r else { return }`.
    #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
    pub struct LocalInit {
        pub eq_token: Token![=],
        pub expr: Box<Expr>,
        pub diverge: Option<(Token![else], Box<Expr>)>,
    }
}

ast_struct! {
    /// A macro invocation in statement position.
    ///
    /// Syntactically it's ambiguous which other kind of statement this macro
    /// would expand to. It can be an of local variable (`let`), item, or
    /// expression.
    #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
    pub struct StmtMacro {
        pub attrs: Vec<Attribute>,
        pub mac: Macro,
        pub semi_token: Option<Token![;]>,
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use crate::parse::discouraged::Speculative;
    use crate::parse::{Parse, ParseStream, Result};
    use proc_macro2::TokenStream;

    struct AllowNoSemi(bool);

    impl Block {
        /// Parse the body of a block as zero or more statements, possibly
        /// including one trailing expression.
        ///
        /// # Example
        ///
        /// ```
        /// use syn::{braced, token, Attribute, Block, Ident, Result, Stmt, Token};
        /// use syn::parse::{Parse, ParseStream};
        ///
        /// // Parse a function with no generics or parameter list.
        /// //
        /// //     fn playground {
        /// //         let mut x = 1;
        /// //         x += 1;
        /// //         println!("{}", x);
        /// //     }
        /// struct MiniFunction {
        ///     attrs: Vec<Attribute>,
        ///     fn_token: Token![fn],
        ///     name: Ident,
        ///     brace_token: token::Brace,
        ///     stmts: Vec<Stmt>,
        /// }
        ///
        /// impl Parse for MiniFunction {
        ///     fn parse(input: ParseStream) -> Result<Self> {
        ///         let outer_attrs = input.call(Attribute::parse_outer)?;
        ///         let fn_token: Token![fn] = input.parse()?;
        ///         let name: Ident = input.parse()?;
        ///
        ///         let content;
        ///         let brace_token = braced!(content in input);
        ///         let inner_attrs = content.call(Attribute::parse_inner)?;
        ///         let stmts = content.call(Block::parse_within)?;
        ///
        ///         Ok(MiniFunction {
        ///             attrs: {
        ///                 let mut attrs = outer_attrs;
        ///                 attrs.extend(inner_attrs);
        ///                 attrs
        ///             },
        ///             fn_token,
        ///             name,
        ///             brace_token,
        ///             stmts,
        ///         })
        ///     }
        /// }
        /// ```
        #[cfg_attr(doc_cfg, doc(cfg(feature = "parsing")))]
        pub fn parse_within(input: ParseStream) -> Result<Vec<Stmt>> {
            let mut stmts = Vec::new();
            loop {
                while let semi @ Some(_) = input.parse()? {
                    stmts.push(Stmt::Expr(Expr::Verbatim(TokenStream::new()), semi));
                }
                if input.is_empty() {
                    break;
                }
                let stmt = parse_stmt(input, AllowNoSemi(true))?;
                let requires_semicolon = match &stmt {
                    Stmt::Expr(stmt, None) => expr::requires_terminator(stmt),
                    Stmt::Macro(stmt) => {
                        stmt.semi_token.is_none() && !stmt.mac.delimiter.is_brace()
                    }
                    Stmt::Local(_) | Stmt::Item(_) | Stmt::Expr(_, Some(_)) => false,
                };
                stmts.push(stmt);
                if input.is_empty() {
                    break;
                } else if requires_semicolon {
                    return Err(input.error("unexpected token"));
                }
            }
            Ok(stmts)
        }
    }

    #[cfg_attr(doc_cfg, doc(cfg(feature = "parsing")))]
    impl Parse for Block {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(Block {
                brace_token: braced!(content in input),
                stmts: content.call(Block::parse_within)?,
            })
        }
    }

    #[cfg_attr(doc_cfg, doc(cfg(feature = "parsing")))]
    impl Parse for Stmt {
        fn parse(input: ParseStream) -> Result<Self> {
            let allow_nosemi = AllowNoSemi(false);
            parse_stmt(input, allow_nosemi)
        }
    }

    fn parse_stmt(input: ParseStream, allow_nosemi: AllowNoSemi) -> Result<Stmt> {
        let mut attrs = input.call(Attribute::parse_outer)?;

        // brace-style macros; paren and bracket macros get parsed as
        // expression statements.
        let ahead = input.fork();
        let mut is_item_macro = false;
        if let Ok(path) = ahead.call(Path::parse_mod_style) {
            if ahead.peek(Token![!]) {
                if ahead.peek2(Ident) {
                    is_item_macro = true;
                } else if ahead.peek2(token::Brace)
                    && !(ahead.peek3(Token![.]) || ahead.peek3(Token![?]))
                {
                    input.advance_to(&ahead);
                    return stmt_mac(input, attrs, path).map(Stmt::Macro);
                }
            }
        }

        if input.peek(Token![let]) {
            stmt_local(input, attrs).map(Stmt::Local)
        } else if input.peek(
            Token![pub]
                | Token![extern]
                | Token![use]
                | Token![fn]
                | Token![mod]
                | Token![type]
                | Token![struct]
                | Token![enum]
                | Token![trait]
                | Token![impl]
                | Token![macro],
        ) || input.peek(Token![crate]) && !input.peek2(Token![::])
            || input.peek(Token![static])
                && (input.peek2(Token![mut])
                    || input.peek2(Ident)
                        && !(input.peek2(Token![async]) && input.peek3(Token![move] | Token![|])))
            || input.peek(Token![const]) && !input.peek2(token::Brace)
            || input.peek(Token![unsafe]) && !input.peek2(token::Brace)
            || input.peek(Token![async])
                && input.peek2(Token![unsafe] | Token![extern] | Token![fn])
            || input.peek(Token![union]) && input.peek2(Ident)
            || input.peek(Token![auto]) && input.peek2(Token![trait])
            || input.peek(Token![default]) && input.peek2(Token![unsafe] | Token![impl])
            || is_item_macro
        {
            let mut item: Item = input.parse()?;
            attrs.extend(item.replace_attrs(Vec::new()));
            item.replace_attrs(attrs);
            Ok(Stmt::Item(item))
        } else {
            stmt_expr(input, allow_nosemi, attrs)
        }
    }

    fn stmt_mac(input: ParseStream, attrs: Vec<Attribute>, path: Path) -> Result<StmtMacro> {
        let bang_token: Token![!] = input.parse()?;
        let (delimiter, tokens) = mac::parse_delimiter(input)?;
        let semi_token: Option<Token![;]> = input.parse()?;

        Ok(StmtMacro {
            attrs,
            mac: Macro {
                path,
                bang_token,
                delimiter,
                tokens,
            },
            semi_token,
        })
    }

    fn stmt_local(input: ParseStream, attrs: Vec<Attribute>) -> Result<Local> {
        let let_token: Token![let] = input.parse()?;

        let mut pat = Pat::parse_single(input)?;
        if input.peek(Token![:]) {
            let colon_token: Token![:] = input.parse()?;
            let ty: Type = input.parse()?;
            pat = Pat::Type(PatType {
                attrs: Vec::new(),
                pat: Box::new(pat),
                colon_token,
                ty: Box::new(ty),
            });
        }

        let init = if let Some(eq_token) = input.parse()? {
            let eq_token: Token![=] = eq_token;
            let expr: Expr = input.parse()?;

            let diverge = if let Some(else_token) = input.parse()? {
                let else_token: Token![else] = else_token;
                let diverge = ExprBlock {
                    attrs: Vec::new(),
                    label: None,
                    block: input.parse()?,
                };
                Some((else_token, Box::new(Expr::Block(diverge))))
            } else {
                None
            };

            Some(LocalInit {
                eq_token,
                expr: Box::new(expr),
                diverge,
            })
        } else {
            None
        };

        let semi_token: Token![;] = input.parse()?;

        Ok(Local {
            attrs,
            let_token,
            pat,
            init,
            semi_token,
        })
    }

    fn stmt_expr(
        input: ParseStream,
        allow_nosemi: AllowNoSemi,
        mut attrs: Vec<Attribute>,
    ) -> Result<Stmt> {
        let mut e = expr::parsing::expr_early(input)?;

        let mut attr_target = &mut e;
        loop {
            attr_target = match attr_target {
                Expr::Assign(e) => &mut e.left,
                Expr::AssignOp(e) => &mut e.left,
                Expr::Binary(e) => &mut e.left,
                Expr::Cast(e) => &mut e.expr,
                Expr::Array(_)
                | Expr::Async(_)
                | Expr::Await(_)
                | Expr::Block(_)
                | Expr::Break(_)
                | Expr::Call(_)
                | Expr::Closure(_)
                | Expr::Const(_)
                | Expr::Continue(_)
                | Expr::Field(_)
                | Expr::ForLoop(_)
                | Expr::Group(_)
                | Expr::If(_)
                | Expr::Index(_)
                | Expr::Infer(_)
                | Expr::Let(_)
                | Expr::Lit(_)
                | Expr::Loop(_)
                | Expr::Macro(_)
                | Expr::Match(_)
                | Expr::MethodCall(_)
                | Expr::Paren(_)
                | Expr::Path(_)
                | Expr::Range(_)
                | Expr::Reference(_)
                | Expr::Repeat(_)
                | Expr::Return(_)
                | Expr::Struct(_)
                | Expr::Try(_)
                | Expr::TryBlock(_)
                | Expr::Tuple(_)
                | Expr::Unary(_)
                | Expr::Unsafe(_)
                | Expr::While(_)
                | Expr::Yield(_)
                | Expr::Verbatim(_) => break,
            };
        }
        attrs.extend(attr_target.replace_attrs(Vec::new()));
        attr_target.replace_attrs(attrs);

        let semi_token: Option<Token![;]> = input.parse()?;

        if allow_nosemi.0 || semi_token.is_some() {
            if let Expr::Macro(ExprMacro { attrs, mac }) = e {
                return Ok(Stmt::Macro(StmtMacro {
                    attrs,
                    mac,
                    semi_token,
                }));
            }
        }

        if semi_token.is_some() {
            Ok(Stmt::Expr(e, semi_token))
        } else if allow_nosemi.0 || !expr::requires_terminator(&e) {
            Ok(Stmt::Expr(e, None))
        } else {
            Err(input.error("expected semicolon"))
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt};

    #[cfg_attr(doc_cfg, doc(cfg(feature = "printing")))]
    impl ToTokens for Block {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.stmts);
            });
        }
    }

    #[cfg_attr(doc_cfg, doc(cfg(feature = "printing")))]
    impl ToTokens for Stmt {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Stmt::Local(local) => local.to_tokens(tokens),
                Stmt::Item(item) => item.to_tokens(tokens),
                Stmt::Expr(expr, semi) => {
                    expr.to_tokens(tokens);
                    semi.to_tokens(tokens);
                }
                Stmt::Macro(mac) => mac.to_tokens(tokens),
            }
        }
    }

    #[cfg_attr(doc_cfg, doc(cfg(feature = "printing")))]
    impl ToTokens for Local {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            expr::printing::outer_attrs_to_tokens(&self.attrs, tokens);
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            if let Some(init) = &self.init {
                init.eq_token.to_tokens(tokens);
                init.expr.to_tokens(tokens);
                if let Some((else_token, diverge)) = &init.diverge {
                    else_token.to_tokens(tokens);
                    diverge.to_tokens(tokens);
                }
            }
            self.semi_token.to_tokens(tokens);
        }
    }

    #[cfg_attr(doc_cfg, doc(cfg(feature = "printing")))]
    impl ToTokens for StmtMacro {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            expr::printing::outer_attrs_to_tokens(&self.attrs, tokens);
            self.mac.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }
}
