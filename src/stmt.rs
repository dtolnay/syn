use super::*;

ast_struct! {
    /// A braced block containing Rust statements.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
    pub struct Block {
        pub brace_token: token::Brace,
        /// Statements in a block
        pub stmts: Vec<Stmt>,
    }
}

ast_enum! {
    /// A statement, usually ending in a semicolon.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
    pub enum Stmt {
        /// A local (let) binding.
        Local(Local),

        /// An item definition.
        Item(Item),

        /// Expression, with or without trailing semicolon.
        Expr(Expr, Option<Token![;]>),
    }
}

ast_struct! {
    /// A local `let` binding: `let x: u64 = s.parse()?`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
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
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    #[cfg_attr(doc_cfg, doc(cfg(feature = "full")))]
    pub struct LocalInit {
        pub eq_token: Token![=],
        pub expr: Box<Expr>,
        pub diverge: Option<(Token![else], Box<Expr>)>,
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
        /// *This function is available only if Syn is built with the `"parsing"`
        /// feature.*
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
                let s = parse_stmt(input, AllowNoSemi(true))?;
                let requires_semicolon = if let Stmt::Expr(s, None) = &s {
                    expr::requires_terminator(s)
                } else {
                    false
                };
                stmts.push(s);
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
        if let Ok(path) = ahead.call(Path::parse_mod_style) {
            if ahead.peek(Token![!])
                && (ahead.peek2(token::Brace)
                    && !(ahead.peek3(Token![.]) || ahead.peek3(Token![?]))
                    || ahead.peek2(Ident))
            {
                input.advance_to(&ahead);
                return stmt_mac(input, attrs, path);
            }
        }

        if input.peek(Token![let]) {
            stmt_local(input, attrs).map(Stmt::Local)
        } else if input.peek(Token![pub])
            || input.peek(Token![crate]) && !input.peek2(Token![::])
            || input.peek(Token![extern])
            || input.peek(Token![use])
            || input.peek(Token![static])
                && (input.peek2(Token![mut])
                    || input.peek2(Ident)
                        && !(input.peek2(Token![async])
                            && (input.peek3(Token![move]) || input.peek3(Token![|]))))
            || input.peek(Token![const]) && !input.peek2(token::Brace)
            || input.peek(Token![unsafe]) && !input.peek2(token::Brace)
            || input.peek(Token![async])
                && (input.peek2(Token![unsafe])
                    || input.peek2(Token![extern])
                    || input.peek2(Token![fn]))
            || input.peek(Token![fn])
            || input.peek(Token![mod])
            || input.peek(Token![type])
            || input.peek(Token![struct])
            || input.peek(Token![enum])
            || input.peek(Token![union]) && input.peek2(Ident)
            || input.peek(Token![auto]) && input.peek2(Token![trait])
            || input.peek(Token![trait])
            || input.peek(Token![default])
                && (input.peek2(Token![unsafe]) || input.peek2(Token![impl]))
            || input.peek(Token![impl])
            || input.peek(Token![macro])
        {
            let mut item: Item = input.parse()?;
            attrs.extend(item.replace_attrs(Vec::new()));
            item.replace_attrs(attrs);
            Ok(Stmt::Item(item))
        } else {
            stmt_expr(input, allow_nosemi, attrs)
        }
    }

    fn stmt_mac(input: ParseStream, attrs: Vec<Attribute>, path: Path) -> Result<Stmt> {
        let bang_token: Token![!] = input.parse()?;
        let ident: Option<Ident> = input.parse()?;
        let (delimiter, tokens) = mac::parse_delimiter(input)?;
        let semi_token: Option<Token![;]> = input.parse()?;

        Ok(Stmt::Item(Item::Macro(ItemMacro {
            attrs,
            ident,
            mac: Macro {
                path,
                bang_token,
                delimiter,
                tokens,
            },
            semi_token,
        })))
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
                _ => break,
            };
        }
        attrs.extend(attr_target.replace_attrs(Vec::new()));
        attr_target.replace_attrs(attrs);

        if let semi @ Some(_) = input.parse()? {
            return Ok(Stmt::Expr(e, semi));
        }

        if allow_nosemi.0 || !expr::requires_terminator(&e) {
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
}
