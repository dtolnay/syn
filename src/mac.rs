#[cfg(feature = "extra-traits")]
use std::fmt;

use super::*;

use proc_macro2::{TokenKind, Delimiter};

ast_struct! {
    /// Represents a macro invocation. The Path indicates which macro
    /// is being invoked, and the vector of token-trees contains the source
    /// of the macro invocation.
    ///
    /// NB: the additional ident for a `macro_rules`-style macro is actually
    /// stored in the enclosing item. Oog.
    pub struct Mac {
        pub path: Path,
        pub bang_token: tokens::Bang,
        pub tokens: Vec<TokenTree>,
    }
}

#[cfg_attr(feature = "clone-impls", derive(Clone))]
pub struct TokenTree(pub proc_macro2::TokenTree);

impl Mac {
    pub fn is_braced(&self) -> bool {
        match self.tokens.last() {
            Some(t) => t.is_braced(),
            None => false,
        }
    }
}

impl TokenTree {
    pub fn is_braced(&self) -> bool {
        match self.0.kind {
            TokenKind::Sequence(Delimiter::Brace, _) => true,
            _ => false,
        }
    }
}

#[cfg(feature = "extra-traits")]
impl PartialEq for TokenTree {
    fn eq(&self, other: &TokenTree) -> bool {
        use proc_macro2::OpKind;

        match (&self.0.kind, &other.0.kind) {
            (&TokenKind::Sequence(d1, ref s1), &TokenKind::Sequence(d2, ref s2)) => {
                match (d1, d2) {
                    (Delimiter::Parenthesis, Delimiter::Parenthesis) |
                    (Delimiter::Brace, Delimiter::Brace) |
                    (Delimiter::Bracket, Delimiter::Bracket) => {}
                    (Delimiter::None, Delimiter::None) => {}
                    _ => return false,
                }

                let s1 = s1.clone().into_iter();
                let mut s2 = s2.clone().into_iter();

                for item1 in s1 {
                    let item2 = match s2.next() {
                        Some(item) => item,
                        None => return false,
                    };
                    if TokenTree(item1) != TokenTree(item2) {
                        return false
                    }
                }
                s2.next().is_none()
            }
            (&TokenKind::Op(o1, k1), &TokenKind::Op(o2, k2)) => {
                o1 == o2 && match (k1, k2) {
                    (OpKind::Alone, OpKind::Alone) |
                    (OpKind::Joint, OpKind::Joint) => true,
                    _ => false,
                }
            }
            (&TokenKind::Literal(ref l1), &TokenKind::Literal(ref l2)) => {
                l1.to_string() == l2.to_string()
            }
            (&TokenKind::Word(ref s1), &TokenKind::Word(ref s2)) => {
                s1.as_str() == s2.as_str()
            }
            _ => false,
        }
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for TokenTree {}

#[cfg(feature = "extra-traits")]
impl ::std::hash::Hash for TokenTree {
    fn hash<H: ::std::hash::Hasher>(&self, h: &mut H) {
        use proc_macro2::OpKind;

        match self.0.kind {
            TokenKind::Sequence(delim, ref stream) => {
                0u8.hash(h);
                match delim {
                    Delimiter::Parenthesis => 0u8.hash(h),
                    Delimiter::Brace => 1u8.hash(h),
                    Delimiter::Bracket => 2u8.hash(h),
                    Delimiter::None => 3u8.hash(h),
                }

                for item in stream.clone().into_iter() {
                    TokenTree(item).hash(h);
                }
                0xffu8.hash(h); // terminator w/ a variant we don't normally hash
            }
            TokenKind::Op(op, kind) => {
                1u8.hash(h);
                op.hash(h);
                match kind {
                    OpKind::Alone => 0u8.hash(h),
                    OpKind::Joint => 1u8.hash(h),
                }
            }
            TokenKind::Literal(ref lit) => (2u8, lit.to_string()).hash(h),
            TokenKind::Word(ref word) => (3u8, word.as_str()).hash(h),
        }
    }
}

#[cfg(feature = "extra-traits")]
impl fmt::Debug for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.to_string().fmt(f)
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use {Lifetime};
    use generics::parsing::lifetime;
    use ident::parsing::word;
    use lit::parsing::lit;
    use synom::IResult;
    use synom::space::{block_comment, whitespace, skip_whitespace};
    use ty::parsing::path;
    use proc_macro2::{self, TokenStream, TokenKind, Delimiter, OpKind, Literal};

    fn tt(kind: TokenKind) -> TokenTree {
        TokenTree(proc_macro2::TokenTree {
            kind: kind,
            span: Default::default(),
        })
    }

    named!(pub mac -> Mac, do_parse!(
        what: path >>
        punct!("!") >>
        body: delimited >>
        (Mac {
            path: what,
            bang_token: tokens::Bang::default(),
            tokens: vec![body],
        })
    ));

    named!(pub token_trees -> Vec<TokenTree>, many0!(token_tree));

    named!(pub token_stream -> TokenStream,
           map!(token_trees, |t: Vec<TokenTree>| t.into_iter().map(|t| t.0).collect()));

    named!(pub delimited -> TokenTree, alt!(
        delimited!(
            punct!("("),
            token_stream,
            punct!(")")
        ) => { |ts| tt(TokenKind::Sequence(Delimiter::Parenthesis, ts)) }
        |
        delimited!(
            punct!("["),
            token_stream,
            punct!("]")
        ) => { |ts| tt(TokenKind::Sequence(Delimiter::Bracket, ts)) }
        |
        delimited!(
            punct!("{"),
            token_stream,
            punct!("}")
        ) => { |ts| tt(TokenKind::Sequence(Delimiter::Brace, ts)) }
    ));

    named!(pub token_tree -> TokenTree, alt!(
        token
        |
        delimited
    ));

    macro_rules! punct1 {
        ($i:expr, $punct:expr) => {
            punct1($i, $punct)
        }
    }

    fn punct1<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, char> {
        let input = skip_whitespace(input);
        if input.starts_with(token) {
            IResult::Done(&input[1..], token.chars().next().unwrap())
        } else {
            IResult::Error
        }
    }

    named!(token -> TokenTree, alt!(
        keyword!("_") => { |_| tt(TokenKind::Op('_', OpKind::Alone)) }
        |
        punct1!("&&") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) } // must be before BinOp
        |
        punct1!("||") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) } // must be before BinOp
        |
        punct1!("->") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) } // must be before BinOp
        |
        punct1!("<-") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) } // must be before Lt
        |
        punct1!("=>") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) } // must be before Eq
        |
        punct1!("...") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) } // must be before DotDot
        |
        punct1!("..") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) } // must be before Dot
        |
        punct1!(".") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        // must be before bin_op
        map!(doc_comment, |s: String| tt(TokenKind::Literal(Literal::doccomment(&s))))
        |
        bin_op_eq // must be before bin_op
        |
        bin_op
        |
        map!(lit, |l: Lit| l.into_token_tree())
        |
        map!(word, |w: Ident| tt(TokenKind::Word(w.sym)))
        |
        map!(lifetime, |lt: Lifetime| tt(TokenKind::Word(lt.ident.sym)))
        |
        punct1!("<=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("==") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("!=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!(">=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("::") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("=") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("<") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!(">") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("!") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("~") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("@") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!(",") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!(";") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!(":") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("#") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("$") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("?") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
    ));

    named!(bin_op -> TokenTree, alt!(
        punct1!("+") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("-") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("*") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("/") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("%") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("^") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("&") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("|") => { |c| tt(TokenKind::Op(c, OpKind::Alone)) }
        |
        punct1!("<<") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!(">>") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
    ));

    named!(bin_op_eq -> TokenTree, alt!(
        punct1!("+=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("-=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("*=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("/=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("%=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("^=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("&=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("|=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!("<<=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
        |
        punct1!(">>=") => { |c| tt(TokenKind::Op(c, OpKind::Joint)) }
    ));

    named!(doc_comment -> String, alt!(
        do_parse!(
            punct!("//!") >>
            content: take_until!("\n") >>
            (format!("//!{}", content))
        )
        |
        do_parse!(
            option!(whitespace) >>
            peek!(tag!("/*!")) >>
            com: block_comment >>
            (com.to_owned())
        )
        |
        do_parse!(
            punct!("///") >>
            not!(tag!("/")) >>
            content: take_until!("\n") >>
            (format!("///{}", content))
        )
        |
        do_parse!(
            option!(whitespace) >>
            peek!(tuple!(tag!("/**"), not!(tag!("*")))) >>
            com: block_comment >>
            (com.to_owned())
        )
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Mac {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.bang_token.to_tokens(tokens);
            tokens.append_all(&self.tokens);
        }
    }

    impl ToTokens for TokenTree {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.0.to_tokens(tokens);
        }
    }
}
