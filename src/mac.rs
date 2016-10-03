use super::*;

/// Represents a macro invocation. The Path indicates which macro
/// is being invoked, and the vector of token-trees contains the source
/// of the macro invocation.
///
/// NB: the additional ident for a `macro_rules`-style macro is actually
/// stored in the enclosing item. Oog.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Mac {
    pub path: Path,
    pub tts: Vec<TokenTree>,
}

/// When the main rust parser encounters a syntax-extension invocation, it
/// parses the arguments to the invocation as a token-tree. This is a very
/// loose structure, such that all sorts of different AST-fragments can
/// be passed to syntax extensions using a uniform type.
///
/// If the syntax extension is an MBE macro, it will attempt to match its
/// LHS token tree against the provided token tree, and if it finds a
/// match, will transcribe the RHS token tree, splicing in any captured
/// `macro_parser::matched_nonterminals` into the `SubstNt`s it finds.
///
/// The RHS of an MBE macro is the only place `SubstNt`s are substituted.
/// Nothing special happens to misnamed or misplaced `SubstNt`s.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenTree {
    /// A single token
    Token(Token),
    /// A delimited sequence of token trees
    Delimited(Delimited),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Delimited {
    /// The type of delimiter
    pub delim: DelimToken,
    /// The delimited sequence of token trees
    pub tts: Vec<TokenTree>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    /* Expression-operator symbols. */
    Eq,
    Lt,
    Le,
    EqEq,
    Ne,
    Ge,
    Gt,
    AndAnd,
    OrOr,
    Not,
    Tilde,
    BinOp(BinOpToken),
    BinOpEq(BinOpToken),

    /* Structural symbols */
    At,
    Dot,
    DotDot,
    DotDotDot,
    Comma,
    Semi,
    Colon,
    ModSep,
    RArrow,
    LArrow,
    FatArrow,
    Pound,
    Dollar,
    Question,

    /* Literals */
    Literal(Lit),

    /* Name components */
    Ident(Ident),
    Underscore,
    Lifetime(Ident),

    DocComment(String),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinOpToken {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    And,
    Or,
    Shl,
    Shr,
}

/// A delimiter token
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DelimToken {
    /// A round parenthesis: `(` or `)`
    Paren,
    /// A square bracket: `[` or `]`
    Bracket,
    /// A curly brace: `{` or `}`
    Brace,
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use Lifetime;
    use generics::parsing::lifetime;
    use ident::parsing::ident;
    use lit::parsing::lit;
    use space::{block_comment, whitespace};

    named!(pub mac -> Mac, do_parse!(
        name: ident >>
        punct!("!") >>
        body: delimited >>
        (Mac {
            path: name.into(),
            tts: vec![TokenTree::Delimited(body)],
        })
    ));

    named!(pub delimited -> Delimited, alt!(
        delimited!(
            punct!("("),
            many0!(token_tree),
            punct!(")")
        ) => { |tts| Delimited { delim: DelimToken::Paren, tts: tts } }
        |
        delimited!(
            punct!("["),
            many0!(token_tree),
            punct!("]")
        ) => { |tts| Delimited { delim: DelimToken::Bracket, tts: tts } }
        |
        delimited!(
            punct!("{"),
            many0!(token_tree),
            punct!("}")
        ) => { |tts| Delimited { delim: DelimToken::Brace, tts: tts } }
    ));

    named!(token_tree -> TokenTree, alt!(
        map!(token, TokenTree::Token)
        |
        map!(delimited, TokenTree::Delimited)
    ));

    named!(token -> Token, alt!(
        map!(bin_op_eq, Token::BinOpEq)
        |
        map!(bin_op, Token::BinOp)
        |
        punct!("=") => { |_| Token::Eq }
        |
        punct!("<") => { |_| Token::Lt }
        |
        punct!("<=") => { |_| Token::Le }
        |
        punct!("==") => { |_| Token::EqEq }
        |
        punct!("!=") => { |_| Token::Ne }
        |
        punct!(">=") => { |_| Token::Ge }
        |
        punct!(">") => { |_| Token::Gt }
        |
        punct!("&&") => { |_| Token::AndAnd }
        |
        punct!("||") => { |_| Token::OrOr }
        |
        punct!("!") => { |_| Token::Not }
        |
        punct!("~") => { |_| Token::Tilde }
        |
        punct!("@") => { |_| Token::At }
        |
        punct!(".") => { |_| Token::Dot }
        |
        punct!("..") => { |_| Token::DotDot }
        |
        punct!("...") => { |_| Token::DotDotDot }
        |
        punct!(",") => { |_| Token::Comma }
        |
        punct!(";") => { |_| Token::Semi }
        |
        punct!(":") => { |_| Token::Colon }
        |
        punct!("::") => { |_| Token::ModSep }
        |
        punct!("->") => { |_| Token::RArrow }
        |
        punct!("<-") => { |_| Token::LArrow }
        |
        punct!("=>") => { |_| Token::FatArrow }
        |
        punct!("#") => { |_| Token::Pound }
        |
        punct!("$") => { |_| Token::Dollar }
        |
        punct!("?") => { |_| Token::Question }
        |
        punct!("_") => { |_| Token::Underscore }
        |
        map!(lit, Token::Literal)
        |
        map!(ident, Token::Ident)
        |
        map!(lifetime, |lt: Lifetime| Token::Lifetime(lt.ident))
        |
        map!(doc_comment, Token::DocComment)
    ));

    named!(bin_op -> BinOpToken, alt!(
        punct!("+") => { |_| BinOpToken::Plus }
        |
        punct!("-") => { |_| BinOpToken::Minus }
        |
        punct!("*") => { |_| BinOpToken::Star }
        |
        punct!("/") => { |_| BinOpToken::Slash }
        |
        punct!("%") => { |_| BinOpToken::Percent }
        |
        punct!("^") => { |_| BinOpToken::Caret }
        |
        punct!("&") => { |_| BinOpToken::And }
        |
        punct!("|") => { |_| BinOpToken::Or }
        |
        punct!("<<") => { |_| BinOpToken::Shl }
        |
        punct!(">>") => { |_| BinOpToken::Shr }
    ));

    named!(bin_op_eq -> BinOpToken, alt!(
        punct!("+=") => { |_| BinOpToken::Plus }
        |
        punct!("-=") => { |_| BinOpToken::Minus }
        |
        punct!("*=") => { |_| BinOpToken::Star }
        |
        punct!("/=") => { |_| BinOpToken::Slash }
        |
        punct!("%=") => { |_| BinOpToken::Percent }
        |
        punct!("^=") => { |_| BinOpToken::Caret }
        |
        punct!("&=") => { |_| BinOpToken::And }
        |
        punct!("|=") => { |_| BinOpToken::Or }
        |
        punct!("<<=") => { |_| BinOpToken::Shl }
        |
        punct!(">>=") => { |_| BinOpToken::Shr }
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
            not!(peek!(tag!("/"))) >>
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
