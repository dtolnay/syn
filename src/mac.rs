use super::*;

/// Represents a macro invocation. The Path indicates which macro
/// is being invoked, and the vector of token-trees contains the source
/// of the macro invocation.
///
/// NB: the additional ident for a macro_rules-style macro is actually
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
/// macro_parser::matched_nonterminals into the `SubstNt`s it finds.
///
/// The RHS of an MBE macro is the only place `SubstNt`s are substituted.
/// Nothing special happens to misnamed or misplaced `SubstNt`s.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenTree {
    /// A single token
    Token(Token),
    /// A delimited sequence of token trees
    Delimited(Delimited),

    // This only makes sense in MBE macros.
    /// A kleene-style repetition sequence with a span
    Sequence(SequenceRepetition),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Delimited {
    /// The type of delimiter
    pub delim: DelimToken,
    /// The delimited sequence of token trees
    pub tts: Vec<TokenTree>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SequenceRepetition {
    /// The sequence of token trees
    pub tts: Vec<TokenTree>,
    /// The optional separator
    pub separator: Option<Token>,
    /// Whether the sequence can be repeated zero (*), or one or more times (+)
    pub op: KleeneOp,
    /// The number of `MatchNt`s that appear in the sequence (and subsequences)
    pub num_captures: usize,
}

/// A Kleene-style [repetition operator](http://en.wikipedia.org/wiki/Kleene_star)
/// for token sequences.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum KleeneOp {
    ZeroOrMore,
    OneOrMore,
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
    /// An opening delimiter, eg. `{`
    OpenDelim(DelimToken),
    /// A closing delimiter, eg. `}`
    CloseDelim(DelimToken),

    /* Literals */
    Literal(Lit, Option<String>),

    /* Name components */
    Ident(Ident),
    Underscore,
    Lifetime(Ident),

    // Can be expanded into several tokens.
    /// Doc comment
    DocComment(String),
    // In left-hand-sides of MBE macros:
    /// Parse a nonterminal (name to bind, name of NT)
    MatchNt(Ident, Ident),
    // In right-hand-sides of MBE macros:
    /// A syntactic variable that will be filled in by macro expansion.
    SubstNt(Ident),
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
    /// An empty delimiter
    NoDelim,
}
