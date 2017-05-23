use Span;

macro_rules! tokens {
    (
        ops: {
            $(($($op:tt)*),)*
        }
        delim: {
            $(($($delim:tt)*),)*
        }
        syms: {
            $(($($sym:tt)*),)*
        }
    ) => (
        $(op! { $($op)* })*
        $(delim! { $($delim)* })*
        $(sym! { $($sym)* })*
    )
}

macro_rules! op {
    (pub struct $name:ident($($contents:tt)*) => $s:expr) => {
        ast_struct! {
            #[cfg_attr(feature = "clone-impls", derive(Copy))]
            #[derive(Default)]
            pub struct $name(pub $($contents)*);
        }

        #[cfg(feature = "printing")]
        impl ::quote::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut ::quote::Tokens) {
                printing::op($s, &self.0, tokens);
            }
        }
    }
}

macro_rules! sym {
    (pub struct $name:ident => $s:expr) => {
        ast_struct! {
            #[cfg_attr(feature = "clone-impls", derive(Copy))]
            #[derive(Default)]
            pub struct $name(pub Span);
        }

        #[cfg(feature = "printing")]
        impl ::quote::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut ::quote::Tokens) {
                printing::sym($s, &self.0, tokens);
            }
        }
    }
}

macro_rules! delim {
    (pub struct $name:ident => $s:expr) => {
        ast_struct! {
            #[cfg_attr(feature = "clone-impls", derive(Copy))]
            #[derive(Default)]
            pub struct $name(pub Span);
        }

        #[cfg(feature = "printing")]
        impl $name {
            pub fn surround<F>(&self,
                               tokens: &mut ::quote::Tokens,
                               f: F)
                where F: FnOnce(&mut ::quote::Tokens)
            {
                printing::delim($s, &self.0, tokens, f);
            }
        }
    }
}

tokens! {
    ops: {
        (pub struct Add([Span; 1])          => "+"),
        (pub struct AddEq([Span; 2])        => "+="),
        (pub struct And([Span; 1])          => "&"),
        (pub struct AndAnd([Span; 2])       => "&&"),
        (pub struct AndEq([Span; 2])        => "&="),
        (pub struct At([Span; 1])           => "@"),
        (pub struct Bang([Span; 1])         => "!"),
        (pub struct Caret([Span; 1])        => "^"),
        (pub struct CaretEq([Span; 2])      => "^="),
        (pub struct Colon([Span; 1])        => ":"),
        (pub struct Colon2([Span; 2])       => "::"),
        (pub struct Comma([Span; 1])        => ","),
        (pub struct Div([Span; 1])          => "/"),
        (pub struct DivEq([Span; 2])        => "/="),
        (pub struct Dot([Span; 1])          => "."),
        (pub struct Dot2([Span; 2])         => ".."),
        (pub struct Dot3([Span; 3])         => "..."),
        (pub struct Eq([Span; 1])           => "="),
        (pub struct EqEq([Span; 2])         => "=="),
        (pub struct Ge([Span; 2])           => ">="),
        (pub struct Gt([Span; 1])           => ">"),
        (pub struct Le([Span; 2])           => "<="),
        (pub struct Lt([Span; 1])           => "<"),
        (pub struct MulEq([Span; 2])        => "*="),
        (pub struct Ne([Span; 2])           => "!="),
        (pub struct Or([Span; 1])           => "|"),
        (pub struct OrEq([Span; 2])         => "|="),
        (pub struct OrOr([Span; 2])         => "||"),
        (pub struct Pound([Span; 1])        => "#"),
        (pub struct Question([Span; 1])     => "?"),
        (pub struct RArrow([Span; 2])       => "->"),
        (pub struct Rem([Span; 1])          => "%"),
        (pub struct RemEq([Span; 2])        => "%="),
        (pub struct Rocket([Span; 2])       => "=>"),
        (pub struct Semi([Span; 1])         => ";"),
        (pub struct Shl([Span; 2])          => "<<"),
        (pub struct ShlEq([Span; 3])        => "<<="),
        (pub struct Shr([Span; 2])          => ">>"),
        (pub struct ShrEq([Span; 3])        => ">>="),
        (pub struct Star([Span; 1])         => "*"),
        (pub struct Sub([Span; 1])          => "-"),
        (pub struct SubEq([Span; 2])        => "-="),
        (pub struct Underscore([Span; 1])   => "_"),
    }
    delim: {
        (pub struct Brace                   => "{"),
        (pub struct Bracket                 => "["),
        (pub struct Paren                   => "("),
    }
    syms: {
        (pub struct As                      => "as"),
        (pub struct Box                     => "box"),
        (pub struct Break                   => "break"),
        (pub struct Catch                   => "catch"),
        (pub struct Const                   => "const"),
        (pub struct Continue                => "continue"),
        (pub struct Crate                   => "crate"),
        (pub struct Default                 => "default"),
        (pub struct Do                      => "do"),
        (pub struct Else                    => "else"),
        (pub struct Enum                    => "enum"),
        (pub struct Extern                  => "extern"),
        (pub struct Fn                      => "fn"),
        (pub struct For                     => "for"),
        (pub struct If                      => "if"),
        (pub struct Impl                    => "impl"),
        (pub struct In                      => "in"),
        (pub struct Let                     => "let"),
        (pub struct Loop                    => "loop"),
        (pub struct Match                   => "match"),
        (pub struct Mod                     => "mod"),
        (pub struct Move                    => "move"),
        (pub struct Mut                     => "mut"),
        (pub struct Pub                     => "pub"),
        (pub struct Ref                     => "ref"),
        (pub struct Return                  => "return"),
        (pub struct Self_                   => "self"),
        (pub struct Static                  => "static"),
        (pub struct Struct                  => "struct"),
        (pub struct Trait                   => "trait"),
        (pub struct Type                    => "type"),
        (pub struct Union                   => "union"),
        (pub struct Unsafe                  => "unsafe"),
        (pub struct Use                     => "use"),
        (pub struct Where                   => "where"),
        (pub struct While                   => "while"),
    }
}

#[cfg(feature = "printing")]
mod printing {
    use Span;
    use proc_macro2::{TokenTree, TokenKind, OpKind};
    use quote::Tokens;

    pub fn op(s: &str, spans: &[Span], tokens: &mut Tokens) {
        assert_eq!(s.len(), spans.len());

        let mut chars = s.chars();
        let mut spans = spans.iter();
        let ch = chars.next_back().unwrap();
        let span = spans.next_back().unwrap();
        for (ch, span) in chars.zip(spans) {
            tokens.append(TokenTree {
                span: span.0,
                kind: TokenKind::Op(ch, OpKind::Joint),
            });
        }

        tokens.append(TokenTree {
            span: span.0,
            kind: TokenKind::Op(ch, OpKind::Alone),
        });
    }

    pub fn sym(s: &str, span: &Span, tokens: &mut Tokens) {
        tokens.append(TokenTree {
            span: span.0,
            kind: TokenKind::Word(s.into()),
        });
    }

    pub fn delim<F>(s: &str, span: &Span, tokens: &mut Tokens, f: F)
        where F: FnOnce(&mut Tokens)
    {
        tokens.append_delimited(s, span.0, f)
    }
}
