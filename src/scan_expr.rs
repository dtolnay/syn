use crate::expr::Member;
use crate::lifetime::Lifetime;
use crate::lit::Lit;
use crate::lit::LitFloat;
use crate::op::{BinOp, UnOp};
use crate::parse::{ParseStream, Result};
use crate::path::{self, AngleBracketedGenericArguments};
use crate::token;
use crate::ty::Type;
use proc_macro2::Delimiter::{self, Brace, Bracket, Parenthesis};

pub(crate) fn scan_expr(input: ParseStream) -> Result<()> {
    let consume = |delimiter: Delimiter| {
        Result::unwrap(input.step(|cursor| match cursor.group(delimiter) {
            Some((_inside, _span, rest)) => Ok((true, rest)),
            None => Ok((false, *cursor)),
        }))
    };

    macro_rules! consume {
        [$token:tt] => {
            input.parse::<Option<Token![$token]>>().unwrap().is_some()
        };
    }

    let mut initial = true;
    let mut depth = 0usize;
    loop {
        if initial {
            if consume![&] {
                initial = consume![mut] || !consume![raw] || consume![const] || consume![mut];
            } else if consume![if] || consume![match] || consume![while] {
                depth += 1;
            } else if input.parse::<Option<Lit>>()?.is_some()
                || (consume(Brace) || consume(Bracket) || consume(Parenthesis))
                || (consume![async] || consume![const] || consume![loop] || consume![unsafe])
                    && (consume(Brace) || break)
            {
                initial = false;
            } else if consume![let] {
                while !consume![=] {
                    if !((consume![|] || consume![ref] || consume![mut] || consume![@])
                        || (consume![!] || input.parse::<Option<Lit>>()?.is_some())
                        || (consume![..=] || consume![..] || consume![&] || consume![_])
                        || (consume(Brace) || consume(Bracket) || consume(Parenthesis)))
                    {
                        path::parsing::qpath(input, true)?;
                    }
                }
            } else if input.parse::<Option<Lifetime>>()?.is_some() && !consume![:] {
                break;
            } else if input.parse::<UnOp>().is_err() {
                path::parsing::qpath(input, true)?;
                initial = consume![!] || depth == 0 && input.peek(token::Brace);
            }
        } else if input.is_empty() || input.peek(Token![,]) {
            return Ok(());
        } else if depth > 0 && consume(Brace) {
            if consume![else] && !consume(Brace) {
                initial = consume![if] || break;
            } else {
                depth -= 1;
            }
        } else if input.parse::<BinOp>().is_ok() || (consume![..] | consume![=]) {
            initial = true;
        } else if consume![.] {
            if input.parse::<Option<LitFloat>>()?.is_none()
                && (input.parse::<Member>()?.is_named() && consume![::])
            {
                AngleBracketedGenericArguments::do_parse(None, input)?;
            }
        } else if consume![as] {
            input.parse::<Type>()?;
        } else if !(consume(Brace) || consume(Bracket) || consume(Parenthesis)) {
            break;
        }
    }

    Err(input.error("unsupported expression"))
}
