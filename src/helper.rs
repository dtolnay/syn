use nom::{self, IResult};

macro_rules! punct {
    ($i:expr, $punct:expr) => {
        tuple!($i, opt!(call!(::nom::multispace)), tag_s!($punct))
    };
}

macro_rules! opt_vec (
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        match $submac!($i, $($args)*) {
            ::nom::IResult::Done(i, o) => ::nom::IResult::Done(i, o),
            ::nom::IResult::Error(_) => ::nom::IResult::Done($i, Vec::new()),
            ::nom::IResult::Incomplete(i) => ::nom::IResult::Incomplete(i)
        }
    });
);

macro_rules! epsilon {
    ($i:expr,) => {
        call!($i, {
            fn epsilon<T>(input: T) -> ::nom::IResult<T, ()> {
                ::nom::IResult::Done(input, ())
            }
            epsilon
        })
    };
}

pub fn escaped_string(input: &str) -> IResult<&str, String> {
    let mut s = String::new();
    let mut chars = input.char_indices().peekable();
    while let Some((byte_offset, ch)) = chars.next() {
        match ch {
            '"' => {
                return IResult::Done(&input[byte_offset..], s);
            }
            '\\' => {
                match chars.next() {
                    Some((_, 'x')) => unimplemented!(),
                    Some((_, 'u')) => unimplemented!(),
                    Some((_, 'n')) => s.push('\n'),
                    Some((_, 'r')) => s.push('\r'),
                    Some((_, 't')) => s.push('\t'),
                    Some((_, '0')) => s.push('\0'),
                    Some((_, '\\')) => s.push('\\'),
                    Some((_, '\n')) => {
                        while let Some(&(_, ch)) = chars.peek() {
                            if ch.is_whitespace() {
                                chars.next();
                            } else {
                                break;
                            }
                        }
                    }
                    _ => break,
                }
            }
            ch => {
                s.push(ch);
            }
        }
    }
    IResult::Error(nom::Err::Position(nom::ErrorKind::Escaped, input))
}
