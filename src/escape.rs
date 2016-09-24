#![cfg(feature = "parsing")]

use std::{char, str};
use nom::IResult;

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
                    Some((_, 'x')) => match backslash_x(&mut chars) {
                        Some(ch) => s.push(ch),
                        None => break,
                    },
                    Some((_, 'n')) => s.push('\n'),
                    Some((_, 'r')) => s.push('\r'),
                    Some((_, 't')) => s.push('\t'),
                    Some((_, '\\')) => s.push('\\'),
                    Some((_, '0')) => s.push('\0'),
                    Some((_, 'u')) => match backslash_u(&mut chars) {
                        Some(ch) => s.push(ch),
                        None => break,
                    },
                    Some((_, '\'')) => s.push('\''),
                    Some((_, '"')) => s.push('"'),
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
    IResult::Error
}

macro_rules! next_char {
    ($chars:ident @ $pat:pat $(| $rest:pat)*) => {
        match $chars.next() {
            Some((_, ch)) => match ch {
                $pat $(| $rest)*  => ch,
                _ => return None,
            },
            None => return None,
        }
    };
}

fn backslash_x<I>(chars: &mut I) -> Option<char> where I: Iterator<Item = (usize, char)> {
    let a = next_char!(chars @ '0'...'7');
    let b = next_char!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F');
    char_from_hex_bytes(&[a as u8, b as u8])
}

#[cfg_attr(feature = "clippy", allow(many_single_char_names))]
fn backslash_u<I>(chars: &mut I) -> Option<char> where I: Iterator<Item = (usize, char)> {
    next_char!(chars @ '{');
    let a = next_char!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F');
    let b = next_char!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if b == '}' {
        return char_from_hex_bytes(&[a as u8]);
    }
    let c = next_char!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if c == '}' {
        return char_from_hex_bytes(&[a as u8, b as u8]);
    }
    let d = next_char!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if d == '}' {
        return char_from_hex_bytes(&[a as u8, b as u8, c as u8]);
    }
    let e = next_char!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if e == '}' {
        return char_from_hex_bytes(&[a as u8, b as u8, c as u8, d as u8]);
    }
    let f = next_char!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if f == '}' {
        return char_from_hex_bytes(&[a as u8, b as u8, c as u8, d as u8, e as u8]);
    }
    next_char!(chars @ '}');
    char_from_hex_bytes(&[a as u8, b as u8, c as u8, d as u8, e as u8, f as u8])
}

/// Assumes the bytes are all '0'...'9' | 'a'...'f' | 'A'...'F'.
fn char_from_hex_bytes(hex_bytes: &[u8]) -> Option<char> {
    let hex_str = unsafe { str::from_utf8_unchecked(hex_bytes) };
    char::from_u32(u32::from_str_radix(hex_str, 16).unwrap())
}

#[test]
fn test_escaped_string() {
    let input = r#"\x62 \u{7} \u{64} \u{bf5} \u{12ba} \u{1F395} \u{102345}""#;
    let expected = "\x62 \u{7} \u{64} \u{bf5} \u{12ba} \u{1F395} \u{102345}";
    assert_eq!(escaped_string(input), IResult::Done("\"", expected.to_string()));
}
