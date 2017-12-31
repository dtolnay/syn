use std::iter::FromIterator;
use std::slice;
use std::vec;
#[cfg(feature = "extra-traits")]
use std::fmt::{self, Debug};

#[cfg_attr(feature = "extra-traits", derive(Eq, PartialEq, Hash))]
#[cfg_attr(feature = "clone-impls", derive(Clone))]
pub struct Delimited<T, D> {
    inner: Vec<(T, Option<D>)>,
}

impl<T, D> Delimited<T, D> {
    pub fn new() -> Delimited<T, D> {
        Delimited { inner: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn first(&self) -> Option<Element<&T, &D>> {
        self.inner.first().map(|&(ref t, ref d)| match *d {
            Some(ref d) => Element::Delimited(t, d),
            None => Element::End(t),
        })
    }

    pub fn last(&self) -> Option<Element<&T, &D>> {
        self.inner.last().map(|&(ref t, ref d)| match *d {
            Some(ref d) => Element::Delimited(t, d),
            None => Element::End(t),
        })
    }

    pub fn last_mut(&mut self) -> Option<Element<&mut T, &mut D>> {
        self.inner
            .last_mut()
            .map(|&mut (ref mut t, ref mut d)| match *d {
                Some(ref mut d) => Element::Delimited(t, d),
                None => Element::End(t),
            })
    }

    pub fn iter(&self) -> Iter<T, D> {
        Iter {
            inner: self.inner.iter(),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<T, D> {
        IterMut {
            inner: self.inner.iter_mut(),
        }
    }

    pub fn push(&mut self, token: T) {
        assert!(self.empty_or_trailing());
        self.inner.push((token, None));
    }

    pub fn push_trailing(&mut self, delimiter: D) {
        assert!(!self.is_empty());
        let last = self.inner.last_mut().unwrap();
        assert!(last.1.is_none());
        last.1 = Some(delimiter);
    }

    pub fn pop(&mut self) -> Option<Element<T, D>> {
        self.inner.pop().map(|(t, d)| Element::new(t, d))
    }

    pub fn trailing_delim(&self) -> bool {
        self.inner.last().map(|last| last.1.is_some()).unwrap_or(false)
    }

    /// Returns true if either this `Delimited` is empty, or it has a trailing
    /// delimiter.
    ///
    /// Equivalent to `delimited.is_empty() || delimited.trailing_delim()`.
    pub fn empty_or_trailing(&self) -> bool {
        self.inner.last().map(|last| last.1.is_some()).unwrap_or(true)
    }
}

#[cfg(feature = "extra-traits")]
impl<T: Debug, D: Debug> Debug for Delimited<T, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T, D> FromIterator<Element<T, D>> for Delimited<T, D> {
    fn from_iter<I: IntoIterator<Item = Element<T, D>>>(i: I) -> Self {
        let mut ret = Delimited::new();
        ret.extend(i);
        ret
    }
}

impl<T, D> Extend<Element<T, D>> for Delimited<T, D> {
    fn extend<I: IntoIterator<Item = Element<T, D>>>(&mut self, i: I) {
        for elem in i {
            match elem {
                Element::Delimited(a, b) => self.inner.push((a, Some(b))),
                Element::End(a) => self.inner.push((a, None)),
            }
        }
    }
}

impl<'a, T, D> IntoIterator for &'a Delimited<T, D> {
    type Item = Element<&'a T, &'a D>;
    type IntoIter = Iter<'a, T, D>;

    fn into_iter(self) -> Self::IntoIter {
        Delimited::iter(self)
    }
}

impl<'a, T, D> IntoIterator for &'a mut Delimited<T, D> {
    type Item = Element<&'a mut T, &'a mut D>;
    type IntoIter = IterMut<'a, T, D>;

    fn into_iter(self) -> Self::IntoIter {
        Delimited::iter_mut(self)
    }
}

impl<T, D> IntoIterator for Delimited<T, D> {
    type Item = Element<T, D>;
    type IntoIter = IntoIter<T, D>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.inner.into_iter(),
        }
    }
}

impl<T, D> Default for Delimited<T, D> {
    fn default() -> Self {
        Delimited::new()
    }
}

pub struct Iter<'a, T: 'a, D: 'a> {
    inner: slice::Iter<'a, (T, Option<D>)>,
}

impl<'a, T, D> Iterator for Iter<'a, T, D> {
    type Item = Element<&'a T, &'a D>;

    fn next(&mut self) -> Option<Element<&'a T, &'a D>> {
        self.inner.next().map(|pair| match pair.1 {
            Some(ref delimited) => Element::Delimited(&pair.0, delimited),
            None => Element::End(&pair.0),
        })
    }
}

pub struct IterMut<'a, T: 'a, D: 'a> {
    inner: slice::IterMut<'a, (T, Option<D>)>,
}

impl<'a, T, D> Iterator for IterMut<'a, T, D> {
    type Item = Element<&'a mut T, &'a mut D>;

    fn next(&mut self) -> Option<Element<&'a mut T, &'a mut D>> {
        self.inner.next().map(|pair| match pair.1 {
            Some(ref mut delimited) => Element::Delimited(&mut pair.0, delimited),
            None => Element::End(&mut pair.0),
        })
    }
}

pub struct IntoIter<T, D> {
    inner: vec::IntoIter<(T, Option<D>)>,
}

impl<T, D> Iterator for IntoIter<T, D> {
    type Item = Element<T, D>;

    fn next(&mut self) -> Option<Element<T, D>> {
        self.inner.next().map(|pair| match pair.1 {
            Some(v) => Element::Delimited(pair.0, v),
            None => Element::End(pair.0),
        })
    }
}

pub enum Element<T, D> {
    Delimited(T, D),
    End(T),
}

impl<T, D> Element<T, D> {
    pub fn into_item(self) -> T {
        match self {
            Element::Delimited(t, _) | Element::End(t) => t,
        }
    }

    pub fn item(&self) -> &T {
        match *self {
            Element::Delimited(ref t, _) | Element::End(ref t) => t,
        }
    }

    pub fn item_mut(&mut self) -> &mut T {
        match *self {
            Element::Delimited(ref mut t, _) | Element::End(ref mut t) => t,
        }
    }

    pub fn delimiter(&self) -> Option<&D> {
        match *self {
            Element::Delimited(_, ref d) => Some(d),
            Element::End(_) => None,
        }
    }

    pub fn new(t: T, d: Option<D>) -> Self {
        match d {
            Some(d) => Element::Delimited(t, d),
            None => Element::End(t),
        }
    }

    pub fn into_tuple(self) -> (T, Option<D>) {
        match self {
            Element::Delimited(t, d) => (t, Some(d)),
            Element::End(t) => (t, None),
        }
    }
}

#[cfg(feature = "parsing")]
mod parsing {
    use super::Delimited;
    use synom::Synom;
    use cursor::Cursor;
    use parse_error;
    use synom::PResult;

    impl<T, D> Delimited<T, D>
    where
        T: Synom,
        D: Synom,
    {
        pub fn parse_separated(input: Cursor) -> PResult<Self> {
            Self::parse(input, T::parse, false)
        }

        pub fn parse_separated_nonempty(input: Cursor) -> PResult<Self> {
            Self::parse_separated_nonempty_with(input, T::parse)
        }

        pub fn parse_terminated(input: Cursor) -> PResult<Self> {
            Self::parse_terminated_with(input, T::parse)
        }

        pub fn parse_terminated_nonempty(input: Cursor) -> PResult<Self> {
            Self::parse_terminated_nonempty_with(input, T::parse)
        }
    }

    impl<T, D> Delimited<T, D>
    where
        D: Synom,
    {
        pub fn parse_separated_with(
            input: Cursor,
            parse: fn(Cursor) -> PResult<T>,
        ) -> PResult<Self> {
            Self::parse(input, parse, false)
        }

        pub fn parse_separated_nonempty_with(
            input: Cursor,
            parse: fn(Cursor) -> PResult<T>,
        ) -> PResult<Self> {
            match Self::parse(input, parse, false) {
                Ok((_, ref b)) if b.is_empty() => parse_error(),
                other => other,
            }
        }

        pub fn parse_terminated_with(
            input: Cursor,
            parse: fn(Cursor) -> PResult<T>,
        ) -> PResult<Self> {
            Self::parse(input, parse, true)
        }

        pub fn parse_terminated_nonempty_with(
            input: Cursor,
            parse: fn(Cursor) -> PResult<T>,
        ) -> PResult<Self> {
            match Self::parse(input, parse, true) {
                Ok((_, ref b)) if b.is_empty() => parse_error(),
                other => other,
            }
        }

        fn parse(
            mut input: Cursor,
            parse: fn(Cursor) -> PResult<T>,
            terminated: bool,
        ) -> PResult<Self> {
            let mut res = Delimited::new();

            // get the first element
            match parse(input) {
                Err(_) => Ok((input, res)),
                Ok((i, o)) => {
                    if i == input {
                        return parse_error();
                    }
                    input = i;
                    res.push(o);

                    // get the separator first
                    while let Ok((i2, s)) = D::parse(input) {
                        if i2 == input {
                            break;
                        }

                        // get the element next
                        if let Ok((i3, o3)) = parse(i2) {
                            if i3 == i2 {
                                break;
                            }
                            res.push_trailing(s);
                            res.push(o3);
                            input = i3;
                        } else {
                            break;
                        }
                    }
                    if terminated {
                        if let Ok((after, sep)) = D::parse(input) {
                            res.push_trailing(sep);
                            input = after;
                        }
                    }
                    Ok((input, res))
                }
            }
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{ToTokens, Tokens};

    impl<T, D> ToTokens for Delimited<T, D>
    where
        T: ToTokens,
        D: ToTokens,
    {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.iter())
        }
    }

    impl<T, D> ToTokens for Element<T, D>
    where
        T: ToTokens,
        D: ToTokens,
    {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Element::Delimited(ref a, ref b) => {
                    a.to_tokens(tokens);
                    b.to_tokens(tokens);
                }
                Element::End(ref a) => a.to_tokens(tokens),
            }
        }
    }
}
