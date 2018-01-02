use std::iter::FromIterator;
use std::slice;
use std::vec;
#[cfg(feature = "extra-traits")]
use std::fmt::{self, Debug};

#[cfg_attr(feature = "extra-traits", derive(Eq, PartialEq, Hash))]
#[cfg_attr(feature = "clone-impls", derive(Clone))]
pub struct Punctuated<T, P> {
    inner: Vec<(T, Option<P>)>,
}

impl<T, P> Punctuated<T, P> {
    pub fn new() -> Punctuated<T, P> {
        Punctuated { inner: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn first(&self) -> Option<Element<&T, &P>> {
        self.inner.first().map(|&(ref t, ref d)| match *d {
            Some(ref d) => Element::Punctuated(t, d),
            None => Element::End(t),
        })
    }

    pub fn last(&self) -> Option<Element<&T, &P>> {
        self.inner.last().map(|&(ref t, ref d)| match *d {
            Some(ref d) => Element::Punctuated(t, d),
            None => Element::End(t),
        })
    }

    pub fn last_mut(&mut self) -> Option<Element<&mut T, &mut P>> {
        self.inner
            .last_mut()
            .map(|&mut (ref mut t, ref mut d)| match *d {
                Some(ref mut d) => Element::Punctuated(t, d),
                None => Element::End(t),
            })
    }

    pub fn iter(&self) -> Iter<T, P> {
        Iter {
            inner: self.inner.iter(),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<T, P> {
        IterMut {
            inner: self.inner.iter_mut(),
        }
    }

    pub fn elements(&self) -> Elements<T, P> {
        Elements {
            inner: self.inner.iter(),
        }
    }

    pub fn elements_mut(&mut self) -> ElementsMut<T, P> {
        ElementsMut {
            inner: self.inner.iter_mut(),
        }
    }

    pub fn into_elements(self) -> IntoElements<T, P> {
        IntoElements {
            inner: self.inner.into_iter(),
        }
    }

    pub fn push_item(&mut self, item: T) {
        assert!(self.empty_or_trailing());
        self.inner.push((item, None));
    }

    pub fn push_punct(&mut self, punctuation: P) {
        assert!(!self.is_empty());
        let last = self.inner.last_mut().unwrap();
        assert!(last.1.is_none());
        last.1 = Some(punctuation);
    }

    pub fn pop(&mut self) -> Option<Element<T, P>> {
        self.inner.pop().map(|(t, d)| Element::new(t, d))
    }

    pub fn trailing_punct(&self) -> bool {
        self.inner.last().map(|last| last.1.is_some()).unwrap_or(false)
    }

    /// Returns true if either this `Punctuated` is empty, or it has a trailing
    /// punctuation.
    ///
    /// Equivalent to `punctuated.is_empty() || punctuated.trailing_punct()`.
    pub fn empty_or_trailing(&self) -> bool {
        self.inner.last().map(|last| last.1.is_some()).unwrap_or(true)
    }
}

impl<T, P> Punctuated<T, P>
where
    P: Default,
{
    pub fn push(&mut self, item: T) {
        if !self.empty_or_trailing() {
            self.push_punct(Default::default());
        }
        self.push_item(item);
    }
}

#[cfg(feature = "extra-traits")]
impl<T: Debug, P: Debug> Debug for Punctuated<T, P> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T, P> FromIterator<Element<T, P>> for Punctuated<T, P> {
    fn from_iter<I: IntoIterator<Item = Element<T, P>>>(i: I) -> Self {
        let mut ret = Punctuated::new();
        ret.extend(i);
        ret
    }
}

impl<T, P> Extend<Element<T, P>> for Punctuated<T, P> {
    fn extend<I: IntoIterator<Item = Element<T, P>>>(&mut self, i: I) {
        for elem in i {
            match elem {
                Element::Punctuated(a, b) => self.inner.push((a, Some(b))),
                Element::End(a) => self.inner.push((a, None)),
            }
        }
    }
}

impl<T, P> IntoIterator for Punctuated<T, P> {
    type Item = T;
    type IntoIter = IntoIter<T, P>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.inner.into_iter(),
        }
    }
}

impl<'a, T, P> IntoIterator for &'a Punctuated<T, P> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T, P>;

    fn into_iter(self) -> Self::IntoIter {
        Punctuated::iter(self)
    }
}

impl<'a, T, P> IntoIterator for &'a mut Punctuated<T, P> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T, P>;

    fn into_iter(self) -> Self::IntoIter {
        Punctuated::iter_mut(self)
    }
}

impl<T, P> Default for Punctuated<T, P> {
    fn default() -> Self {
        Punctuated::new()
    }
}

pub struct Elements<'a, T: 'a, P: 'a> {
    inner: slice::Iter<'a, (T, Option<P>)>,
}

impl<'a, T, P> Iterator for Elements<'a, T, P> {
    type Item = Element<&'a T, &'a P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|pair| match pair.1 {
            Some(ref p) => Element::Punctuated(&pair.0, p),
            None => Element::End(&pair.0),
        })
    }
}

pub struct ElementsMut<'a, T: 'a, P: 'a> {
    inner: slice::IterMut<'a, (T, Option<P>)>,
}

impl<'a, T, P> Iterator for ElementsMut<'a, T, P> {
    type Item = Element<&'a mut T, &'a mut P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|pair| match pair.1 {
            Some(ref mut p) => Element::Punctuated(&mut pair.0, p),
            None => Element::End(&mut pair.0),
        })
    }
}

pub struct IntoElements<T, P> {
    inner: vec::IntoIter<(T, Option<P>)>,
}

impl<T, P> Iterator for IntoElements<T, P> {
    type Item = Element<T, P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|pair| match pair.1 {
            Some(p) => Element::Punctuated(pair.0, p),
            None => Element::End(pair.0),
        })
    }
}

pub struct IntoIter<T, P> {
    inner: vec::IntoIter<(T, Option<P>)>,
}

impl<T, P> Iterator for IntoIter<T, P> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|pair| pair.0)
    }
}

pub struct Iter<'a, T: 'a, P: 'a> {
    inner: slice::Iter<'a, (T, Option<P>)>,
}

impl<'a, T, P> Iterator for Iter<'a, T, P> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|pair| &pair.0)
    }
}

pub struct IterMut<'a, T: 'a, P: 'a> {
    inner: slice::IterMut<'a, (T, Option<P>)>,
}

impl<'a, T, P> Iterator for IterMut<'a, T, P> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|pair| &mut pair.0)
    }
}

pub enum Element<T, P> {
    Punctuated(T, P),
    End(T),
}

impl<T, P> Element<T, P> {
    pub fn into_item(self) -> T {
        match self {
            Element::Punctuated(t, _) | Element::End(t) => t,
        }
    }

    pub fn item(&self) -> &T {
        match *self {
            Element::Punctuated(ref t, _) | Element::End(ref t) => t,
        }
    }

    pub fn item_mut(&mut self) -> &mut T {
        match *self {
            Element::Punctuated(ref mut t, _) | Element::End(ref mut t) => t,
        }
    }

    pub fn punct(&self) -> Option<&P> {
        match *self {
            Element::Punctuated(_, ref d) => Some(d),
            Element::End(_) => None,
        }
    }

    pub fn new(t: T, d: Option<P>) -> Self {
        match d {
            Some(d) => Element::Punctuated(t, d),
            None => Element::End(t),
        }
    }

    pub fn into_tuple(self) -> (T, Option<P>) {
        match self {
            Element::Punctuated(t, d) => (t, Some(d)),
            Element::End(t) => (t, None),
        }
    }
}

#[cfg(feature = "parsing")]
mod parsing {
    use super::Punctuated;
    use synom::Synom;
    use cursor::Cursor;
    use parse_error;
    use synom::PResult;

    impl<T, P> Punctuated<T, P>
    where
        T: Synom,
        P: Synom,
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

    impl<T, P> Punctuated<T, P>
    where
        P: Synom,
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
                Ok((ref b, _)) if b.is_empty() => parse_error(),
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
                Ok((ref b, _)) if b.is_empty() => parse_error(),
                other => other,
            }
        }

        fn parse(
            mut input: Cursor,
            parse: fn(Cursor) -> PResult<T>,
            terminated: bool,
        ) -> PResult<Self> {
            let mut res = Punctuated::new();

            // get the first element
            match parse(input) {
                Err(_) => Ok((res, input)),
                Ok((o, i)) => {
                    if i == input {
                        return parse_error();
                    }
                    input = i;
                    res.push_item(o);

                    // get the separator first
                    while let Ok((s, i2)) = P::parse(input) {
                        if i2 == input {
                            break;
                        }

                        // get the element next
                        if let Ok((o3, i3)) = parse(i2) {
                            if i3 == i2 {
                                break;
                            }
                            res.push_punct(s);
                            res.push_item(o3);
                            input = i3;
                        } else {
                            break;
                        }
                    }
                    if terminated {
                        if let Ok((sep, after)) = P::parse(input) {
                            res.push_punct(sep);
                            input = after;
                        }
                    }
                    Ok((res, input))
                }
            }
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{ToTokens, Tokens};

    impl<T, P> ToTokens for Punctuated<T, P>
    where
        T: ToTokens,
        P: ToTokens,
    {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.elements())
        }
    }

    impl<T, P> ToTokens for Element<T, P>
    where
        T: ToTokens,
        P: ToTokens,
    {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Element::Punctuated(ref a, ref b) => {
                    a.to_tokens(tokens);
                    b.to_tokens(tokens);
                }
                Element::End(ref a) => a.to_tokens(tokens),
            }
        }
    }
}
