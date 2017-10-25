use std::iter::FromIterator;
use std::slice;
use std::vec;

#[cfg_attr(feature = "extra-traits", derive(Eq, PartialEq, Hash, Debug))]
#[cfg_attr(feature = "clone-impls", derive(Clone))]
pub struct Delimited<T, D> {
    inner: Vec<(T, Option<D>)>
}

impl<T, D> Delimited<T, D> {
    pub fn new() -> Delimited<T, D> {
        Delimited {
            inner: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn get(&self, idx: usize) -> Element<&T, &D> {
        let (ref t, ref d) = self.inner[idx];
        match *d {
            Some(ref d) => Element::Delimited(t, d),
            None => Element::End(t),
        }
    }

    pub fn get_mut(&mut self, idx: usize) -> Element<&mut T, &mut D> {
        let (ref mut t, ref mut d) = self.inner[idx];
        match *d {
            Some(ref mut d) => Element::Delimited(t, d),
            None => Element::End(t),
        }
    }

    pub fn first(&self) -> Option<Element<&T, &D>> {
        self.inner.first().map(|&(ref t, ref d)| {
            match *d {
                Some(ref d) => Element::Delimited(t, d),
                None => Element::End(t),
            }
        })
    }

    pub fn first_mut(&mut self) -> Option<Element<&mut T, &mut D>> {
        self.inner.first_mut().map(|&mut (ref mut t, ref mut d)| {
            match *d {
                Some(ref mut d) => Element::Delimited(t, d),
                None => Element::End(t),
            }
        })
    }

    pub fn last(&self) -> Option<Element<&T, &D>> {
        self.inner.last().map(|&(ref t, ref d)| {
            match *d {
                Some(ref d) => Element::Delimited(t, d),
                None => Element::End(t),
            }
        })
    }

    pub fn last_mut(&mut self) -> Option<Element<&mut T, &mut D>> {
        self.inner.last_mut().map(|&mut (ref mut t, ref mut d)| {
            match *d {
                Some(ref mut d) => Element::Delimited(t, d),
                None => Element::End(t),
            }
        })
    }

    pub fn iter(&self) -> Iter<T, D> {
        Iter { inner: self.inner.iter() }
    }

    pub fn iter_mut(&mut self) -> IterMut<T, D> {
        IterMut { inner: self.inner.iter_mut() }
    }

    pub fn items(&self) -> Items<T, D> {
        Items { inner: self.inner.iter() }
    }

    pub fn push(&mut self, token: Element<T, D>) {
        assert!(self.is_empty() || self.trailing_delim());
        match token {
            Element::Delimited(t, d) => self.inner.push((t, Some(d))),
            Element::End(t) => self.inner.push((t, None)),
        }
    }

    pub fn push_first(&mut self, token: T) {
        assert!(self.is_empty());
        self.inner.push((token, None));
    }

    pub fn push_next(&mut self, token: T, delimiter: D) {
        self.push_trailing(delimiter);
        self.inner.push((token, None));
    }

    pub fn push_trailing(&mut self, delimiter: D) {
        let len = self.len();
        assert!(self.inner[len - 1].1.is_none());
        self.inner[len - 1].1 = Some(delimiter);
    }

    pub fn push_default(&mut self, token: T) where D: Default {
        if self.is_empty() || self.trailing_delim() {
            self.inner.push((token, None));
        } else {
            self.push_next(token, D::default());
        }
    }

    pub fn pop(&mut self) -> Option<Element<T, D>> {
        self.inner.pop().map(|e| {
            match e {
                (t, Some(d)) => Element::Delimited(t, d),
                (t, None) => Element::End(t),
            }
        })
    }

    pub fn into_vec(self) -> Vec<T> {
        self.inner.into_iter().map(|t| t.0).collect()
    }

    pub fn trailing_delim(&self) -> bool {
        self.inner[self.inner.len() - 1].1.is_some()
    }

    /// Returns true if either this `Delimited` is empty, or it has a trailing
    /// delimiter. This is useful within `ToTokens` implementations for `syn`.
    #[doc(hidden)]
    pub fn empty_or_trailing(&self) -> bool {
        self.is_empty() || self.trailing_delim()
    }
}

impl<T, D> From<Vec<(T, Option<D>)>> for Delimited<T, D> {
    fn from(v: Vec<(T, Option<D>)>) -> Self {
        Delimited {
            inner: v,
        }
    }
}

impl<T, D> From<Vec<T>> for Delimited<T, D>
    where D: Default,
{
    fn from(v: Vec<T>) -> Self {
        let len = v.len();
        Delimited {
            inner: v.into_iter().enumerate().map(|(i, item)| {
                (item, if i + 1 == len {None} else {Some(D::default())})
            }).collect(),
        }
    }
}

impl<T, D> FromIterator<Element<T, D>> for Delimited<T, D> {
    fn from_iter<I: IntoIterator<Item = Element<T, D>>>(i: I) -> Self {
        let mut ret = Delimited::new();
        ret.extend(i);
        ret
    }
}

impl<T, D> FromIterator<T> for Delimited<T, D>
    where D: Default,
{
    fn from_iter<I: IntoIterator<Item = T>>(i: I) -> Self {
        let mut ret = Delimited::new();
        ret.extend(i);
        ret
    }
}

impl<T, D> Extend<Element<T, D>> for Delimited<T, D> {
    fn extend<I: IntoIterator<Item = Element<T, D>>>(&mut self, i: I) {
        for element in i {
            match element {
                Element::Delimited(a, b) => self.inner.push((a, Some(b))),
                Element::End(a) => self.inner.push((a, None)),
            }
        }
    }
}

impl<T, D> Extend<T> for Delimited<T, D>
    where D: Default,
{
    fn extend<I: IntoIterator<Item = T>>(&mut self, i: I) {
        for element in i {
            self.push_default(element);
        }
    }
}

impl<'a, T, D> IntoIterator for &'a Delimited<T, D> {
    type Item = Element<&'a T, &'a D>;
    type IntoIter = Iter<'a, T, D>;

    fn into_iter(self) -> Iter<'a, T, D> {
        <Delimited<T, D>>::iter(self)
    }
}

impl<T, D> IntoIterator for Delimited<T, D> {
    type Item = Element<T, D>;
    type IntoIter = IntoIter<T, D>;

    fn into_iter(self) -> IntoIter<T, D> {
        IntoIter { inner: self.inner.into_iter() }
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
        self.inner.next().map(|pair| {
            match pair.1 {
                Some(ref delimited) => Element::Delimited(&pair.0, delimited),
                None => Element::End(&pair.0),
            }
        })
    }
}

pub struct IterMut<'a, T: 'a, D: 'a> {
    inner: slice::IterMut<'a, (T, Option<D>)>,
}

impl<'a, T, D> Iterator for IterMut<'a, T, D> {
    type Item = Element<&'a mut T, &'a mut D>;

    fn next(&mut self) -> Option<Element<&'a mut T, &'a mut D>> {
        self.inner.next().map(|pair| {
            match pair.1 {
                Some(ref mut delimited) => Element::Delimited(&mut pair.0, delimited),
                None => Element::End(&mut pair.0),
            }
        })
    }
}

pub struct Items<'a, T: 'a, D: 'a> {
    inner: slice::Iter<'a, (T, Option<D>)>,
}

impl<'a, T, D> Iterator for Items<'a, T, D> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        self.inner.next().map(|pair| &pair.0)
    }
}

pub struct IntoIter<T, D> {
    inner: vec::IntoIter<(T, Option<D>)>,
}

impl<T, D> Iterator for IntoIter<T, D> {
    type Item = Element<T, D>;

    fn next(&mut self) -> Option<Element<T, D>> {
        self.inner.next().map(|pair| {
            match pair.1 {
                Some(v) => Element::Delimited(pair.0, v),
                None => Element::End(pair.0)
            }
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
            Element::Delimited(t, _) |
            Element::End(t) => t,
        }
    }

    pub fn item(&self) -> &T {
        match *self {
            Element::Delimited(ref t, _) |
            Element::End(ref t) => t,
        }
    }

    pub fn item_mut(&mut self) -> &mut T {
        match *self {
            Element::Delimited(ref mut t, _) |
            Element::End(ref mut t) => t,
        }
    }

    pub fn delimiter(&self) -> Option<&D> {
        match *self {
            Element::Delimited(_, ref d) => Some(d),
            Element::End(_) => None,
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
    use {PResult, Cursor, Synom, parse_error};

    impl<T, D> Delimited<T, D>
        where T: Synom,
              D: Synom,
    {
        pub fn parse_separated(input: Cursor) -> PResult<Self>
        {
            Self::parse(input, T::parse, false)
        }

        pub fn parse_separated_nonempty(input: Cursor) -> PResult<Self>
        {
            Self::parse_separated_nonempty_with(input, T::parse)
        }

        pub fn parse_terminated(input: Cursor) -> PResult<Self>
        {
            Self::parse_terminated_with(input, T::parse)
        }
    }

    impl<T, D> Delimited<T, D>
        where D: Synom,
    {
        pub fn parse_separated_nonempty_with(
                input: Cursor,
                parse: fn(Cursor) -> PResult<T>)
            -> PResult<Self>
        {
            match Self::parse(input, parse, false) {
                Ok((_, ref b)) if b.is_empty() => parse_error(),
                other => other,
            }
        }

        pub fn parse_terminated_with(
                input: Cursor,
                parse: fn(Cursor) -> PResult<T>)
            -> PResult<Self>
        {
            Self::parse(input, parse, true)
        }

        fn parse(mut input: Cursor,
                 parse: fn(Cursor) -> PResult<T>,
                 terminated: bool)
            -> PResult<Self>
        {
            let mut res = Delimited::new();

            // get the first element
            match parse(input) {
                Err(_) => Ok((input, res)),
                Ok((i, o)) => {
                    if i == input {
                        return parse_error();
                    }
                    input = i;
                    res.push_first(o);

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
                            res.push_next(o3, s);
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
    use quote::{Tokens, ToTokens};


    impl<T, D> ToTokens for Delimited<T, D>
        where T: ToTokens,
              D: ToTokens,
    {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.iter())
        }
    }

    impl<T, D> ToTokens for Element<T, D>
        where T: ToTokens,
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
