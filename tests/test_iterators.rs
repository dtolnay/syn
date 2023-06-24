#![allow(clippy::uninlined_format_args)]

use syn::punctuated::{Pair, Punctuated};
use syn::Token;

#[macro_use]
mod macros;

macro_rules! check_exact_size_iterator {
    ($iter:expr) => {{
        let iter = $iter;
        let size_hint = iter.size_hint();
        let len = iter.len();
        let count = iter.count();
        assert_eq!(len, count);
        assert_eq!(size_hint, (count, Some(count)));
    }};
}

#[test]
fn pairs() {
    let mut p: Punctuated<_, Token![,]> = punctuated!(2, 3, 4);

    check_exact_size_iterator!(p.pairs());
    check_exact_size_iterator!(p.pairs_mut());
    check_exact_size_iterator!(p.into_pairs());

    let mut p: Punctuated<_, Token![,]> = punctuated!(2, 3, 4);

    assert_eq!(p.pairs().next_back().map(Pair::into_value), Some(&4));
    assert_eq!(
        p.pairs_mut().next_back().map(Pair::into_value),
        Some(&mut 4)
    );
    assert_eq!(p.into_pairs().next_back().map(Pair::into_value), Some(4));
}

#[test]
fn iter() {
    let mut p: Punctuated<_, Token![,]> = punctuated!(2, 3, 4);

    check_exact_size_iterator!(p.iter());
    check_exact_size_iterator!(p.iter_mut());
    check_exact_size_iterator!(p.into_iter());

    let mut p: Punctuated<_, Token![,]> = punctuated!(2, 3, 4);

    assert_eq!(p.iter().next_back(), Some(&4));
    assert_eq!(p.iter_mut().next_back(), Some(&mut 4));
    assert_eq!(p.into_iter().next_back(), Some(4));
}

#[test]
fn may_dangle() {
    let p: Punctuated<_, Token![,]> = punctuated!(2, 3, 4);
    for element in &p {
        if *element == 2 {
            drop(p);
            break;
        }
    }

    let mut p: Punctuated<_, Token![,]> = punctuated!(2, 3, 4);
    for element in &mut p {
        if *element == 2 {
            drop(p);
            break;
        }
    }
}

#[test]
fn extend() {
    let mut p: Punctuated<i32, Token![,]> = punctuated!(1, 2, 3);

    // No punctuation at the end after calling extend on an empty iterator of items T.
    p.extend(None::<i32>);
    assert_eq!(p, punctuated!(1, 2, 3));

    // But punctuation at the end after calling extend on an empty iterator of pairs.
    // This behavior is not consistent and should be changed in a future breaking release
    // to behave like above.
    p.extend(None::<Pair<_, _>>);
    let mut expected = punctuated!(1, 2, 3);
    expected.push_punct(Default::default());
    assert_eq!(p, expected);

    p.extend([4, 5, 6]);
    assert_eq!(p, punctuated!(1, 2, 3, 4, 5, 6));

    p.extend([Pair::Punctuated(7, Default::default())]);
    let mut expected = punctuated!(1, 2, 3, 4, 5, 6, 7);
    expected.push_punct(Default::default());
    assert_eq!(p, expected);

    p.extend([Pair::Punctuated(8, Default::default()), Pair::End(9)]);
    assert_eq!(p, punctuated!(1, 2, 3, 4, 5, 6, 7, 8, 9));
}
