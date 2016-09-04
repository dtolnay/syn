// Copied from nom master:
// https://github.com/Geal/nom/blob/a38188f333c29d00c32a3082bec5491d2eefa33f/src/sequence.rs#L591-L687
// Will be released in nom 2.0.

#[macro_export]
macro_rules! do_parse (
  ($i:expr, $($rest:tt)*) => (
    {
      do_parse_impl!($i, 0usize, $($rest)*)
    }
  );
);

/// Internal parser, do not use directly
#[doc(hidden)]
#[macro_export]
macro_rules! do_parse_impl (

  ($i:expr, $consumed:expr, ( $($rest:expr),* )) => (
    ::nom::IResult::Done($i, ( $($rest),* ))
  );

  ($i:expr, $consumed:expr, $e:ident >> $($rest:tt)*) => (
    do_parse_impl!($i, $consumed, call!($e) >> $($rest)*);
  );
  ($i:expr, $consumed:expr, $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => (
    {
      match $submac!($i, $($args)*) {
        ::nom::IResult::Error(e)      => ::nom::IResult::Error(e),
        ::nom::IResult::Incomplete(::nom::Needed::Unknown) =>
          ::nom::IResult::Incomplete(::nom::Needed::Unknown),
        ::nom::IResult::Incomplete(::nom::Needed::Size(i)) =>
          ::nom::IResult::Incomplete(::nom::Needed::Size($consumed + i)),
        ::nom::IResult::Done(i,_)     => {
          do_parse_impl!(i,
            $consumed + (::nom::InputLength::input_len(&($i)) -
                         ::nom::InputLength::input_len(&i)), $($rest)*)
        },
      }
    }
  );

  ($i:expr, $consumed:expr, $field:ident : $e:ident >> $($rest:tt)*) => (
    do_parse_impl!($i, $consumed, $field: call!($e) >> $($rest)*);
  );

  ($i:expr, $consumed:expr, $field:ident : $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => (
    {
      match  $submac!($i, $($args)*) {
        ::nom::IResult::Error(e)      => ::nom::IResult::Error(e),
        ::nom::IResult::Incomplete(::nom::Needed::Unknown) =>
          ::nom::IResult::Incomplete(::nom::Needed::Unknown),
        ::nom::IResult::Incomplete(::nom::Needed::Size(i)) =>
          ::nom::IResult::Incomplete(::nom::Needed::Size($consumed + i)),
        ::nom::IResult::Done(i,o)     => {
          let $field = o;
          do_parse_impl!(i,
            $consumed + (::nom::InputLength::input_len(&($i)) -
                         ::nom::InputLength::input_len(&i)), $($rest)*)
        },
      }
    }
  );

  // ending the chain
  ($i:expr, $consumed:expr, $e:ident >> ( $($rest:tt)* )) => (
    do_parse_impl!($i, $consumed, call!($e) >> ( $($rest)* ));
  );

  ($i:expr, $consumed:expr, $submac:ident!( $($args:tt)* ) >> ( $($rest:tt)* )) => (
    match $submac!($i, $($args)*) {
      ::nom::IResult::Error(e)      => ::nom::IResult::Error(e),
      ::nom::IResult::Incomplete(::nom::Needed::Unknown) =>
        ::nom::IResult::Incomplete(::nom::Needed::Unknown),
      ::nom::IResult::Incomplete(::nom::Needed::Size(i)) =>
        ::nom::IResult::Incomplete(::nom::Needed::Size($consumed + i)),
      ::nom::IResult::Done(i,_)     => {
        ::nom::IResult::Done(i, ( $($rest)* ))
      },
    }
  );

  ($i:expr, $consumed:expr, $field:ident : $e:ident >> ( $($rest:tt)* )) => (
    do_parse_impl!($i, $consumed, $field: call!($e) >> ( $($rest)* ) );
  );

  ($i:expr, $consumed:expr, $field:ident : $submac:ident!( $($args:tt)* ) >> ( $($rest:tt)* )) => (
    match $submac!($i, $($args)*) {
      ::nom::IResult::Error(e)      => ::nom::IResult::Error(e),
      ::nom::IResult::Incomplete(::nom::Needed::Unknown) =>
        ::nom::IResult::Incomplete(::nom::Needed::Unknown),
      ::nom::IResult::Incomplete(::nom::Needed::Size(i)) =>
        ::nom::IResult::Incomplete(::nom::Needed::Size($consumed + i)),
      ::nom::IResult::Done(i,o)     => {
        let $field = o;
        ::nom::IResult::Done(i, ( $($rest)* ))
      },
    }
  );

);
