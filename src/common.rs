pub type Ident = String;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Inherited,
}

fn ident_ch(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

named!(pub word<&str, Ident>, preceded!(
    opt!(call!(::nom::multispace)),
    map!(take_while1_s!(ident_ch), String::from)
));

named!(pub visibility<&str, Visibility>, preceded!(
    opt!(call!(::nom::multispace)),
    alt!(
        terminated!(tag_s!("pub"), call!(::nom::multispace)) => { |_| Visibility::Public }
        |
        epsilon!() => { |_| Visibility::Inherited }
    )
));
