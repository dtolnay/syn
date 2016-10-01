#![feature(rustc_macro)]

#[macro_use]
extern crate serde_derive;

#[derive(Serialize)]
#[derive(Deserialize)]
struct DefaultStruct<A, B, C, D, E> where C: MyDefault, E: MyDefault {
    a1: A,
    #[serde(default)]
    a2: B,
    #[serde(default = "MyDefault::my_default")]
    a3: C,
    #[serde(skip_deserializing)]
    a4: D,
    #[serde(skip_deserializing, default = "MyDefault::my_default")]
    a5: E,
}

#[derive(Serialize)]
#[derive(Deserialize)]
enum DefaultEnum<A, B, C, D, E> where C: MyDefault, E: MyDefault {
    Struct {
        a1: A,
        #[serde(default)]
        a2: B,
        #[serde(default = "MyDefault::my_default")]
        a3: C,
        #[serde(skip_deserializing)]
        a4: D,
        #[serde(skip_deserializing, default = "MyDefault::my_default")]
        a5: E,
    },
}

#[derive(Deserialize)]
struct NoStdDefault(i8);

#[derive(Deserialize)]
struct ContainsNoStdDefault<A: MyDefault> {
    #[serde(default = "MyDefault::my_default")]
    a: A,
}

#[derive(Deserialize)]
struct ContainsNotDeserialize<A, B, C: DeserializeWith, E: MyDefault> {
    #[serde(skip_deserializing)]
    a: A,
    #[serde(skip_deserializing, default)]
    b: B,
    #[serde(deserialize_with = "DeserializeWith::deserialize_with", default)]
    c: C,
    #[serde(skip_deserializing, default = "MyDefault::my_default")]
    e: E,
}

#[derive(Serialize)]
#[serde(deny_unknown_fields)]
#[derive(Deserialize)]
struct DenyUnknown {
    a1: i32,
}

#[derive(Serialize)]
#[serde(rename = "Superhero")]
#[derive(Deserialize)]
struct RenameStruct {
    a1: i32,
    #[serde(rename = "a3")]
    a2: i32,
}

#[derive(Serialize)]
#[serde(rename(serialize = "SuperheroSer", deserialize = "SuperheroDe"))]
#[derive(Deserialize)]
struct RenameStructSerializeDeserialize {
    a1: i32,
    #[serde(rename(serialize = "a4", deserialize = "a5"))]
    a2: i32,
}

#[derive(Serialize)]
#[serde(rename = "Superhero")]
#[derive(Deserialize)]
enum RenameEnum {

    #[serde(rename = "bruce_wayne")]
    Batman ,

    #[serde(rename = "clark_kent")]
    Superman ( i8 ) ,

    #[serde(rename = "diana_prince")]
    WonderWoman ( i8 , i8 ) ,

    #[serde(rename = "barry_allan")]
    Flash {
        #[serde(rename = "b")]
        a : i32,
    } ,
}

#[derive(Deserialize)]
#[serde ( rename ( serialize = "SuperheroSer" , deserialize = "SuperheroDe" ) ) ]
#[derive(Serialize)]
enum RenameEnumSerializeDeserialize<A> {

    #[serde(rename(serialize = "dick_grayson", deserialize = "jason_todd"))]
    Robin {
        a: i8,
        #[serde(rename(serialize = "c"))]
        #[serde(rename(deserialize = "d"))]
        b: A,
    },
}

#[derive(Serialize)]
struct SkipSerializingStruct< 'a , B, C> where C : ShouldSkip {
    a: & 'a i8,
    #[serde(skip_serializing)]
    b: B,
    #[serde(skip_serializing_if = "ShouldSkip::should_skip")]
    c: C,
}

#[derive(Serialize)]
enum SkipSerializingEnum<'a, B, C> where C: ShouldSkip {
    Struct {
        a: &'a i8,
        #[serde(skip_serializing)]
        _b: B,
        #[serde(skip_serializing_if = "ShouldSkip::should_skip")]
        c: C,
    },
}

#[derive(Serialize)]
struct ContainsNotSerialize<'a, B, C, D> where B: 'a , D: SerializeWith {
    a: &'a Option<i8>,
    #[serde(skip_serializing)]
    b: &'a B,
    #[serde(skip_serializing)]
    c: Option<C>,
    #[serde(serialize_with = "SerializeWith::serialize_with")]
    d: D,
}

#[derive(Serialize)]
struct SerializeWithStruct<'a, B> where B: SerializeWith {
    a: &'a i8,
    #[serde(serialize_with = "SerializeWith::serialize_with")]
    b: B,
}

#[derive(Serialize)]
enum SerializeWithEnum<'a, B> where B: SerializeWith {
    Struct {
        a: &'a i8,
        #[serde(serialize_with = "SerializeWith::serialize_with")]
        b: B,
    },
}

#[derive(Deserialize)]
struct DeserializeWithStruct<B> where B: DeserializeWith {
    a: i8,
    #[serde(deserialize_with = "DeserializeWith::deserialize_with")]
    b: B,
}

#[derive(Deserialize)]
enum DeserializeWithEnum<B> where B: DeserializeWith {
    Struct {
        a: i8,
        #[serde(deserialize_with = "DeserializeWith::deserialize_with")]
        b: B,
    },
}

#[derive(Deserialize)]
enum InvalidLengthEnum {
    A(i32, i32, i32),
    B(
    #[serde(skip_deserializing)]
    i32, i32, i32),
}

#[derive(Deserialize)]
struct TupleStruct(i32, i32, i32);

#[derive(Deserialize)]
struct Struct {
    a: i32,
    b: i32,
    #[serde(skip_deserializing)]
    c: i32,
}

#[derive(Deserialize)]
enum Enum {
    Unit,
    Simple(i32),
    Seq(i32, i32, i32),
    Map {
        a: i32,
        b: i32,
        c: i32,
    },
}

#[derive(Deserialize)]
struct IgnoreBase {
    a: i32,
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct With<T> {
    t: T,
    #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
    x: X,
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct WithRef<'a, T: 'a> {
    #[serde(skip_deserializing)]
    t: Option<&'a T>,
    #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
    x: X,
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct PhantomX {
    x: PhantomData<X>,
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct PhantomT<T> {
    t: PhantomData<T>,
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct NoBounds<T> {
    t: T,
    option: Option<T>,
    boxed: Box<T>,
    option_boxed: Option<Box<T>>,
}

#[derive(Serialize)]
#[derive(Deserialize)]
enum EnumWith<T> {
    Unit,
    Newtype(
            #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
            X),
    Tuple(T,
        #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
        X),
    Struct {
        t: T,
        #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
        x: X,
    },
}

#[derive(Serialize)]
struct MultipleRef<'a, 'b, 'c, T> where T: 'c, 'c: 'b, 'b: 'a {
    t: T,
    rrrt: &'a &'b &'c T,
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct Newtype(
            #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
            X);

#[derive(Serialize)]
#[derive(Deserialize)]
struct Tuple<T>(T,
                #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
                X);

#[derive(Serialize)]
#[derive(Deserialize)]
enum TreeNode<D> {
    Split {
        left: Box<TreeNode<D>>,
        right: Box<TreeNode<D>>,
    },
    Leaf {
        data: D,
    },
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct ListNode<D> {
    data: D,
    next: Box<ListNode<D>>,
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct RecursiveA {
    b: Box<RecursiveB>,
}

#[derive(Serialize)]
#[derive(Deserialize)]
enum RecursiveB { A(RecursiveA), }

#[derive(Serialize)]
#[derive(Deserialize)]
struct RecursiveGenericA<T> {
    t: T,
    b: Box<RecursiveGenericB<T>>,
}

#[derive(Serialize)]
#[derive(Deserialize)]
enum RecursiveGenericB<T> { T(T), A(RecursiveGenericA<T>), }

#[derive(Serialize)]
struct OptionStatic<'a> {
    a: Option<&'a str>,
    b: Option<&'static str>,
}

#[derive(Serialize)]
#[serde(bound = "D: SerializeWith + DeserializeWith")]
#[derive(Deserialize)]
struct WithTraits1<D, E> {
    #[serde(serialize_with = "SerializeWith::serialize_with",
            deserialize_with = "DeserializeWith::deserialize_with")]
    d: D,
    #[serde(serialize_with = "SerializeWith::serialize_with",
            deserialize_with = "DeserializeWith::deserialize_with",
            bound = "E: SerializeWith + DeserializeWith")]
    e: E,
}

#[derive(Serialize)]
#[serde(bound(serialize = "D: SerializeWith",
            deserialize = "D: DeserializeWith"))]
#[derive(Deserialize)]
struct WithTraits2<D, E> {
    #[serde(serialize_with = "SerializeWith::serialize_with",
            deserialize_with = "DeserializeWith::deserialize_with")]
    d: D,
    #[serde(serialize_with = "SerializeWith::serialize_with",
            bound(serialize = "E: SerializeWith"))]
    #[serde(deserialize_with = "DeserializeWith::deserialize_with",
            bound(deserialize = "E: DeserializeWith"))]
    e: E,
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct CowStr<'a>(Cow<'a, str>);

#[derive(Serialize)]
#[serde(bound(deserialize = "T::Owned: Deserialize"))]
#[derive(Deserialize)]
struct CowT < 'a , T : ? Sized + 'a + ToOwned > ( Cow < 'a , T > ) ;

#[derive(Serialize)]
struct SerNamedTuple<'a, 'b, A: 'a, B: 'b, C>(&'a A, &'b mut B, C);

#[derive(Deserialize)]
struct DeNamedTuple<A, B, C>(A, B, C);

#[derive(Serialize)]
struct SerNamedMap<'a, 'b, A: 'a, B: 'b, C> {
    a: &'a A,
    b: &'b mut B,
    c: C,
}

#[derive(Deserialize)]
struct DeNamedMap<A, B, C> {
    a: A,
    b: < Vec < T > as a :: b :: Trait > :: AssociatedItem,
    c: < Vec < T > > :: AssociatedItem,
}

#[derive(Serialize)]
enum SerEnum<'a, B: 'a, C: 'a, D> where for < 'a > D: 'a {
    Unit,
    Seq(i8, B, &'a C, &'a mut D),
    Map {
        a: i8,
        b: B,
        c: &'a C,
        d: &'a mut D,
    },
    _Unit2,
    _Seq2(i8, B, &'a C, &'a mut D),
    _Map2 {
        a: i8,
        b: B,
        c: &'a C,
        d: &'a mut D,
    },
}

#[derive(Serialize)]
#[derive(Deserialize)]
enum DeEnum<B, C, D> {
    Unit,
    Seq(i8, B, C, D),
    Map {
        a: i8,
        b: B,
        c: C,
        d: D,
    },
    _Unit2,
    _Seq2(i8, B, C, D),
    _Map2 {
        a: i8,
        b: B,
        c: C,
        d: D,
    },
}

#[derive(Serialize)]
enum Lifetimes<'a> {
    LifetimeSeq(&'a i32),
    NoLifetimeSeq(i32),
    LifetimeMap {
        a: &'a i32,
    },
    NoLifetimeMap {
        a: i32,
    },
}

#[derive(Serialize)]
#[derive(Deserialize)]
pub struct GenericStruct<T> {
    x: T,
}

#[derive(Serialize)]
#[derive(Deserialize)]
pub struct GenericNewTypeStruct<T>(T);

#[derive(Serialize)]
#[derive(Deserialize)]
pub struct GenericTupleStruct<T, U>(T, U);

#[derive(Serialize)]
#[derive(Deserialize)]
pub enum GenericEnum<T, U: for < 'a > F<'a>> {
    Unit,
    NewType(T),
    Seq(T, U),
    Map {
        x: T,
        y: U,
    },
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct DefaultTyParam<T: AssociatedType<X = i32> = i32> {
    phantom: std :: marker :: PhantomData<T>,
}

#[derive(Serialize)]
struct UnitStruct;

#[derive(Serialize)]
struct TupleStruct(i32, i32, i32);

#[derive(Serialize)]
struct Struct {
    a: i32,
    b: i32,
    c: i32,
}

#[derive(Serialize)]
enum Enum {
    Unit,
    One(i32),
    Seq(i32, i32),
    Map {
        a: i32,
        b: i32,
    },
}

#[derive(Serialize)]
#[derive(Deserialize)]
struct Bounds<T: Serialize + Deserialize> {
    t: T,
    option: Option<T>,
    boxed: Box<T>,
    option_boxed: Option<Box<T>>,
}

#[derive(Deserialize)]
#[rustc_copy_clone_marker]
struct UnitStruct;

#[derive(Serialize)]
#[allow(dead_code)]
#[deny(unused_variables)]
enum Void { }

#[derive(Serialize)]
#[derive(Deserialize)]
struct NamedUnit;

#[derive(Deserialize)]
#[doc = r" A custom basic event not covered by the Matrix specification."]
#[derive(Debug, Serialize)]
pub struct CustomEvent {
    /// The event's content.
    content: String,
}
