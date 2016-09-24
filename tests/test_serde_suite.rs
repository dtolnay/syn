extern crate syn;

#[macro_use]
extern crate quote;

/// These are all of the items from serde_codegen's test suite.
/// Obnoxious whitespace has been added in an attempt to fool the parser.
#[test]
fn test_all() {
    for s in ITEMS {
        let ast = syn::parse_macro_input(s).unwrap();
        let tokens = quote!(#ast).to_string();
        assert_eq!(ast, syn::parse_macro_input(&tokens).unwrap());
    }

    static ITEMS: &'static [&'static str] = &[
        r#"
        # [ derive ( Serialize ) ]
         #[derive(Deserialize)]
        struct DefaultStruct <A , B, C, D, E> where C : MyDefault , E: MyDefault {
            a1 : A,
            #[serde(default)]
            a2: B,
            #[serde(default = "MyDefault::my_default")]
            a3: C,
            #[serde(skip_deserializing)]
            a4: D,
            #[serde(skip_deserializing, default = "MyDefault::my_default")]
            a5: E,
        }
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Deserialize)]
        struct NoStdDefault(i8);
        "#,
        r#"
        #[derive(Deserialize)]
        struct ContainsNoStdDefault <A : MyDefault> {
            #[serde(default = "MyDefault::my_default")]
            a: A,
        }
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        #[serde(deny_unknown_fields)]
        #[derive(Deserialize)]
        struct DenyUnknown {
            a1: i32,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[serde(rename = "Superhero")]
        #[derive(Deserialize)]
        struct RenameStruct {
            a1: i32,
            #[serde(rename = "a3")]
            a2: i32,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[serde(rename(serialize = "SuperheroSer", deserialize = "SuperheroDe"))]
        #[derive(Deserialize)]
        struct RenameStructSerializeDeserialize {
            a1: i32,
            #[serde(rename(serialize = "a4", deserialize = "a5"))]
            a2: i32,
        }
        "#,
        r#"
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
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        struct SkipSerializingStruct< 'a , B, C> where C : ShouldSkip {
            a: & 'a i8,
            #[serde(skip_serializing)]
            b: B,
            #[serde(skip_serializing_if = "ShouldSkip::should_skip")]
            c: C,
        }
        "#,
        r#"
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
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        struct SerializeWithStruct<'a, B> where B: SerializeWith {
            a: &'a i8,
            #[serde(serialize_with = "SerializeWith::serialize_with")]
            b: B,
        }
        "#,
        r#"
        #[derive(Serialize)]
        enum SerializeWithEnum<'a, B> where B: SerializeWith {
            Struct {
                a: &'a i8,
                #[serde(serialize_with = "SerializeWith::serialize_with")]
                b: B,
            },
        }
        "#,
        r#"
        #[derive(Deserialize)]
        struct DeserializeWithStruct<B> where B: DeserializeWith {
            a: i8,
            #[serde(deserialize_with = "DeserializeWith::deserialize_with")]
            b: B,
        }
        "#,
        r#"
        #[derive(Deserialize)]
        enum DeserializeWithEnum<B> where B: DeserializeWith {
            Struct {
                a: i8,
                #[serde(deserialize_with = "DeserializeWith::deserialize_with")]
                b: B,
            },
        }
        "#,
        r#"
        #[derive(Deserialize)]
        enum InvalidLengthEnum {
            A(i32, i32, i32),
            B(
            #[serde(skip_deserializing)]
            i32, i32, i32),
        }
        "#,
        r#"
        #[derive(Deserialize)]
        struct TupleStruct(i32, i32, i32);
        "#,
        r#"
        #[derive(Deserialize)]
        struct Struct {
            a: i32,
            b: i32,
            #[serde(skip_deserializing)]
            c: i32,
        }
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Deserialize)]
        struct IgnoreBase {
            a: i32,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct With<T> {
            t: T,
            #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
            x: X,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct WithRef<'a, T: 'a> {
            #[serde(skip_deserializing)]
            t: Option<&'a T>,
            #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
            x: X,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct PhantomX {
            x: PhantomData<X>,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct PhantomT<T> {
            t: PhantomData<T>,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct NoBounds<T> {
            t: T,
            option: Option<T>,
            boxed: Box<T>,
            option_boxed: Option<Box<T>>,
        }
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        struct MultipleRef<'a, 'b, 'c, T> where T: 'c, 'c: 'b, 'b: 'a {
            t: T,
            rrrt: &'a &'b &'c T,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct Newtype(
                    #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
                    X);
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct Tuple<T>(T,
                        #[serde(serialize_with = "ser_x", deserialize_with = "de_x")]
                        X);
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct ListNode<D> {
            data: D,
            next: Box<ListNode<D>>,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct RecursiveA {
            b: Box<RecursiveB>,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        enum RecursiveB { A(RecursiveA), }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct RecursiveGenericA<T> {
            t: T,
            b: Box<RecursiveGenericB<T>>,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        enum RecursiveGenericB<T> { T(T), A(RecursiveGenericA<T>), }
        "#,
        r#"
        #[derive(Serialize)]
        struct OptionStatic<'a> {
            a: Option<&'a str>,
            b: Option<&'static str>,
        }
        "#,
        r#"
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
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct CowStr<'a>(Cow<'a, str>);
        "#,
        r#"
        #[derive(Serialize)]
        #[serde(bound(deserialize = "T::Owned: Deserialize"))]
        #[derive(Deserialize)]
        struct CowT < 'a , T : ? Sized + 'a + ToOwned > ( Cow < 'a , T > ) ;
        "#,
        r#"
        #[derive(Serialize)]
        struct SerNamedTuple<'a, 'b, A: 'a, B: 'b, C>(&'a A, &'b mut B, C);
        "#,
        r#"
        #[derive(Deserialize)]
        struct DeNamedTuple<A, B, C>(A, B, C);
        "#,
        r#"
        #[derive(Serialize)]
        struct SerNamedMap<'a, 'b, A: 'a, B: 'b, C> {
            a: &'a A,
            b: &'b mut B,
            c: C,
        }
        "#,
        r#"
        #[derive(Deserialize)]
        struct DeNamedMap<A, B, C> {
            a: A,
            b: < Vec < T > as a :: b :: Trait > :: AssociatedItem,
            c: < Vec < T > > :: AssociatedItem,
        }
        "#,
        r#"
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
        "#,
        r#"
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
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        pub struct GenericStruct<T> {
            x: T,
        }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        pub struct GenericNewTypeStruct<T>(T);
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        pub struct GenericTupleStruct<T, U>(T, U);
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct DefaultTyParam<T: AssociatedType<X = i32> = i32> {
            phantom: std :: marker :: PhantomData<T>,
        }
        "#,
        r#"
        #[derive(Serialize)]
        struct UnitStruct;
        "#,
        r#"
        #[derive(Serialize)]
        struct TupleStruct(i32, i32, i32);
        "#,
        r#"
        #[derive(Serialize)]
        struct Struct {
            a: i32,
            b: i32,
            c: i32,
        }
        "#,
        r#"
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
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct Bounds<T: Serialize + Deserialize> {
            t: T,
            option: Option<T>,
            boxed: Box<T>,
            option_boxed: Option<Box<T>>,
        }
        "#,
        r#"
        #[derive(Deserialize)]
        #[rustc_copy_clone_marker]
        struct UnitStruct;
        "#,
        r#"
        #[derive(Serialize)]
        #[allow(dead_code)]
        #[deny(unused_variables)]
        enum Void { }
        "#,
        r#"
        #[derive(Serialize)]
        #[derive(Deserialize)]
        struct NamedUnit;
        "#,
    ];
}
