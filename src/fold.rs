// Adapted from libsyntax.

//! A Folder represents an AST->AST fold; it accepts an AST piece,
//! and returns a piece of the same type.

use super::*;
#[cfg(not(feature = "full"))]
use constant;

use delimited::{Delimited, Element};

/// AST->AST fold.
///
/// Each method of the Folder trait is a hook to be potentially overridden. Each
/// method's default implementation recursively visits the substructure of the
/// input via the `noop_fold` methods, which perform an "identity fold", that
/// is, they return the same structure that they are given (for example the
/// `fold_crate` method by default calls `fold::noop_fold_crate`).
///
/// If you want to ensure that your code handles every variant explicitly, you
/// need to override each method and monitor future changes to `Folder` in case
/// a new method with a new default implementation gets introduced.
pub trait Folder {
    // Any additions to this trait should happen in form
    // of a call to a public `noop_*` function that only calls
    // out to the folder again, not other `noop_*` functions.
    //
    // This is a necessary API workaround to the problem of not
    // being able to call out to the super default method
    // in an overridden default method.

    fn fold_ident(&mut self, _ident: Ident) -> Ident {
        noop_fold_ident(self, _ident)
    }
    fn fold_derive_input(&mut self, derive_input: DeriveInput) -> DeriveInput {
        noop_fold_derive_input(self, derive_input)
    }
    fn fold_ty(&mut self, ty: Ty) -> Ty {
        noop_fold_ty(self, ty)
    }
    fn fold_generics(&mut self, generics: Generics) -> Generics {
        noop_fold_generics(self, generics)
    }
    fn fold_ty_param_bound(&mut self, bound: TyParamBound) -> TyParamBound {
        noop_fold_ty_param_bound(self, bound)
    }
    fn fold_poly_trait_ref(&mut self, trait_ref: PolyTraitRef) -> PolyTraitRef {
        noop_fold_poly_trait_ref(self, trait_ref)
    }
    fn fold_variant_data(&mut self, data: VariantData) -> VariantData {
        noop_fold_variant_data(self, data)
    }
    fn fold_field(&mut self, field: Field) -> Field {
        noop_fold_field(self, field)
    }
    fn fold_variant(&mut self, variant: Variant) -> Variant {
        noop_fold_variant(self, variant)
    }
    fn fold_lifetime(&mut self, _lifetime: Lifetime) -> Lifetime {
        noop_fold_lifetime(self, _lifetime)
    }
    fn fold_lifetime_def(&mut self, lifetime: LifetimeDef) -> LifetimeDef {
        noop_fold_lifetime_def(self, lifetime)
    }
    fn fold_path(&mut self, path: Path) -> Path {
        noop_fold_path(self, path)
    }
    fn fold_path_segment(&mut self, path_segment: PathSegment) -> PathSegment {
        noop_fold_path_segment(self, path_segment)
    }
    fn fold_path_parameters(&mut self, path_parameters: PathParameters) -> PathParameters {
        noop_fold_path_parameters(self, path_parameters)
    }
    fn fold_assoc_type_binding(&mut self, type_binding: TypeBinding) -> TypeBinding {
        noop_fold_assoc_type_binding(self, type_binding)
    }
    fn fold_attribute(&mut self, _attr: Attribute) -> Attribute {
        noop_fold_attribute(self, _attr)
    }
    fn fold_fn_ret_ty(&mut self, ret_ty: FunctionRetTy) -> FunctionRetTy {
        noop_fold_fn_ret_ty(self, ret_ty)
    }
    fn fold_const_expr(&mut self, expr: ConstExpr) -> ConstExpr {
        noop_fold_const_expr(self, expr)
    }
    fn fold_lit(&mut self, _lit: Lit) -> Lit {
        noop_fold_lit(self, _lit)
    }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        noop_fold_mac(self, mac)
    }

    #[cfg(feature = "full")]
    fn fold_crate(&mut self, _crate: Crate) -> Crate {
        noop_fold_crate(self, _crate)
    }
    #[cfg(feature = "full")]
    fn fold_item(&mut self, item: Item) -> Item {
        noop_fold_item(self, item)
    }
    #[cfg(feature = "full")]
    fn fold_expr(&mut self, expr: Expr) -> Expr {
        noop_fold_expr(self, expr)
    }
    #[cfg(feature = "full")]
    fn fold_foreign_item(&mut self, foreign_item: ForeignItem) -> ForeignItem {
        noop_fold_foreign_item(self, foreign_item)
    }
    #[cfg(feature = "full")]
    fn fold_pat(&mut self, pat: Pat) -> Pat {
        noop_fold_pat(self, pat)
    }
    #[cfg(feature = "full")]
    fn fold_fn_decl(&mut self, fn_decl: FnDecl) -> FnDecl {
        noop_fold_fn_decl(self, fn_decl)
    }
    #[cfg(feature = "full")]
    fn fold_trait_item(&mut self, trait_item: TraitItem) -> TraitItem {
        noop_fold_trait_item(self, trait_item)
    }
    #[cfg(feature = "full")]
    fn fold_impl_item(&mut self, impl_item: ImplItem) -> ImplItem {
        noop_fold_impl_item(self, impl_item)
    }
    #[cfg(feature = "full")]
    fn fold_method_sig(&mut self, method_sig: MethodSig) -> MethodSig {
        noop_fold_method_sig(self, method_sig)
    }
    #[cfg(feature = "full")]
    fn fold_stmt(&mut self, stmt: Stmt) -> Stmt {
        noop_fold_stmt(self, stmt)
    }
    #[cfg(feature = "full")]
    fn fold_block(&mut self, block: Block) -> Block {
        noop_fold_block(self, block)
    }
    #[cfg(feature = "full")]
    fn fold_local(&mut self, local: Local) -> Local {
        noop_fold_local(self, local)
    }
    #[cfg(feature = "full")]
    fn fold_view_path(&mut self, view_path: ViewPath) -> ViewPath {
        noop_fold_view_path(self, view_path)
    }
}

trait LiftOnce<T, U> {
    type Output;
    fn lift<F>(self, f: F) -> Self::Output where F: FnOnce(T) -> U;
}

impl<T, U> LiftOnce<T, U> for Box<T> {
    type Output = Box<U>;
    // Clippy false positive
    // https://github.com/Manishearth/rust-clippy/issues/1478
    #[cfg_attr(feature = "cargo-clippy", allow(boxed_local))]
    fn lift<F>(self, f: F) -> Box<U>
        where F: FnOnce(T) -> U
    {
        Box::new(f(*self))
    }
}

trait LiftMut<T, U> {
    type Output;
    fn lift<F>(self, f: F) -> Self::Output where F: FnMut(T) -> U;
}

impl<T, U> LiftMut<T, U> for Vec<T> {
    type Output = Vec<U>;
    fn lift<F>(self, f: F) -> Vec<U>
        where F: FnMut(T) -> U
    {
        self.into_iter().map(f).collect()
    }
}

impl<T, D, U> LiftMut<T, U> for Delimited<T, D> {
    type Output = Delimited<U, D>;
    fn lift<F>(self, mut f: F) -> Self::Output
        where F: FnMut(T) -> U
    {
        self.into_iter().map(|e| {
            match e {
                Element::Delimited(t, d) => Element::Delimited(f(t), d),
                Element::End(t) => Element::End(f(t))
            }
        }).collect()
    }
}

pub fn noop_fold_ident<F: ?Sized + Folder>(_: &mut F, _ident: Ident) -> Ident {
    _ident
}

pub fn noop_fold_derive_input<F: ?Sized + Folder>(folder: &mut F,
                                         DeriveInput{ ident,
                                                      vis,
                                                      attrs,
                                                      generics,
body }: DeriveInput) -> DeriveInput{
    use Body::*;
    DeriveInput {
        ident: folder.fold_ident(ident),
        vis: noop_fold_vis(folder, vis),
        attrs: attrs.lift(|a| folder.fold_attribute(a)),
        generics: folder.fold_generics(generics),
        body: match body {
            Enum(data) => {
                Enum(BodyEnum {
                    variants: data.variants.lift(move |v| folder.fold_variant(v)),
                    ..data
                })
            }
            Struct(data) => {
                Struct(BodyStruct {
                    data: folder.fold_variant_data(data.data),
                    ..data
                })
            }
        },
    }
}

pub fn noop_fold_ty<F: ?Sized + Folder>(folder: &mut F, ty: Ty) -> Ty {
    use ty::*;
    use Ty::*;

    match ty {
        Slice(t) => {
            Slice(TySlice {
                ty: t.ty.lift(|v| folder.fold_ty(v)),
                ..t
            })
        }
        Group(t) => {
            Group(TyGroup {
                ty: t.ty.lift(|v| folder.fold_ty(v)),
                ..t
            })
        }
        Paren(t) => {
            Paren(TyParen {
                ty: t.ty.lift(|v| folder.fold_ty(v)),
                ..t
            })
        }
        Ptr(t) => {
            let ty = *t.ty;
            let MutTy { ty, mutability } = ty;
            Ptr(TyPtr {
                ty: Box::new(MutTy {
                    ty: folder.fold_ty(ty),
                    mutability: mutability,
                }),
                ..t
            })
        }
        Rptr(t) => {
            let ty = *t.ty;
            let MutTy { ty, mutability } = ty;
            Rptr(TyRptr {
                lifetime: t.lifetime.map(|l| folder.fold_lifetime(l)),
                ty: Box::new(MutTy {
                    ty: folder.fold_ty(ty),
                    mutability: mutability,
                }),
                ..t
            })
        }
        Never(t) => Never(t),
        Infer(t) => Infer(t),
        Tup(t) => {
            Tup(TyTup {
                tys: t.tys.lift(|x| folder.fold_ty(x)),
                ..t
            })
        }
        BareFn(t) => {
            let ty = *t.ty;
            BareFn(TyBareFn {
                ty: Box::new(BareFnTy {
                    lifetimes: ty.lifetimes.map(|l| {
                        noop_fold_bound_lifetimes(folder, l)
                    }),
                    inputs: ty.inputs.lift(|v| {
                        BareFnArg {
                            name: v.name.map(|n| (folder.fold_ident(n.0), n.1)),
                            ty: folder.fold_ty(v.ty),
                        }
                    }),
                    output: folder.fold_fn_ret_ty(ty.output),
                    ..ty
                }),
            })
        }
        Path(t) => {
            Path(TyPath {
                qself: t.qself.map(|v| noop_fold_qself(folder, v)),
                path: folder.fold_path(t.path),
            })
        }
        Array(t) => {
            Array(TyArray {
                ty: t.ty.lift(|v| folder.fold_ty(v)),
                amt: folder.fold_const_expr(t.amt),
                ..t
            })
        }
        TraitObject(t) => {
            TraitObject(TyTraitObject {
                bounds: t.bounds.lift(|v| folder.fold_ty_param_bound(v)),
            })
        }
        ImplTrait(t) => {
            ImplTrait(TyImplTrait {
                bounds: t.bounds.lift(|v| folder.fold_ty_param_bound(v)),
                ..t
            })
        }
        Mac(mac) => Mac(folder.fold_mac(mac)),
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))] // clippy lies
fn noop_fold_qself<F: ?Sized + Folder>(folder: &mut F, qself: QSelf) -> QSelf {
    QSelf {
        ty: Box::new(folder.fold_ty(*(qself.ty))),
        ..qself
    }
}

pub fn noop_fold_generics<F: ?Sized + Folder>(folder: &mut F,
                                              generics: Generics)
-> Generics{
    use WherePredicate::*;
    Generics {
        lifetimes: generics.lifetimes.lift(|l| folder.fold_lifetime_def(l)),
        ty_params: generics.ty_params.lift(|ty| {
            TyParam {
                attrs: ty.attrs.lift(|a| folder.fold_attribute(a)),
                ident: folder.fold_ident(ty.ident),
                bounds: ty.bounds.lift(|ty_pb| folder.fold_ty_param_bound(ty_pb)),
                default: ty.default.map(|v| folder.fold_ty(v)),
                ..ty
            }
        }),
        where_clause: WhereClause {
            predicates: generics.where_clause.predicates.lift(|p| {
                match p {
                    BoundPredicate(bound_predicate) => {
                        BoundPredicate(WhereBoundPredicate {
                            bound_lifetimes: bound_predicate.bound_lifetimes
                                .map(|l| noop_fold_bound_lifetimes(folder, l)),
                            bounded_ty: folder.fold_ty(bound_predicate.bounded_ty),
                            bounds: bound_predicate.bounds
                                .lift(|ty_pb| folder.fold_ty_param_bound(ty_pb)),
                            ..bound_predicate
                        })
                    }
                    RegionPredicate(region_predicate) => {
                        RegionPredicate(WhereRegionPredicate {
                            lifetime: folder.fold_lifetime(region_predicate.lifetime),
                            bounds: region_predicate.bounds
                                .lift(|b| folder.fold_lifetime(b)),
                            ..region_predicate
                        })
                    }
                    EqPredicate(eq_predicate) => {
                        EqPredicate(WhereEqPredicate {
                            lhs_ty: folder.fold_ty(eq_predicate.lhs_ty),
                            rhs_ty: folder.fold_ty(eq_predicate.rhs_ty),
                            ..eq_predicate
                        })
                    }
                }
            }),
            ..generics.where_clause
        },
        ..generics
    }
}

pub fn noop_fold_ty_param_bound<F: ?Sized + Folder>(folder: &mut F,
                                                    bound: TyParamBound)
                                                    -> TyParamBound {
    use TyParamBound::*;
    match bound {
        Trait(ty, modifier) => Trait(folder.fold_poly_trait_ref(ty), modifier),
        Region(lifetime) => Region(folder.fold_lifetime(lifetime)),
    }
}

pub fn noop_fold_poly_trait_ref<F: ?Sized + Folder>(folder: &mut F,
                                                    trait_ref: PolyTraitRef)
                                                    -> PolyTraitRef {
    PolyTraitRef {
        bound_lifetimes: trait_ref.bound_lifetimes.map(|bl| {
            noop_fold_bound_lifetimes(folder, bl)
        }),
        trait_ref: folder.fold_path(trait_ref.trait_ref),
    }
}

pub fn noop_fold_variant_data<F: ?Sized + Folder>(folder: &mut F,
                                                  data: VariantData)
                                                  -> VariantData {
    use VariantData::*;
    match data {
        Struct(fields, t) => Struct(fields.lift(|f| folder.fold_field(f)), t),
        Tuple(fields, t) => Tuple(fields.lift(|f| folder.fold_field(f)), t),
        Unit => Unit,
    }
}

pub fn noop_fold_field<F: ?Sized + Folder>(folder: &mut F, field: Field) -> Field {
    Field {
        ident: field.ident.map(|i| folder.fold_ident(i)),
        vis: noop_fold_vis(folder, field.vis),
        attrs: field.attrs.lift(|a| folder.fold_attribute(a)),
        ty: folder.fold_ty(field.ty),
        ..field
    }
}

pub fn noop_fold_variant<F: ?Sized + Folder>(folder: &mut F,
                                             variant: Variant)
    -> Variant
{
    Variant {
        ident: folder.fold_ident(variant.ident),
        attrs: variant.attrs.lift(|v| folder.fold_attribute(v)),
        data: folder.fold_variant_data(variant.data),
        discriminant: variant.discriminant.map(|ce| folder.fold_const_expr(ce)),
        ..variant
    }
}

pub fn noop_fold_lifetime<F: ?Sized + Folder>(_: &mut F, _lifetime: Lifetime) -> Lifetime {
    _lifetime
}

pub fn noop_fold_bound_lifetimes<F: ?Sized + Folder>(folder: &mut F,
                                                     b: BoundLifetimes)
    -> BoundLifetimes
{
    BoundLifetimes {
        lifetimes: b.lifetimes.lift(|l| folder.fold_lifetime_def(l)),
        ..b
    }
}

pub fn noop_fold_lifetime_def<F: ?Sized + Folder>(folder: &mut F,
                                                  def: LifetimeDef)
    -> LifetimeDef
{
    LifetimeDef {
        attrs: def.attrs.lift(|x| folder.fold_attribute(x)),
        lifetime: folder.fold_lifetime(def.lifetime),
        bounds: def.bounds.lift(|l| folder.fold_lifetime(l)),
        ..def
    }
}

pub fn noop_fold_path<F: ?Sized + Folder>(folder: &mut F, path: Path) -> Path {
    Path {
        segments: path.segments.lift(|s| folder.fold_path_segment(s)),
        ..path
    }
}

pub fn noop_fold_path_segment<F: ?Sized + Folder>(folder: &mut F,
                                                  seg: PathSegment)
                                                  -> PathSegment {
    PathSegment {
        ident: folder.fold_ident(seg.ident),
        parameters: folder.fold_path_parameters(seg.parameters),
    }
}

pub fn noop_fold_path_parameters<F: ?Sized + Folder>(folder: &mut F,
                                                     path_parameters: PathParameters)
                                                     -> PathParameters {
    use PathParameters::*;
    match path_parameters {
        None => None,
        AngleBracketed(d) => {
            AngleBracketed(AngleBracketedParameterData {
                lifetimes: d.lifetimes.lift(|l| folder.fold_lifetime(l)),
                types: d.types.lift(|ty| folder.fold_ty(ty)),
                bindings: d.bindings.lift(|tb| folder.fold_assoc_type_binding(tb)),
                ..d
            })
        }
        Parenthesized(d) => {
            Parenthesized(ParenthesizedParameterData {
                inputs: d.inputs.lift(|i| folder.fold_ty(i)),
                output: folder.fold_fn_ret_ty(d.output),
                ..d
            })
        }
    }
}

pub fn noop_fold_assoc_type_binding<F: ?Sized + Folder>(folder: &mut F,
                                                        binding: TypeBinding)
    -> TypeBinding
{
    TypeBinding {
        ident: folder.fold_ident(binding.ident),
        ty: folder.fold_ty(binding.ty),
        ..binding
    }
}

pub fn noop_fold_attribute<F: ?Sized + Folder>(_: &mut F, attr: Attribute) -> Attribute {
    attr
}

pub fn noop_fold_fn_ret_ty<F: ?Sized + Folder>(folder: &mut F,
                                               ret_ty: FunctionRetTy)
                                               -> FunctionRetTy {
    use FunctionRetTy::*;
    match ret_ty {
        Default => Default,
        Ty(ty, t) => Ty(folder.fold_ty(ty), t),
    }
}

pub fn noop_fold_const_expr<F: ?Sized + Folder>(folder: &mut F, expr: ConstExpr) -> ConstExpr {
    use constant::*;
    use constant::ConstExpr::*;

    match expr {
        Call(c) => {
            Call(ConstCall {
                func: c.func.lift(|e| folder.fold_const_expr(e)),
                args: c.args.lift(|v| folder.fold_const_expr(v)),
                ..c
            })
        }
        Binary(c) => {
            Binary(ConstBinary {
                left: c.left.lift(|e| folder.fold_const_expr(e)),
                right: c.right.lift(|e| folder.fold_const_expr(e)),
                ..c
            })
        }
        Unary(c) => {
            Unary(ConstUnary {
                expr: c.expr.lift(|e| folder.fold_const_expr(e)),
                ..c
            })
        }
        Lit(l) => Lit(folder.fold_lit(l)),
        Cast(c) => {
            Cast(ConstCast {
                expr: c.expr.lift(|e| folder.fold_const_expr(e)),
                ty: c.ty.lift(|v| folder.fold_ty(v)),
                ..c
            })
        }
        Path(p) => Path(folder.fold_path(p)),
        Index(c) => {
            Index(ConstIndex {
                expr: c.expr.lift(|e| folder.fold_const_expr(e)),
                index: c.index.lift(|e| folder.fold_const_expr(e)),
                ..c
            })
        }
        Paren(c) => {
            Paren(ConstParen {
                expr: c.expr.lift(|e| folder.fold_const_expr(e)),
                ..c
            })
        }
        Other(e) => Other(noop_fold_other_const_expr(folder, e)),
    }
}

#[cfg(feature = "full")]
fn noop_fold_other_const_expr<F: ?Sized + Folder>(folder: &mut F, e: Expr) -> Expr {
    folder.fold_expr(e)
}

#[cfg(not(feature = "full"))]
fn noop_fold_other_const_expr<F: ?Sized + Folder>(_: &mut F,
                                                  e: constant::Other)
                                                  -> constant::Other {
    e
}

pub fn noop_fold_lit<F: ?Sized + Folder>(_: &mut F, _lit: Lit) -> Lit {
    _lit
}

pub fn noop_fold_tt<F: ?Sized + Folder>(folder: &mut F, tt: TokenTree) -> TokenTree {
    use proc_macro2::{TokenKind, TokenTree as TokenTree2};
    match tt.0.kind {
        TokenKind::Word(sym) => {
            let sym = folder.fold_ident(Ident::new(sym, Span(tt.0.span)));
            TokenTree(TokenTree2 {
                span: sym.span.0,
                kind: TokenKind::Word(sym.sym),
            })
        }
        TokenKind::Op(..) => tt,
        TokenKind::Literal(lit) => {
            folder.fold_lit(Lit {
                value: LitKind::Other(lit),
                span: Span(tt.0.span),
            }).into_token_tree()
        }
        TokenKind::Sequence(delim, stream) => {
            let stream = stream.into_iter().map(|tt| {
                noop_fold_tt(folder, TokenTree(tt)).0
            }).collect();
            TokenTree(TokenTree2 {
                span: tt.0.span,
                kind: TokenKind::Sequence(delim, stream),
            })
        }
    }
}

pub fn noop_fold_mac<F: ?Sized + Folder>(folder: &mut F, mac: Mac) -> Mac {
    Mac {
        path: folder.fold_path(mac.path),
        tokens: mac.tokens.lift(|tt| noop_fold_tt(folder, tt)),
        ..mac
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_crate<F: ?Sized + Folder>(folder: &mut F,
                                           krate: Crate)
                                           -> Crate {
    Crate {
        attrs: krate.attrs.lift(|a| folder.fold_attribute(a)),
        items: krate.items.lift(|i| folder.fold_item(i)),
        ..krate
    }

}

#[cfg(feature = "full")]
pub fn noop_fold_block<F: ?Sized + Folder>(folder: &mut F, block: Block) -> Block {
    Block {
        stmts: block.stmts.lift(|s| folder.fold_stmt(s)),
        ..block
    }
}

fn noop_fold_vis<F: ?Sized + Folder>(folder: &mut F, vis: Visibility) -> Visibility {
    use Visibility::*;
    match vis {
        Crate(t) => Crate(t),
        Inherited(i) => Inherited(i),
        Public(p) => Public(p),
        Restricted(data) => {
            Restricted(VisRestricted {
                path: data.path.lift(|p| folder.fold_path(p)),
                ..data
            })
        }
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_item<F: ?Sized + Folder>(folder: &mut F,
                                          Item { attrs, node }: Item)
                                          -> Item {
    use item::*;
    use ItemKind::*;
    Item {
        attrs: attrs.lift(|a| folder.fold_attribute(a)),
        node: match node {
            ExternCrate(i) => {
                ExternCrate(ItemExternCrate {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    rename: i.rename.map(|(_as, id)| (_as, folder.fold_ident(id))),
                    ..i
                })
            }
            Use(i) => {
                Use(ItemUse {
                    vis: noop_fold_vis(folder, i.vis),
                    path: Box::new(folder.fold_view_path(*i.path)),
                    ..i
                })
            }
            Static(i) => {
                Static(ItemStatic {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    ty: Box::new(folder.fold_ty(*i.ty)),
                    expr: i.expr.lift(|e| folder.fold_expr(e)),
                    ..i
                })
            }
            Const(i) => {
                Const(ItemConst {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    ty: i.ty.lift(|ty| folder.fold_ty(ty)),
                    expr: i.expr.lift(|e| folder.fold_expr(e)),
                    ..i
                })
            }
            Fn(i) => {
                Fn(ItemFn {
                    vis: noop_fold_vis(folder, i.vis),
                    decl: i.decl.lift(|v| folder.fold_fn_decl(v)),
                    block: i.block.lift(|v| folder.fold_block(v)),
                    ..i
                })
            }
            Mod(i) => {
                Mod(ItemMod {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    content: i.content.map(|(brace, items)| {
                        (brace, items.lift(|i| folder.fold_item(i)))
                    }),
                    ..i
                })
            }
            ForeignMod(i) => {
                ForeignMod(ItemForeignMod {
                    items: i.items.lift(|foreign_item| {
                        folder.fold_foreign_item(foreign_item)
                    }),
                    ..i
                })
            }
            Ty(i) => {
                Ty(ItemTy {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    ty: i.ty.lift(|ty| folder.fold_ty(ty)),
                    generics: folder.fold_generics(i.generics),
                    ..i
                })
            }
            Enum(i) => {
                Enum(ItemEnum {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    variants: i.variants.lift(|v| folder.fold_variant(v)),
                    generics: folder.fold_generics(i.generics),
                    ..i
                })
            }
            Struct(i) => {
                Struct(ItemStruct {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    data: folder.fold_variant_data(i.data),
                    generics: folder.fold_generics(i.generics),
                    ..i
                })
            }
            Union(i) => {
                Union(ItemUnion {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    data: folder.fold_variant_data(i.data),
                    generics: folder.fold_generics(i.generics),
                    ..i
                })
            }
            Trait(i) => {
                Trait(ItemTrait {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    generics: folder.fold_generics(i.generics),
                    supertraits: i.supertraits.lift(|typb| folder.fold_ty_param_bound(typb)),
                    items: i.items.lift(|ti| folder.fold_trait_item(ti)),
                    ..i
                })
            }
            DefaultImpl(i) => {
                DefaultImpl(ItemDefaultImpl {
                    path: folder.fold_path(i.path),
                    ..i
                })
            }
            Impl(i) => {
                Impl(ItemImpl {
                    generics: folder.fold_generics(i.generics),
                    trait_: i.trait_.map(|(polarity, p, _for)|
                                         (polarity, folder.fold_path(p), _for)),
                    self_ty: i.self_ty.lift(|ty| folder.fold_ty(ty)),
                    items: i.items.lift(|i| folder.fold_impl_item(i)),
                    ..i
                })
            }
            Mac(mac) => Mac(folder.fold_mac(mac)),
        },
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_expr<F: ?Sized + Folder>(folder: &mut F, Expr { node, attrs }: Expr) -> Expr {
    use expr::*;
    use expr::ExprKind::*;

    Expr {
        node: match node {
            Box(e) => {
                Box(ExprBox {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            InPlace(e) => {
                InPlace(ExprInPlace {
                    place: e.place.lift(|e| folder.fold_expr(e)),
                    value: e.value.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Array(e) => {
                Array(ExprArray {
                    exprs: e.exprs.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Call(e) => {
                Call(ExprCall {
                    func: e.func.lift(|e| folder.fold_expr(e)),
                    args: e.args.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            MethodCall(e) => {
                MethodCall(ExprMethodCall {
                    method: folder.fold_ident(e.method),
                    typarams: e.typarams.lift(|t| folder.fold_ty(t)),
                    args: e.args.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Tup(e) => {
                Tup(ExprTup {
                    args: e.args.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Binary(e) => {
                Binary(ExprBinary {
                    left: e.left.lift(|e| folder.fold_expr(e)),
                    right: e.right.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Unary(e) => {
                Unary(ExprUnary {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Lit(lit) => Lit(folder.fold_lit(lit)),
            Cast(e) => {
                Cast(ExprCast {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ty: e.ty.lift(|t| folder.fold_ty(t)),
                    ..e
                })
            }
            Type(e) => {
                Type(ExprType {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ty: e.ty.lift(|t| folder.fold_ty(t)),
                    ..e
                })
            }
            If(e) => {
                If(ExprIf {
                    cond: e.cond.lift(|e| folder.fold_expr(e)),
                    if_true: folder.fold_block(e.if_true),
                    if_false: e.if_false.map(|v| v.lift(|e| folder.fold_expr(e))),
                    ..e
                })
            }
            IfLet(e) => {
                IfLet(ExprIfLet {
                    pat: e.pat.lift(|p| folder.fold_pat(p)),
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    if_true: folder.fold_block(e.if_true),
                    if_false: e.if_false.map(|v| v.lift(|e| folder.fold_expr(e))),
                    ..e
                })
            }
            While(e) => {
                While(ExprWhile {
                    cond: e.cond.lift(|e| folder.fold_expr(e)),
                    body: folder.fold_block(e.body),
                    label: e.label.map(|i| folder.fold_lifetime(i)),
                    ..e
                })
            }
            WhileLet(e) => {
                WhileLet(ExprWhileLet {
                    pat: e.pat.lift(|p| folder.fold_pat(p)),
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    body: folder.fold_block(e.body),
                    label: e.label.map(|i| folder.fold_lifetime(i)),
                    ..e
                })
            }
            ForLoop(e) => {
                ForLoop(ExprForLoop {
                    pat: e.pat.lift(|p| folder.fold_pat(p)),
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    body: folder.fold_block(e.body),
                    label: e.label.map(|i| folder.fold_lifetime(i)),
                    ..e
                })
            }
            Loop(e) => {
                Loop(ExprLoop {
                    body: folder.fold_block(e.body),
                    label: e.label.map(|i| folder.fold_lifetime(i)),
                    ..e
                })
            }
            Match(e) => {
                Match(ExprMatch {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    arms: e.arms.lift(|a: Arm| {
                        Arm {
                            attrs: a.attrs.lift(|a| folder.fold_attribute(a)),
                            pats: a.pats.lift(|p| folder.fold_pat(p)),
                            guard: a.guard.map(|v| v.lift(|e| folder.fold_expr(e))),
                            body: a.body.lift(|e| folder.fold_expr(e)),
                            ..a
                        }
                    }),
                    ..e
                })
            }
            Catch(e) => {
                Catch(ExprCatch {
                    block: folder.fold_block(e.block),
                    ..e
                })
            }
            Closure(e) => {
                Closure(ExprClosure {
                    decl: e.decl.lift(|v| folder.fold_fn_decl(v)),
                    body: e.body.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Block(e) => {
                Block(ExprBlock {
                    block: folder.fold_block(e.block),
                    ..e
                })
            }
            Assign(e) => {
                Assign(ExprAssign {
                    left: e.left.lift(|e| folder.fold_expr(e)),
                    right: e.right.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            AssignOp(e) => {
                AssignOp(ExprAssignOp {
                    left: e.left.lift(|e| folder.fold_expr(e)),
                    right: e.right.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Field(e) => {
                Field(ExprField {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    field: folder.fold_ident(e.field),
                    ..e
                })
            }
            TupField(e) => {
                TupField(ExprTupField {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Index(e) => {
                Index(ExprIndex {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    index: e.index.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Range(e) => {
                Range(ExprRange {
                    from: e.from.map(|v| v.lift(|e| folder.fold_expr(e))),
                    to: e.to.map(|v| v.lift(|e| folder.fold_expr(e))),
                    ..e
                })
            }
            Path(e) => {
                Path(ExprPath {
                    qself: e.qself.map(|v| noop_fold_qself(folder, v)),
                    path: folder.fold_path(e.path),
                })
            }
            AddrOf(e) => {
                AddrOf(ExprAddrOf {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Break(e) => {
                Break(ExprBreak {
                    label: e.label.map(|i| folder.fold_lifetime(i)),
                    expr: e.expr.map(|v| v.lift(|e| folder.fold_expr(e))),
                    ..e
                })
            }
            Continue(e) => {
                Continue(ExprContinue {
                    label: e.label.map(|i| folder.fold_lifetime(i)),
                    ..e
                })
            }
            Ret(e) => {
                Ret(ExprRet {
                    expr: e.expr.map(|v| v.lift(|e| folder.fold_expr(e))),
                    ..e
                })
            }
            Mac(mac) => Mac(folder.fold_mac(mac)),
            Struct(e) => {
                Struct(ExprStruct {
                    path: folder.fold_path(e.path),
                    fields: e.fields.lift(|field: FieldValue| {
                        FieldValue {
                            ident: folder.fold_ident(field.ident),
                            expr: folder.fold_expr(field.expr),
                            attrs: field.attrs.lift(|v| folder.fold_attribute(v)),
                            ..field
                        }
                    }),
                    rest: e.rest.map(|v| v.lift(|e| folder.fold_expr(e))),
                    ..e
                })
            }
            Repeat(e) => {
                Repeat(ExprRepeat {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    amt: e.amt.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Paren(e) => {
                Paren(ExprParen {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Group(e) => {
                Group(ExprGroup {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
            Try(e) => {
                Try(ExprTry {
                    expr: e.expr.lift(|e| folder.fold_expr(e)),
                    ..e
                })
            }
        },
        attrs: attrs.into_iter().map(|a| folder.fold_attribute(a)).collect(),
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_foreign_item<F: ?Sized + Folder>(folder: &mut F,
                                                  item: ForeignItem)
-> ForeignItem{
    use item::*;

    ForeignItem {
        ident: folder.fold_ident(item.ident),
        attrs: item.attrs.into_iter().map(|a| folder.fold_attribute(a)).collect(),
        node: match item.node {
            ForeignItemKind::Fn(item) => {
                ForeignItemKind::Fn(ForeignItemFn {
                    decl: item.decl.lift(|v| folder.fold_fn_decl(v)),
                })
            }
            ForeignItemKind::Static(item) => {
                ForeignItemKind::Static(ForeignItemStatic {
                    ty: item.ty.lift(|v| folder.fold_ty(v)),
                    ..item
                })
            }
        },
        vis: noop_fold_vis(folder, item.vis),
        ..item
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_pat<F: ?Sized + Folder>(folder: &mut F, pat: Pat) -> Pat {
    use Pat::*;
    match pat {
        Wild(b) => Wild(b),
        Ident(p) => {
            Ident(PatIdent {
                ident: folder.fold_ident(p.ident),
                subpat: p.subpat.map(|p| p.lift(|p| folder.fold_pat(p))),
                ..p
            })
        }
        Struct(p) => {
            Struct(PatStruct {
                path: folder.fold_path(p.path),
                fields: p.fields.lift(|field: FieldPat| {
                    FieldPat {
                        ident: folder.fold_ident(field.ident),
                        pat: field.pat.lift(|p| folder.fold_pat(p)),
                        attrs: field.attrs.lift(|a| folder.fold_attribute(a)),
                        ..field
                    }
                }),
                ..p
            })
        }
        TupleStruct(p) => {
            TupleStruct(PatTupleStruct {
                path: folder.fold_path(p.path),
                pat: PatTuple {
                    pats: p.pat.pats.lift(|p| folder.fold_pat(p)),
                    ..p.pat
                },
            })
        }
        Path(p) => {
            Path(PatPath {
                qself: p.qself.map(|v| noop_fold_qself(folder, v)),
                path: folder.fold_path(p.path),
            })
        }
        Tuple(p) => {
            Tuple(PatTuple {
                pats: p.pats.lift(|p| folder.fold_pat(p)),
                ..p
            })
        }
        Box(p) => {
            Box(PatBox {
                pat: p.pat.lift(|p| folder.fold_pat(p)),
                ..p
            })
        }
        Ref(p) => {
            Ref(PatRef {
                pat: p.pat.lift(|p| folder.fold_pat(p)),
                ..p
            })
        }
        Lit(p) => {
            Lit(PatLit {
                expr: p.expr.lift(|e| folder.fold_expr(e)),
            })
        }
        Range(p) => {
            Range(PatRange {
                hi: p.hi.lift(|e| folder.fold_expr(e)),
                lo: p.lo.lift(|e| folder.fold_expr(e)),
                ..p
            })
        }
        Slice(p) => {
            Slice(PatSlice {
                front: p.front.lift(|p| folder.fold_pat(p)),
                middle: p.middle.map(|v| v.lift(|p| folder.fold_pat(p))),
                back: p.back.lift(|p| folder.fold_pat(p)),
                ..p
            })
        }
        Mac(mac) => Mac(folder.fold_mac(mac)),
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_fn_decl<F: ?Sized + Folder>(folder: &mut F, decl: FnDecl)
    -> FnDecl
{
    FnDecl {
        inputs: decl.inputs.lift(|a| {
            use item::*;
            use FnArg::*;
            match a {
                SelfRef(a) => {
                    SelfRef(ArgSelfRef {
                        lifetime: a.lifetime.map(|v| folder.fold_lifetime(v)),
                        ..a
                    })
                }
                SelfValue(a) => SelfValue(a),
                Captured(a) => {
                    Captured(ArgCaptured {
                        pat: folder.fold_pat(a.pat),
                        ty: folder.fold_ty(a.ty),
                        ..a
                    })
                }
                Ignored(ty) => Ignored(folder.fold_ty(ty)),
            }
        }),
        output: folder.fold_fn_ret_ty(decl.output),
        generics: folder.fold_generics(decl.generics),
        ..decl
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_trait_item<F: ?Sized + Folder>(folder: &mut F,
                                                item: TraitItem)
                                                -> TraitItem {
    use item::*;
    use TraitItemKind::*;
    TraitItem {
        attrs: item.attrs.lift(|v| folder.fold_attribute(v)),
        node: match item.node {
            Const(i) => {
                Const(TraitItemConst {
                    ident: folder.fold_ident(i.ident),
                    ty: folder.fold_ty(i.ty),
                    default: i.default.map(|(eq, v)| (eq, folder.fold_expr(v))),
                    ..i
                })
            }
            Method(i) => {
                Method(TraitItemMethod {
                    sig: folder.fold_method_sig(i.sig),
                    default: i.default.map(|v| folder.fold_block(v)),
                    ..i
                })
            }
            Type(i) => {
                Type(TraitItemType {
                    ident: folder.fold_ident(i.ident),
                    bounds: i.bounds.lift(|v| folder.fold_ty_param_bound(v)),
                    default: i.default.map(|(eq, v)| (eq, folder.fold_ty(v))),
                    ..i
                })
            }
            Macro(mac) => Macro(folder.fold_mac(mac)),
        },
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_impl_item<F: ?Sized + Folder>(folder: &mut F, item: ImplItem)
    -> ImplItem
{
    use item::*;
    use ImplItemKind::*;

    ImplItem {
        attrs: item.attrs.lift(|v| folder.fold_attribute(v)),
        node: match item.node {
            Const(i) => {
                Const(ImplItemConst {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    ty: folder.fold_ty(i.ty),
                    expr: folder.fold_expr(i.expr),
                    ..i
                })
            }
            Method(i) => {
                Method(ImplItemMethod {
                    vis: noop_fold_vis(folder, i.vis),
                    sig: folder.fold_method_sig(i.sig),
                    block: folder.fold_block(i.block),
                    ..i
                })
            }
            Type(i) => {
                Type(ImplItemType {
                    vis: noop_fold_vis(folder, i.vis),
                    ident: folder.fold_ident(i.ident),
                    ty: folder.fold_ty(i.ty),
                    ..i
                })
            }
            Macro(mac) => Macro(folder.fold_mac(mac)),
        },
        ..item
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_method_sig<F: ?Sized + Folder>(folder: &mut F, sig: MethodSig)
    -> MethodSig
{
    MethodSig {
        ident: folder.fold_ident(sig.ident),
        decl: folder.fold_fn_decl(sig.decl),
        ..sig
    }

}

#[cfg(feature = "full")]
pub fn noop_fold_stmt<F: ?Sized + Folder>(folder: &mut F, stmt: Stmt) -> Stmt {
    use Stmt::*;
    match stmt {
        Local(local) => Local(local.lift(|l| folder.fold_local(l))),
        Item(item) => Item(item.lift(|v| folder.fold_item(v))),
        Expr(expr) => Expr(expr.lift(|v| folder.fold_expr(v))),
        Semi(expr, t) => Semi(expr.lift(|v| folder.fold_expr(v)), t),
        Mac(mac_stmt) => {
            Mac(mac_stmt.lift(|(mac, style, attrs)| {
                                  (folder.fold_mac(mac),
                                   style,
                                   attrs.lift(|a| folder.fold_attribute(a)))
                              }))
        }
    }

}

#[cfg(feature = "full")]
pub fn noop_fold_local<F: ?Sized + Folder>(folder: &mut F, local: Local)
    -> Local
{
    Local {
        pat: local.pat.lift(|v| folder.fold_pat(v)),
        ty: local.ty.map(|v| v.lift(|t| folder.fold_ty(t))),
        init: local.init.map(|v| v.lift(|e| folder.fold_expr(e))),
        attrs: local.attrs.lift(|a| folder.fold_attribute(a)),
        ..local
    }
}

#[cfg(feature = "full")]
pub fn noop_fold_view_path<F: ?Sized + Folder>(folder: &mut F, view_path: ViewPath) -> ViewPath {
    use item::*;
    use ViewPath::*;
    match view_path {
        Simple(p) => {
            Simple(PathSimple {
                path: folder.fold_path(p.path),
                rename: p.rename.map(|i| folder.fold_ident(i)),
                ..p
            })
        }
        Glob(p) => {
            Glob(PathGlob {
                path: folder.fold_path(p.path),
                ..p
            })
        }
        List(p) => {
            List(PathList {
                path: folder.fold_path(p.path),
                items: p.items.lift(|item: PathListItem| {
                    PathListItem {
                        name: folder.fold_ident(item.name),
                        rename: item.rename.map(|i| folder.fold_ident(i)),
                        ..item
                    }
                }),
                ..p
            })
        }
    }
}
