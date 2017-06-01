// Adapted from libsyntax.

//! AST walker. Each overridden visit method has full control over what
//! happens with its node, it can do its own traversal of the node's children,
//! call `visit::walk_*` to apply the default traversal algorithm, or prevent
//! deeper traversal by doing nothing.
//!
//! Note: it is an important invariant that the default visitor walks the body
//! of a function in "execution order" (more concretely, reverse post-order
//! with respect to the CFG implied by the AST), meaning that if AST node A may
//! execute before AST node B, then A is visited first.  The borrow checker in
//! particular relies on this property.
//!
//! Note: walking an AST before macro expansion is probably a bad idea. For
//! instance, a walker looking for item names in a module will miss all of
//! those that are created by the expansion of a macro.

use super::*;

/// Each method of the Visitor trait is a hook to be potentially
/// overridden.  Each method's default implementation recursively visits
/// the substructure of the input via the corresponding `walk` method;
/// e.g. the `visit_mod` method by default calls `visit::walk_mod`.
///
/// If you want to ensure that your code handles every variant
/// explicitly, you need to override each method.  (And you also need
/// to monitor future changes to `Visitor` in case a new method with a
/// new default implementation gets introduced.)
pub trait Visitor: Sized {
    fn visit_ident(&mut self, _ident: &Ident) {}
    fn visit_derive_input(&mut self, derive_input: &DeriveInput) {
        walk_derive_input(self, derive_input)
    }
    fn visit_ty(&mut self, ty: &Ty) {
        walk_ty(self, ty)
    }
    fn visit_generics(&mut self, generics: &Generics) {
        walk_generics(self, generics)
    }
    fn visit_ty_param_bound(&mut self, bound: &TyParamBound) {
        walk_ty_param_bound(self, bound)
    }
    fn visit_poly_trait_ref(&mut self, trait_ref: &PolyTraitRef, modifier: &TraitBoundModifier) {
        walk_poly_trait_ref(self, trait_ref, modifier)
    }
    fn visit_variant_data(&mut self, data: &VariantData, _ident: &Ident, _generics: &Generics) {
        walk_variant_data(self, data)
    }
    fn visit_field(&mut self, field: &Field) {
        walk_field(self, field)
    }
    fn visit_variant(&mut self, variant: &Variant, generics: &Generics) {
        walk_variant(self, variant, generics)
    }
    fn visit_lifetime(&mut self, _lifetime: &Lifetime) {}
    fn visit_lifetime_def(&mut self, lifetime: &LifetimeDef) {
        walk_lifetime_def(self, lifetime)
    }
    fn visit_path(&mut self, path: &Path) {
        walk_path(self, path)
    }
    fn visit_path_segment(&mut self, path_segment: &PathSegment) {
        walk_path_segment(self, path_segment)
    }
    fn visit_path_parameters(&mut self, path_parameters: &PathParameters) {
        walk_path_parameters(self, path_parameters)
    }
    fn visit_assoc_type_binding(&mut self, type_binding: &TypeBinding) {
        walk_assoc_type_binding(self, type_binding)
    }
    fn visit_attribute(&mut self, _attr: &Attribute) {}
    fn visit_fn_ret_ty(&mut self, ret_ty: &FunctionRetTy) {
        walk_fn_ret_ty(self, ret_ty)
    }
    fn visit_lit(&mut self, _lit: &Lit) {}
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_mac(&mut self, mac: &Mac) {
        walk_mac(self, mac);
    }

    #[cfg(feature = "full")]
    fn visit_file(&mut self, file: &File) {
        walk_file(self, file);
    }
    #[cfg(feature = "full")]
    fn visit_item(&mut self, item: &Item) {
        walk_item(self, item);
    }
    #[cfg(feature = "full")]
    fn visit_foreign_item(&mut self, foreign_item: &ForeignItem) {
        walk_foreign_item(self, foreign_item);
    }
    #[cfg(feature = "full")]
    fn visit_pat(&mut self, pat: &Pat) {
        walk_pat(self, pat);
    }
    #[cfg(feature = "full")]
    fn visit_fn_decl(&mut self, fn_decl: &FnDecl) {
        walk_fn_decl(self, fn_decl);
    }
    #[cfg(feature = "full")]
    fn visit_trait_item(&mut self, trait_item: &TraitItem) {
        walk_trait_item(self, trait_item);
    }
    #[cfg(feature = "full")]
    fn visit_impl_item(&mut self, impl_item: &ImplItem) {
        walk_impl_item(self, impl_item);
    }
    #[cfg(feature = "full")]
    fn visit_method_sig(&mut self, method_sig: &MethodSig) {
        walk_method_sig(self, method_sig);
    }
    #[cfg(feature = "full")]
    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }
    #[cfg(feature = "full")]
    fn visit_local(&mut self, local: &Local) {
        walk_local(self, local);
    }
    #[cfg(feature = "full")]
    fn visit_view_path(&mut self, view_path: &ViewPath) {
        walk_view_path(self, view_path);
    }
}

macro_rules! walk_list {
    ($visitor:expr, $method:ident, $list:expr $(, $extra_args:expr)*) => {
        for elem in $list {
            $visitor.$method(elem $(, $extra_args)*)
        }
    };
}

pub fn walk_opt_ident<V: Visitor>(visitor: &mut V, opt_ident: &Option<Ident>) {
    if let Some(ref ident) = *opt_ident {
        visitor.visit_ident(ident);
    }
}

pub fn walk_lifetime_def<V: Visitor>(visitor: &mut V, lifetime_def: &LifetimeDef) {
    visitor.visit_lifetime(&lifetime_def.lifetime);
    walk_list!(visitor, visit_lifetime, lifetime_def.bounds.items());
}

pub fn walk_poly_trait_ref<V>(visitor: &mut V, trait_ref: &PolyTraitRef, _: &TraitBoundModifier)
    where V: Visitor
{
    if let Some(ref bl) = trait_ref.bound_lifetimes {
        walk_list!(visitor, visit_lifetime_def, bl.lifetimes.items());
    }
    visitor.visit_path(&trait_ref.trait_ref);
}

pub fn walk_derive_input<V: Visitor>(visitor: &mut V, derive_input: &DeriveInput) {
    visitor.visit_ident(&derive_input.ident);
    visitor.visit_generics(&derive_input.generics);
    match derive_input.body {
        Body::Enum(ref data) => {
            walk_list!(visitor, visit_variant,
                       data.variants.items(),
                       &derive_input.generics);
        }
        Body::Struct(ref data) => {
            visitor.visit_variant_data(&data.data, &derive_input.ident, &derive_input.generics);
        }
    }
    walk_list!(visitor, visit_attribute, &derive_input.attrs);
}

pub fn walk_variant<V>(visitor: &mut V, variant: &Variant, generics: &Generics)
    where V: Visitor
{
    visitor.visit_ident(&variant.ident);
    visitor.visit_variant_data(&variant.data, &variant.ident, generics);
    walk_list!(visitor, visit_attribute, &variant.attrs);
}

pub fn walk_ty<V: Visitor>(visitor: &mut V, ty: &Ty) {
    use ty::*;

    match *ty {
        Ty::Slice(TySlice { ref ty, .. }) |
        Ty::Group(TyGroup { ref ty, .. }) |
        Ty::Paren(TyParen { ref ty, .. }) => visitor.visit_ty(ty),
        Ty::Ptr(TyPtr { ref ty, .. }) => visitor.visit_ty(&ty.ty),
        Ty::Rptr(TyRptr { ref lifetime, ref ty, .. }) => {
            walk_list!(visitor, visit_lifetime, lifetime);
            visitor.visit_ty(&ty.ty)
        }
        Ty::Never(_) | Ty::Infer(_) => {}
        Ty::Tup(TyTup { ref tys, .. }) => {
            walk_list!(visitor, visit_ty, tys.items());
        }
        Ty::BareFn(TyBareFn { ref ty }) => {
            if let Some(ref l) = ty.lifetimes {
                walk_list!(visitor, visit_lifetime_def, l.lifetimes.items());
            }
            for argument in ty.inputs.items() {
                if let Some((ref name, _)) = argument.name {
                    visitor.visit_ident(name);
                }
                visitor.visit_ty(&argument.ty)
            }
            visitor.visit_fn_ret_ty(&ty.output)
        }
        Ty::Path(TyPath { ref qself, ref path }) => {
            if let Some(ref qself) = *qself {
                visitor.visit_ty(&qself.ty);
            }
            visitor.visit_path(path);
        }
        Ty::Array(TyArray { ref ty, ref amt, .. }) => {
            visitor.visit_ty(ty);
            visitor.visit_expr(amt);
        }
        Ty::TraitObject(TyTraitObject { ref bounds, .. })  |
        Ty::ImplTrait(TyImplTrait { ref bounds, .. }) => {
            walk_list!(visitor, visit_ty_param_bound, bounds.items());
        }
        Ty::Mac(ref mac) => {
            visitor.visit_mac(mac);
        }
    }
}

pub fn walk_path<V: Visitor>(visitor: &mut V, path: &Path) {
    for segment in path.segments.items() {
        visitor.visit_path_segment(segment);
    }
}

pub fn walk_path_segment<V: Visitor>(visitor: &mut V, segment: &PathSegment) {
    visitor.visit_ident(&segment.ident);
    visitor.visit_path_parameters(&segment.parameters);
}

pub fn walk_path_parameters<V>(visitor: &mut V, path_parameters: &PathParameters)
    where V: Visitor
{
    match *path_parameters {
        PathParameters::None => {}
        PathParameters::AngleBracketed(ref data) => {
            walk_list!(visitor, visit_ty, data.types.items());
            walk_list!(visitor, visit_lifetime, data.lifetimes.items());
            walk_list!(visitor, visit_assoc_type_binding, data.bindings.items());
        }
        PathParameters::Parenthesized(ref data) => {
            walk_list!(visitor, visit_ty, data.inputs.items());
            visitor.visit_fn_ret_ty(&data.output);
        }
    }
}

pub fn walk_assoc_type_binding<V: Visitor>(visitor: &mut V, type_binding: &TypeBinding) {
    visitor.visit_ident(&type_binding.ident);
    visitor.visit_ty(&type_binding.ty);
}

pub fn walk_ty_param_bound<V: Visitor>(visitor: &mut V, bound: &TyParamBound) {
    match *bound {
        TyParamBound::Trait(ref ty, ref modifier) => {
            visitor.visit_poly_trait_ref(ty, modifier);
        }
        TyParamBound::Region(ref lifetime) => {
            visitor.visit_lifetime(lifetime);
        }
    }
}

pub fn walk_generics<V: Visitor>(visitor: &mut V, generics: &Generics) {
    for param in generics.ty_params.items() {
        visitor.visit_ident(&param.ident);
        walk_list!(visitor, visit_ty_param_bound, param.bounds.items());
        walk_list!(visitor, visit_ty, &param.default);
    }
    walk_list!(visitor, visit_lifetime_def, generics.lifetimes.items());
    for predicate in generics.where_clause.predicates.items() {
        match *predicate {
            WherePredicate::BoundPredicate(WhereBoundPredicate { ref bounded_ty,
                                                                 ref bounds,
                                                                 ref bound_lifetimes,
                                                                 .. }) => {
                visitor.visit_ty(bounded_ty);
                walk_list!(visitor, visit_ty_param_bound, bounds.items());
                if let Some(ref l) = *bound_lifetimes {
                    walk_list!(visitor, visit_lifetime_def, l.lifetimes.items());
                }
            }
            WherePredicate::RegionPredicate(WhereRegionPredicate { ref lifetime,
                                                                   ref bounds,
                                                                   .. }) => {
                visitor.visit_lifetime(lifetime);
                walk_list!(visitor, visit_lifetime, bounds.items());
            }
            WherePredicate::EqPredicate(WhereEqPredicate { ref lhs_ty, ref rhs_ty, .. }) => {
                visitor.visit_ty(lhs_ty);
                visitor.visit_ty(rhs_ty);
            }
        }
    }
}

pub fn walk_fn_ret_ty<V: Visitor>(visitor: &mut V, ret_ty: &FunctionRetTy) {
    if let FunctionRetTy::Ty(ref output_ty, _) = *ret_ty {
        visitor.visit_ty(output_ty)
    }
}

pub fn walk_variant_data<V: Visitor>(visitor: &mut V, data: &VariantData) {
    let fields = match *data {
        VariantData::Struct(ref f, _) |
        VariantData::Tuple(ref f, _) => f,
        VariantData::Unit => return,
    };
    walk_list!(visitor, visit_field, fields.items());
}

pub fn walk_field<V: Visitor>(visitor: &mut V, field: &Field) {
    walk_opt_ident(visitor, &field.ident);
    visitor.visit_ty(&field.ty);
    walk_list!(visitor, visit_attribute, &field.attrs);
}

pub fn walk_mac<V: Visitor>(visitor: &mut V, mac: &Mac) {
    visitor.visit_path(&mac.path);
}

#[cfg(feature = "full")]
pub fn walk_file<V: Visitor>(visitor: &mut V, file: &File) {
    walk_list!(visitor, visit_attribute, &file.attrs);
    walk_list!(visitor, visit_item, &file.items);
}

#[cfg(feature = "full")]
pub fn walk_item<V: Visitor>(visitor: &mut V, item: &Item) {
    use item::*;

    walk_list!(visitor, visit_attribute, &item.attrs);
    match item.node {
        ItemKind::ExternCrate(ItemExternCrate { ref ident, ref rename, .. }) => {
            visitor.visit_ident(ident);
            if let Some((_, ref id)) = *rename {
                visitor.visit_ident(id);
            }
        }
        ItemKind::Use(ItemUse { ref path, .. }) => {
            visitor.visit_view_path(path);
        }
        ItemKind::Static(ItemStatic { ref ident, ref ty, ref expr, .. }) |
        ItemKind::Const(ItemConst { ref ident, ref ty, ref expr, .. }) => {
            visitor.visit_ident(ident);
            visitor.visit_ty(ty);
            visitor.visit_expr(expr);
        }
        ItemKind::Fn(ItemFn { ref ident, ref decl, ref block, ..  }) => {
            visitor.visit_ident(ident);
            visitor.visit_fn_decl(decl);
            walk_list!(visitor, visit_stmt, &block.stmts);
        }
        ItemKind::Mod(ItemMod { ref ident, ref content, .. }) => {
            visitor.visit_ident(ident);
            if let Some((_, ref items)) = *content {
                walk_list!(visitor, visit_item, items);
            }
        }
        ItemKind::ForeignMod(ItemForeignMod { ref items, .. }) => {
            walk_list!(visitor, visit_foreign_item, items);
        }
        ItemKind::Ty(ItemTy { ref ident, ref ty, ref generics, .. }) => {
            visitor.visit_ident(ident);
            visitor.visit_ty(ty);
            visitor.visit_generics(generics);
        }
        ItemKind::Enum(ItemEnum { ref ident, ref variants, ref generics, ..}) => {
            visitor.visit_ident(ident);
            walk_list!(visitor, visit_variant, variants.items(), generics);
        }
        ItemKind::Struct(ItemStruct { ref ident, ref data, ref generics, .. }) |
        ItemKind::Union(ItemUnion { ref ident, ref data, ref generics, .. }) => {
            visitor.visit_ident(ident);
            visitor.visit_variant_data(data, ident, generics);
        }
        ItemKind::Trait(ItemTrait {
            ref ident,
            ref generics,
            ref supertraits,
            ref items,
            ..
        }) => {
            visitor.visit_ident(ident);
            visitor.visit_generics(generics);
            walk_list!(visitor, visit_ty_param_bound, supertraits.items());
            walk_list!(visitor, visit_trait_item, items);
        }
        ItemKind::DefaultImpl(ItemDefaultImpl { ref path, .. }) => {
            visitor.visit_path(path);
        }
        ItemKind::Impl(ItemImpl {
            ref generics,
            ref trait_,
            ref self_ty,
            ref items,
            ..
        }) => {
            visitor.visit_generics(generics);
            if let Some((_, ref path, _)) = *trait_ {
                visitor.visit_path(path);
            }
            visitor.visit_ty(self_ty);
            walk_list!(visitor, visit_impl_item, items);
        }
        ItemKind::Mac(ref mac) => visitor.visit_mac(mac),
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]
pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    use expr::*;
    use expr::ExprKind::*;

    walk_list!(visitor, visit_attribute, &expr.attrs);
    match expr.node {
        #[cfg(feature = "full")]
        InPlace(ExprInPlace { ref place, ref value, .. }) => {
            visitor.visit_expr(place);
            visitor.visit_expr(value);
        }
        Call(ExprCall { ref func, ref args, .. }) => {
            visitor.visit_expr(func);
            walk_list!(visitor, visit_expr, args.items());
        }
        #[cfg(feature = "full")]
        MethodCall(ExprMethodCall { ref method, ref typarams, ref args, .. }) => {
            visitor.visit_ident(method);
            walk_list!(visitor, visit_ty, typarams.items());
            walk_list!(visitor, visit_expr, args.items());
        }
        #[cfg(feature = "full")]
        Array(ExprArray { ref exprs, ..}) |
        Tup(ExprTup { args: ref exprs, .. }) => {
            walk_list!(visitor, visit_expr, exprs.items());
        }
        Lit(ref lit) => {
            visitor.visit_lit(lit);
        }
        Cast(ExprCast { ref expr, ref ty, .. }) |
        Type(ExprType { ref expr, ref ty, .. }) => {
            visitor.visit_expr(expr);
            visitor.visit_ty(ty);
        }
        #[cfg(feature = "full")]
        If(ExprIf { ref cond, ref if_true, ref if_false, .. }) => {
            visitor.visit_expr(cond);
            walk_list!(visitor, visit_stmt, &if_true.stmts);
            if let Some(ref alt) = *if_false {
                visitor.visit_expr(alt);
            }
        }
        #[cfg(feature = "full")]
        IfLet(ExprIfLet { ref pat, ref expr, ref if_true, ref if_false, .. }) => {
            visitor.visit_pat(pat);
            visitor.visit_expr(expr);
            walk_list!(visitor, visit_stmt, &if_true.stmts);
            if let Some(ref alt) = *if_false {
                visitor.visit_expr(alt);
            }
        }
        #[cfg(feature = "full")]
        While(ExprWhile { ref cond, ref body, ref label, .. }) => {
            visitor.visit_expr(cond);
            walk_list!(visitor, visit_stmt, &body.stmts);
            if let Some(ref label) = *label {
                visitor.visit_lifetime(label);
            }
        }
        #[cfg(feature = "full")]
        WhileLet(ExprWhileLet { ref pat, ref expr, ref body, ref label, .. }) |
        ForLoop(ExprForLoop { ref pat, ref expr, ref body, ref label, .. }) => {
            visitor.visit_pat(pat);
            visitor.visit_expr(expr);
            walk_list!(visitor, visit_stmt, &body.stmts);
            if let Some(ref label) = *label {
                visitor.visit_lifetime(label);
            }
        }
        #[cfg(feature = "full")]
        Loop(ExprLoop { ref body, ref label, .. }) => {
            walk_list!(visitor, visit_stmt, &body.stmts);
            if let Some(ref label) = *label {
                visitor.visit_lifetime(label);
            }
        }
        #[cfg(feature = "full")]
        Match(ExprMatch { ref expr, ref arms, .. }) => {
            visitor.visit_expr(expr);
            for &Arm { ref attrs, ref pats, ref guard, ref body, .. } in arms {
                walk_list!(visitor, visit_attribute, attrs);
                walk_list!(visitor, visit_pat, pats.items());
                if let Some(ref guard) = *guard {
                    visitor.visit_expr(guard);
                }
                visitor.visit_expr(body);
            }
        }
        #[cfg(feature = "full")]
        Closure(ExprClosure { ref decl, ref body, .. }) => {
            visitor.visit_fn_decl(decl);
            visitor.visit_expr(body);
        }
        #[cfg(feature = "full")]
        Catch(ExprCatch { ref block, .. }) |
        Block(ExprBlock { ref block, .. }) => {
            walk_list!(visitor, visit_stmt, &block.stmts);
        }
        Binary(ExprBinary { ref left, ref right, .. }) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        #[cfg(feature = "full")]
        Assign(ExprAssign { ref left, ref right, .. }) |
        AssignOp(ExprAssignOp { ref left, ref right, .. }) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        #[cfg(feature = "full")]
        Field(ExprField { ref expr, ref field, .. }) => {
            visitor.visit_expr(expr);
            visitor.visit_ident(field);
        }
        Index(ExprIndex { ref expr, ref index, .. }) => {
            visitor.visit_expr(expr);
            visitor.visit_expr(index);
        }
        #[cfg(feature = "full")]
        Range(ExprRange { ref from, ref to, .. }) => {
            if let Some(ref start) = *from {
                visitor.visit_expr(start);
            }
            if let Some(ref end) = *to {
                visitor.visit_expr(end);
            }
        }
        Path(ExprPath { ref qself, ref path }) => {
            if let Some(ref qself) = *qself {
                visitor.visit_ty(&qself.ty);
            }
            visitor.visit_path(path);
        }
        #[cfg(feature = "full")]
        Break(ExprBreak { ref label, ref expr, .. }) => {
            if let Some(ref label) = *label {
                visitor.visit_lifetime(label);
            }
            if let Some(ref expr) = *expr {
                visitor.visit_expr(expr);
            }
        }
        #[cfg(feature = "full")]
        Continue(ExprContinue { ref label, .. }) => {
            if let Some(ref label) = *label {
                visitor.visit_lifetime(label);
            }
        }
        #[cfg(feature = "full")]
        Ret(ExprRet { ref expr, .. }) => {
            if let Some(ref expr) = *expr {
                visitor.visit_expr(expr);
            }
        }
        Mac(ref mac) => {
            visitor.visit_mac(mac);
        }
        #[cfg(feature = "full")]
        Struct(ExprStruct { ref path, ref fields, ref rest, .. }) => {
            visitor.visit_path(path);
            for &FieldValue { ref ident, ref expr, .. } in fields.items() {
                visitor.visit_ident(ident);
                visitor.visit_expr(expr);
            }
            if let Some(ref base) = *rest {
                visitor.visit_expr(base);
            }
        }
        #[cfg(feature = "full")]
        Repeat(ExprRepeat { ref expr, ref amt, .. }) => {
            visitor.visit_expr(expr);
            visitor.visit_expr(amt);
        }
        #[cfg(feature = "full")]
        Yield(ExprYield { ref expr, .. }) => {
            if let Some(ref expr) = *expr {
                visitor.visit_expr(expr);
            }
        }
        #[cfg(feature = "full")]
        TupField(ExprTupField { ref expr, .. }) |
        Unary(ExprUnary { ref expr, .. }) |
        Box(ExprBox { ref expr, .. }) |
        AddrOf(ExprAddrOf { ref expr, .. }) |
        Try(ExprTry { ref expr, .. }) => {
            visitor.visit_expr(expr);
        }
        Paren(ExprParen { ref expr, .. }) |
        Group(ExprGroup { ref expr, .. }) => {
            visitor.visit_expr(expr);
        }
        #[cfg(not(feature = "full"))]
        _ => unreachable!(),
    }
}

#[cfg(feature = "full")]
pub fn walk_foreign_item<V: Visitor>(visitor: &mut V, foreign_item: &ForeignItem) {
    use item::*;

    visitor.visit_ident(&foreign_item.ident);
    walk_list!(visitor, visit_attribute, &foreign_item.attrs);
    match foreign_item.node {
        ForeignItemKind::Fn(ForeignItemFn { ref decl, .. }) => {
            visitor.visit_fn_decl(decl);
        }
        ForeignItemKind::Static(ForeignItemStatic { ref ty, .. }) => {
            visitor.visit_ty(ty);
        }
    }
}

#[cfg(feature = "full")]
pub fn walk_pat<V: Visitor>(visitor: &mut V, pat: &Pat) {
    use expr::*;

    match *pat {
        Pat::Wild(_) => {}
        Pat::Ident(PatIdent { ref ident, ref subpat, .. }) => {
            visitor.visit_ident(ident);
            if let Some(ref pat) = *subpat {
                visitor.visit_pat(pat);
            }
        }
        Pat::Struct(PatStruct { ref path, ref fields, .. }) => {
            visitor.visit_path(path);
            for &FieldPat { ref ident, ref pat, .. } in fields.items() {
                visitor.visit_ident(ident);
                visitor.visit_pat(pat);
            }
        }
        Pat::TupleStruct(PatTupleStruct { ref path, ref pat, .. }) => {
            visitor.visit_path(path);
            walk_list!(visitor, visit_pat, pat.pats.items());
        }
        Pat::Path(PatPath { ref qself, ref path }) => {
            if let Some(ref qself) = *qself {
                visitor.visit_ty(&qself.ty);
            }
            visitor.visit_path(path);
        }
        Pat::Tuple(PatTuple { ref pats, .. }) => {
            walk_list!(visitor, visit_pat, pats.items());
        }
        Pat::Box(PatBox { ref pat, .. }) |
        Pat::Ref(PatRef { ref pat, .. }) => {
            visitor.visit_pat(pat);
        }
        Pat::Lit(PatLit { ref expr }) => {
            visitor.visit_expr(expr);
        }
        Pat::Range(PatRange { ref lo, ref hi, .. }) => {
            visitor.visit_expr(lo);
            visitor.visit_expr(hi);
        }
        Pat::Slice(PatSlice { ref front, ref middle, ref back, .. }) => {
            walk_list!(visitor, visit_pat, front.items());
            if let Some(ref mid) = *middle {
                visitor.visit_pat(mid);
            }
            walk_list!(visitor, visit_pat, back.items());
        }
        Pat::Mac(ref mac) => {
            visitor.visit_mac(mac);
        }
    }
}

#[cfg(feature = "full")]
pub fn walk_fn_decl<V: Visitor>(visitor: &mut V, fn_decl: &FnDecl) {
    use item::*;

    for input in fn_decl.inputs.items() {
        match *input {
            FnArg::SelfRef(_) |
            FnArg::SelfValue(_) => {}
            FnArg::Captured(ArgCaptured { ref pat, ref ty, .. }) => {
                visitor.visit_pat(pat);
                visitor.visit_ty(ty);
            }
            FnArg::Ignored(ref ty) => {
                visitor.visit_ty(ty);
            }
        }
    }
    visitor.visit_generics(&fn_decl.generics);
    visitor.visit_fn_ret_ty(&fn_decl.output);
}

#[cfg(feature = "full")]
pub fn walk_trait_item<V: Visitor>(visitor: &mut V, trait_item: &TraitItem) {
    use item::*;

    walk_list!(visitor, visit_attribute, &trait_item.attrs);
    match trait_item.node {
        TraitItemKind::Const(TraitItemConst { ref ident, ref ty, ref default, ..}) => {
            visitor.visit_ident(ident);
            visitor.visit_ty(ty);
            if let Some((_, ref expr)) = *default {
                visitor.visit_expr(expr);
            }
        }
        TraitItemKind::Method(TraitItemMethod { ref sig, ref default, .. }) => {
            visitor.visit_method_sig(sig);
            if let Some(ref block) = *default {
                walk_list!(visitor, visit_stmt, &block.stmts);
            }
        }
        TraitItemKind::Type(TraitItemType { ref ident, ref bounds, ref default, .. }) => {
            visitor.visit_ident(ident);
            walk_list!(visitor, visit_ty_param_bound, bounds.items());
            if let Some((_, ref ty)) = *default {
                visitor.visit_ty(ty);
            }
        }
        TraitItemKind::Macro(ref mac) => {
            visitor.visit_mac(mac);
        }
    }
}

#[cfg(feature = "full")]
pub fn walk_impl_item<V: Visitor>(visitor: &mut V, impl_item: &ImplItem) {
    use item::*;

    walk_list!(visitor, visit_attribute, &impl_item.attrs);
    match impl_item.node {
        ImplItemKind::Const(ImplItemConst { ref ident, ref ty, ref expr, .. }) => {
            visitor.visit_ident(ident);
            visitor.visit_ty(ty);
            visitor.visit_expr(expr);
        }
        ImplItemKind::Method(ImplItemMethod { ref sig, ref block, .. }) => {
            visitor.visit_method_sig(sig);
            walk_list!(visitor, visit_stmt, &block.stmts);
        }
        ImplItemKind::Type(ImplItemType { ref ident, ref ty, .. }) => {
            visitor.visit_ident(ident);
            visitor.visit_ty(ty);
        }
        ImplItemKind::Macro(ref mac) => {
            visitor.visit_mac(mac);
        }
    }
}

#[cfg(feature = "full")]
pub fn walk_method_sig<V: Visitor>(visitor: &mut V, method_sig: &MethodSig) {
    visitor.visit_fn_decl(&method_sig.decl);
}

#[cfg(feature = "full")]
pub fn walk_stmt<V: Visitor>(visitor: &mut V, stmt: &Stmt) {
    match *stmt {
        Stmt::Local(ref local) => {
            visitor.visit_local(local);
        }
        Stmt::Item(ref item) => {
            visitor.visit_item(item);
        }
        Stmt::Expr(ref expr) |
        Stmt::Semi(ref expr, _) => {
            visitor.visit_expr(expr);
        }
        Stmt::Mac(ref details) => {
            let (ref mac, _, ref attrs) = **details;
            visitor.visit_mac(mac);
            walk_list!(visitor, visit_attribute, attrs);
        }
    }
}

#[cfg(feature = "full")]
pub fn walk_local<V: Visitor>(visitor: &mut V, local: &Local) {
    visitor.visit_pat(&local.pat);
    if let Some(ref ty) = local.ty {
        visitor.visit_ty(ty);
    }
    if let Some(ref init) = local.init {
        visitor.visit_expr(init);
    }
    walk_list!(visitor, visit_attribute, &local.attrs);
}

#[cfg(feature = "full")]
pub fn walk_view_path<V: Visitor>(visitor: &mut V, view_path: &ViewPath) {
    use item::*;
    match *view_path {
        ViewPath::Simple(PathSimple { ref path, ref rename, .. }) => {
            visitor.visit_path(path);
            walk_opt_ident(visitor, rename);
        }
        ViewPath::Glob(PathGlob { ref path, .. }) => {
            visitor.visit_path(path);
        }
        ViewPath::List(PathList { ref path, ref items, .. }) => {
            visitor.visit_path(path);
            for &PathListItem { ref name, ref rename, .. } in items.items() {
                visitor.visit_ident(name);
                walk_opt_ident(visitor, rename);
            }
        }
    }
}
