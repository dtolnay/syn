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
    fn visit_const_expr(&mut self, expr: &ConstExpr) {
        walk_const_expr(self, expr)
    }
    fn visit_lit(&mut self, _lit: &Lit) {}

    fn visit_mac(&mut self, mac: &Mac) {
        walk_mac(self, mac);
    }

    #[cfg(feature = "full")]
    fn visit_crate(&mut self, _crate: &Crate) {
        walk_crate(self, _crate);
    }
    #[cfg(feature = "full")]
    fn visit_item(&mut self, item: &Item) {
        walk_item(self, item);
    }
    #[cfg(feature = "full")]
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
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
    walk_list!(visitor, visit_lifetime, &lifetime_def.bounds);
}

pub fn walk_poly_trait_ref<V>(visitor: &mut V, trait_ref: &PolyTraitRef, _: &TraitBoundModifier)
    where V: Visitor
{
    walk_list!(visitor, visit_lifetime_def, &trait_ref.bound_lifetimes);
    visitor.visit_path(&trait_ref.trait_ref);
}

pub fn walk_derive_input<V: Visitor>(visitor: &mut V, derive_input: &DeriveInput) {
    visitor.visit_ident(&derive_input.ident);
    visitor.visit_generics(&derive_input.generics);
    match derive_input.body {
        Body::Enum(ref variants) => {
            walk_list!(visitor, visit_variant, variants, &derive_input.generics);
        }
        Body::Struct(ref variant_data) => {
            visitor.visit_variant_data(variant_data, &derive_input.ident, &derive_input.generics);
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
        Ty::Slice(TySlice { ref ty }) |
        Ty::Paren(TyParen { ref ty }) => visitor.visit_ty(ty),
        Ty::Ptr(TyPtr { ref ty }) => visitor.visit_ty(&ty.ty),
        Ty::Rptr(TyRptr { ref lifetime, ref ty }) => {
            walk_list!(visitor, visit_lifetime, lifetime);
            visitor.visit_ty(&ty.ty)
        }
        Ty::Never(_) | Ty::Infer(_) => {}
        Ty::Tup(TyTup { ref tys }) => {
            walk_list!(visitor, visit_ty, tys);
        }
        Ty::BareFn(TyBareFn { ref ty }) => {
            walk_list!(visitor, visit_lifetime_def, &ty.lifetimes);
            for argument in &ty.inputs {
                walk_opt_ident(visitor, &argument.name);
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
        Ty::Array(TyArray { ref ty, ref amt }) => {
            visitor.visit_ty(ty);
            visitor.visit_const_expr(amt);
        }
        Ty::TraitObject(TyTraitObject { ref bounds })  |
        Ty::ImplTrait(TyImplTrait { ref bounds }) => {
            walk_list!(visitor, visit_ty_param_bound, bounds);
        }
        Ty::Mac(ref mac) => {
            visitor.visit_mac(mac);
        }
    }
}

pub fn walk_path<V: Visitor>(visitor: &mut V, path: &Path) {
    for segment in &path.segments {
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
        PathParameters::AngleBracketed(ref data) => {
            walk_list!(visitor, visit_ty, &data.types);
            walk_list!(visitor, visit_lifetime, &data.lifetimes);
            walk_list!(visitor, visit_assoc_type_binding, &data.bindings);
        }
        PathParameters::Parenthesized(ref data) => {
            walk_list!(visitor, visit_ty, &data.inputs);
            walk_list!(visitor, visit_ty, &data.output);
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
    for param in &generics.ty_params {
        visitor.visit_ident(&param.ident);
        walk_list!(visitor, visit_ty_param_bound, &param.bounds);
        walk_list!(visitor, visit_ty, &param.default);
    }
    walk_list!(visitor, visit_lifetime_def, &generics.lifetimes);
    for predicate in &generics.where_clause.predicates {
        match *predicate {
            WherePredicate::BoundPredicate(WhereBoundPredicate { ref bounded_ty,
                                                                 ref bounds,
                                                                 ref bound_lifetimes,
                                                                 .. }) => {
                visitor.visit_ty(bounded_ty);
                walk_list!(visitor, visit_ty_param_bound, bounds);
                walk_list!(visitor, visit_lifetime_def, bound_lifetimes);
            }
            WherePredicate::RegionPredicate(WhereRegionPredicate { ref lifetime,
                                                                   ref bounds,
                                                                   .. }) => {
                visitor.visit_lifetime(lifetime);
                walk_list!(visitor, visit_lifetime, bounds);
            }
            WherePredicate::EqPredicate(WhereEqPredicate { ref lhs_ty, ref rhs_ty, .. }) => {
                visitor.visit_ty(lhs_ty);
                visitor.visit_ty(rhs_ty);
            }
        }
    }
}

pub fn walk_fn_ret_ty<V: Visitor>(visitor: &mut V, ret_ty: &FunctionRetTy) {
    if let FunctionRetTy::Ty(ref output_ty) = *ret_ty {
        visitor.visit_ty(output_ty)
    }
}

pub fn walk_variant_data<V: Visitor>(visitor: &mut V, data: &VariantData) {
    walk_list!(visitor, visit_field, data.fields());
}

pub fn walk_field<V: Visitor>(visitor: &mut V, field: &Field) {
    walk_opt_ident(visitor, &field.ident);
    visitor.visit_ty(&field.ty);
    walk_list!(visitor, visit_attribute, &field.attrs);
}

pub fn walk_const_expr<V: Visitor>(visitor: &mut V, len: &ConstExpr) {
    use constant::*;
    use constant::ConstExpr::*;

    match *len {
        Call(ConstCall { ref func, ref args }) => {
            visitor.visit_const_expr(func);
            walk_list!(visitor, visit_const_expr, args);
        }
        Binary(ConstBinary { ref left, ref right, .. }) => {
            visitor.visit_const_expr(left);
            visitor.visit_const_expr(right);
        }
        Lit(ref lit) => {
            visitor.visit_lit(lit);
        }
        Cast(ConstCast { ref expr, ref ty }) => {
            visitor.visit_const_expr(expr);
            visitor.visit_ty(ty);
        }
        Path(ref path) => {
            visitor.visit_path(path);
        }
        Index(ConstIndex { ref expr, ref index }) => {
            visitor.visit_const_expr(expr);
            visitor.visit_const_expr(index);
        }
        Unary(ConstUnary { ref expr, .. }) |
        Paren(ConstParen { ref expr }) => {
            visitor.visit_const_expr(expr);
        }
        Other(ref other) => {
            #[cfg(feature = "full")]
            fn walk_other<V: Visitor>(visitor: &mut V, other: &Expr) {
                visitor.visit_expr(other);
            }
            #[cfg(not(feature = "full"))]
            fn walk_other<V: Visitor>(_: &mut V, _: &super::constant::Other) {}
            walk_other(visitor, other);
        }
    }
}

pub fn walk_mac<V: Visitor>(visitor: &mut V, mac: &Mac) {
    visitor.visit_path(&mac.path);
}

#[cfg(feature = "full")]
pub fn walk_crate<V: Visitor>(visitor: &mut V, _crate: &Crate) {
    walk_list!(visitor, visit_attribute, &_crate.attrs);
    walk_list!(visitor, visit_item, &_crate.items);
}

#[cfg(feature = "full")]
pub fn walk_item<V: Visitor>(visitor: &mut V, item: &Item) {
    use item::*;

    visitor.visit_ident(&item.ident);
    walk_list!(visitor, visit_attribute, &item.attrs);
    match item.node {
        ItemKind::ExternCrate(ItemExternCrate { ref original }) => {
            walk_opt_ident(visitor, original);
        }
        ItemKind::Use(ItemUse { ref path }) => {
            visitor.visit_view_path(path);
        }
        ItemKind::Static(ItemStatic { ref ty, ref expr, .. }) |
        ItemKind::Const(ItemConst { ref ty, ref expr }) => {
            visitor.visit_ty(ty);
            visitor.visit_expr(expr);
        }
        ItemKind::Fn(ItemFn { ref decl, ref generics, ref block, ..  }) => {
            visitor.visit_fn_decl(decl);
            visitor.visit_generics(generics);
            walk_list!(visitor, visit_stmt, &block.stmts);
        }
        ItemKind::Mod(ItemMod { ref items }) => {
            if let Some(ref items) = *items {
                walk_list!(visitor, visit_item, items);
            }
        }
        ItemKind::ForeignMod(ItemForeignMod { ref items, .. }) => {
            walk_list!(visitor, visit_foreign_item, items);
        }
        ItemKind::Ty(ItemTy { ref ty, ref generics }) => {
            visitor.visit_ty(ty);
            visitor.visit_generics(generics);
        }
        ItemKind::Enum(ItemEnum { ref variants, ref generics }) => {
            walk_list!(visitor, visit_variant, variants, generics);
        }
        ItemKind::Struct(ItemStruct { ref data, ref generics }) |
        ItemKind::Union(ItemUnion { ref data, ref generics }) => {
            visitor.visit_variant_data(data, &item.ident, generics);
        }
        ItemKind::Trait(ItemTrait {
            ref generics,
            ref supertraits,
            ref items,
            ..
        }) => {
            visitor.visit_generics(generics);
            walk_list!(visitor, visit_ty_param_bound, supertraits);
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
            if let Some(ref path) = *trait_ {
                visitor.visit_path(path);
            }
            visitor.visit_ty(self_ty);
            walk_list!(visitor, visit_impl_item, items);
        }
        ItemKind::Mac(ref mac) => visitor.visit_mac(mac),
    }
}

#[cfg(feature = "full")]
#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]
pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    use expr::*;
    use expr::ExprKind::*;

    walk_list!(visitor, visit_attribute, &expr.attrs);
    match expr.node {
        InPlace(ExprInPlace { ref place, ref value }) => {
            visitor.visit_expr(place);
            visitor.visit_expr(value);
        }
        Call(ExprCall { ref func, ref args }) => {
            visitor.visit_expr(func);
            walk_list!(visitor, visit_expr, args);
        }
        MethodCall(ExprMethodCall { ref method, ref typarams, ref args }) => {
            visitor.visit_ident(method);
            walk_list!(visitor, visit_ty, typarams);
            walk_list!(visitor, visit_expr, args);
        }
        Array(ExprArray { ref exprs }) |
        Tup(ExprTup { args: ref exprs }) => {
            walk_list!(visitor, visit_expr, exprs);
        }
        Lit(ref lit) => {
            visitor.visit_lit(lit);
        }
        Cast(ExprCast { ref expr, ref ty }) |
        Type(ExprType { ref expr, ref ty }) => {
            visitor.visit_expr(expr);
            visitor.visit_ty(ty);
        }
        If(ExprIf { ref cond, ref if_true, ref if_false }) => {
            visitor.visit_expr(cond);
            walk_list!(visitor, visit_stmt, &if_true.stmts);
            if let Some(ref alt) = *if_false {
                visitor.visit_expr(alt);
            }
        }
        IfLet(ExprIfLet { ref pat, ref expr, ref if_true, ref if_false }) => {
            visitor.visit_pat(pat);
            visitor.visit_expr(expr);
            walk_list!(visitor, visit_stmt, &if_true.stmts);
            if let Some(ref alt) = *if_false {
                visitor.visit_expr(alt);
            }
        }
        While(ExprWhile { ref cond, ref body, ref label }) => {
            visitor.visit_expr(cond);
            walk_list!(visitor, visit_stmt, &body.stmts);
            walk_opt_ident(visitor, label);
        }
        WhileLet(ExprWhileLet { ref pat, ref expr, ref body, ref label }) |
        ForLoop(ExprForLoop { ref pat, ref expr, ref body, ref label }) => {
            visitor.visit_pat(pat);
            visitor.visit_expr(expr);
            walk_list!(visitor, visit_stmt, &body.stmts);
            walk_opt_ident(visitor, label);
        }
        Loop(ExprLoop { ref body, ref label }) => {
            walk_list!(visitor, visit_stmt, &body.stmts);
            walk_opt_ident(visitor, label);
        }
        Match(ExprMatch { ref expr, ref arms }) => {
            visitor.visit_expr(expr);
            for &Arm { ref attrs, ref pats, ref guard, ref body } in arms {
                walk_list!(visitor, visit_attribute, attrs);
                walk_list!(visitor, visit_pat, pats);
                if let Some(ref guard) = *guard {
                    visitor.visit_expr(guard);
                }
                visitor.visit_expr(body);
            }
        }
        Closure(ExprClosure { ref decl, ref body, .. }) => {
            visitor.visit_fn_decl(decl);
            visitor.visit_expr(body);
        }
        Catch(ExprCatch { ref block }) |
        Block(ExprBlock { ref block, .. }) => {
            walk_list!(visitor, visit_stmt, &block.stmts);
        }
        Binary(ExprBinary { ref left, ref right, .. }) |
        Assign(ExprAssign { ref left, ref right }) |
        AssignOp(ExprAssignOp { ref left, ref right, .. }) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        Field(ExprField { ref expr, ref field }) => {
            visitor.visit_expr(expr);
            visitor.visit_ident(field);
        }
        Index(ExprIndex { ref expr, ref index }) => {
            visitor.visit_expr(expr);
            visitor.visit_expr(index);
        }
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
        Break(ExprBreak { ref label, ref expr }) => {
            walk_opt_ident(visitor, label);
            if let Some(ref expr) = *expr {
                visitor.visit_expr(expr);
            }
        }
        Continue(ExprContinue { ref label }) => {
            walk_opt_ident(visitor, label);
        }
        Ret(ExprRet { ref expr }) => {
            if let Some(ref expr) = *expr {
                visitor.visit_expr(expr);
            }
        }
        Mac(ref mac) => {
            visitor.visit_mac(mac);
        }
        Struct(ExprStruct { ref path, ref fields, ref rest }) => {
            visitor.visit_path(path);
            for &FieldValue { ref ident, ref expr, .. } in fields {
                visitor.visit_ident(ident);
                visitor.visit_expr(expr);
            }
            if let Some(ref base) = *rest {
                visitor.visit_expr(base);
            }
        }
        Repeat(ExprRepeat { ref expr, ref amt }) => {
            visitor.visit_expr(expr);
            visitor.visit_expr(amt);
        }
        TupField(ExprTupField { ref expr, .. }) |
        Unary(ExprUnary { ref expr, .. }) |
        Box(ExprBox { ref expr }) |
        AddrOf(ExprAddrOf { ref expr, .. }) |
        Paren(ExprParen { ref expr }) |
        Try(ExprTry { ref expr }) => {
            visitor.visit_expr(expr);
        }
    }
}

#[cfg(feature = "full")]
pub fn walk_foreign_item<V: Visitor>(visitor: &mut V, foreign_item: &ForeignItem) {
    use item::*;

    visitor.visit_ident(&foreign_item.ident);
    walk_list!(visitor, visit_attribute, &foreign_item.attrs);
    match foreign_item.node {
        ForeignItemKind::Fn(ForeignItemFn { ref decl, ref generics }) => {
            visitor.visit_fn_decl(decl);
            visitor.visit_generics(generics);
        }
        ForeignItemKind::Static(ForeignItemStatic { ref ty, .. }) => {
            visitor.visit_ty(ty);
        }
    }
}

#[cfg(feature = "full")]
pub fn walk_pat<V: Visitor>(visitor: &mut V, pat: &Pat) {
    match *pat {
        Pat::Wild => {}
        Pat::Ident(_, ref ident, ref maybe_pat) => {
            visitor.visit_ident(ident);
            if let Some(ref pat) = *maybe_pat {
                visitor.visit_pat(pat);
            }
        }
        Pat::Struct(ref path, ref field_pats, _) => {
            visitor.visit_path(path);
            for &FieldPat { ref ident, ref pat, .. } in field_pats {
                visitor.visit_ident(ident);
                visitor.visit_pat(pat);
            }
        }
        Pat::TupleStruct(ref path, ref pats, _) => {
            visitor.visit_path(path);
            walk_list!(visitor, visit_pat, pats);
        }
        Pat::Path(ref maybe_qself, ref path) => {
            if let Some(ref qself) = *maybe_qself {
                visitor.visit_ty(&qself.ty);
            }
            visitor.visit_path(path);
        }
        Pat::Tuple(ref pats, _) => {
            walk_list!(visitor, visit_pat, pats);
        }
        Pat::Box(ref pat) |
        Pat::Ref(ref pat, _) => {
            visitor.visit_pat(pat);
        }
        Pat::Lit(ref expr) => {
            visitor.visit_expr(expr);
        }
        Pat::Range(ref start, ref end, _) => {
            visitor.visit_expr(start);
            visitor.visit_expr(end);
        }
        Pat::Slice(ref start, ref maybe_mid, ref end) => {
            walk_list!(visitor, visit_pat, start);
            if let Some(ref mid) = *maybe_mid {
                visitor.visit_pat(mid);
            }
            walk_list!(visitor, visit_pat, end);
        }
        Pat::Mac(ref mac) => {
            visitor.visit_mac(mac);
        }
    }
}

#[cfg(feature = "full")]
pub fn walk_fn_decl<V: Visitor>(visitor: &mut V, fn_decl: &FnDecl) {
    use item::*;

    for input in &fn_decl.inputs {
        match *input {
            FnArg::SelfRef(_) |
            FnArg::SelfValue(_) => {}
            FnArg::Captured(ArgCaptured { ref pat, ref ty }) => {
                visitor.visit_pat(pat);
                visitor.visit_ty(ty);
            }
            FnArg::Ignored(ref ty) => {
                visitor.visit_ty(ty);
            }
        }
    }
    visitor.visit_fn_ret_ty(&fn_decl.output);
}

#[cfg(feature = "full")]
pub fn walk_trait_item<V: Visitor>(visitor: &mut V, trait_item: &TraitItem) {
    use item::*;

    visitor.visit_ident(&trait_item.ident);
    walk_list!(visitor, visit_attribute, &trait_item.attrs);
    match trait_item.node {
        TraitItemKind::Const(TraitItemConst { ref ty, ref default }) => {
            visitor.visit_ty(ty);
            if let Some(ref expr) = *default {
                visitor.visit_expr(expr);
            }
        }
        TraitItemKind::Method(TraitItemMethod { ref sig, ref default }) => {
            visitor.visit_method_sig(sig);
            if let Some(ref block) = *default {
                walk_list!(visitor, visit_stmt, &block.stmts);
            }
        }
        TraitItemKind::Type(TraitItemType { ref bounds, ref default }) => {
            walk_list!(visitor, visit_ty_param_bound, bounds);
            if let Some(ref ty) = *default {
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

    visitor.visit_ident(&impl_item.ident);
    walk_list!(visitor, visit_attribute, &impl_item.attrs);
    match impl_item.node {
        ImplItemKind::Const(ImplItemConst { ref ty, ref expr }) => {
            visitor.visit_ty(ty);
            visitor.visit_expr(expr);
        }
        ImplItemKind::Method(ImplItemMethod { ref sig, ref block }) => {
            visitor.visit_method_sig(sig);
            walk_list!(visitor, visit_stmt, &block.stmts);
        }
        ImplItemKind::Type(ImplItemType { ref ty }) => {
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
    visitor.visit_generics(&method_sig.generics);
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
        Stmt::Semi(ref expr) => {
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
        ViewPath::Simple(PathSimple { ref path, ref rename }) => {
            visitor.visit_path(path);
            walk_opt_ident(visitor, rename);
        }
        ViewPath::Glob(PathGlob { ref path }) => {
            visitor.visit_path(path);
        }
        ViewPath::List(PathList { ref path, ref items }) => {
            visitor.visit_path(path);
            for &PathListItem { ref name, ref rename } in items {
                visitor.visit_ident(name);
                walk_opt_ident(visitor, rename);
            }
        }
    }
}
