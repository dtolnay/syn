use std::mem;
use syn::visit_mut::{self, VisitMut};
use syn::{Expr, File, Generics, LifetimeParam, TypeParam};

pub struct FlattenParens;

impl VisitMut for FlattenParens {
    fn visit_expr_mut(&mut self, e: &mut Expr) {
        while let Expr::Paren(paren) = e {
            *e = mem::replace(&mut *paren.expr, Expr::PLACEHOLDER);
        }
        visit_mut::visit_expr_mut(self, e);
    }
}

pub struct AsIfPrinted;

impl VisitMut for AsIfPrinted {
    fn visit_file_mut(&mut self, file: &mut File) {
        file.shebang = None;
        visit_mut::visit_file_mut(self, file);
    }

    fn visit_generics_mut(&mut self, generics: &mut Generics) {
        if generics.params.is_empty() {
            generics.lt_token = None;
            generics.gt_token = None;
        }
        if let Some(where_clause) = &generics.where_clause {
            if where_clause.predicates.is_empty() {
                generics.where_clause = None;
            }
        }
        visit_mut::visit_generics_mut(self, generics);
    }

    fn visit_lifetime_param_mut(&mut self, param: &mut LifetimeParam) {
        if param.bounds.is_empty() {
            param.colon_token = None;
        }
        visit_mut::visit_lifetime_param_mut(self, param);
    }

    fn visit_type_param_mut(&mut self, param: &mut TypeParam) {
        if param.bounds.is_empty() {
            param.colon_token = None;
        }
        visit_mut::visit_type_param_mut(self, param);
    }
}
