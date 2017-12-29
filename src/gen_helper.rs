#[cfg(feature = "fold")]
pub mod fold {
    use delimited::Delimited;
    use fold::Folder;
    use proc_macro2::Span;

    pub trait FoldHelper {
        type Item;
        fn lift<F>(self, f: F) -> Self where F: FnMut(Self::Item) -> Self::Item;
    }

    impl<T> FoldHelper for Vec<T> {
        type Item = T;
        fn lift<F>(self, f: F) -> Self where F: FnMut(Self::Item) -> Self::Item {
            self.into_iter().map(f).collect()
        }
    }

    impl<T, U> FoldHelper for Delimited<T, U> {
        type Item = T;
        fn lift<F>(self, mut f: F) -> Self where F: FnMut(Self::Item) -> Self::Item {
            self.into_iter().map(|elem| {
                let (t, u) = elem.into_tuple();
                (f(t), u)
            }).collect::<Vec<(T, Option<U>)>>().into()
        }
    }

    pub fn tokens_helper<F: Folder + ?Sized, S: Spans>(folder: &mut F, spans: &S) -> S {
        spans.fold(folder)
    }

    pub trait Spans {
        fn fold<F: Folder + ?Sized>(&self, folder: &mut F) -> Self;
    }

    impl Spans for Span {
        fn fold<F: Folder + ?Sized>(&self, folder: &mut F) -> Self {
            folder.fold_span(*self)
        }
    }

    impl Spans for [Span; 1] {
        fn fold<F: Folder + ?Sized>(&self, folder: &mut F) -> Self {
            [folder.fold_span(self[0])]
        }
    }

    impl Spans for [Span; 2] {
        fn fold<F: Folder + ?Sized>(&self, folder: &mut F) -> Self {
            [folder.fold_span(self[0]), folder.fold_span(self[1])]
        }
    }

    impl Spans for [Span; 3] {
        fn fold<F: Folder + ?Sized>(&self, folder: &mut F) -> Self {
            [folder.fold_span(self[0]), folder.fold_span(self[1]), folder.fold_span(self[2])]
        }
    }
}

#[cfg(feature = "visit")]
pub mod visit {
    use proc_macro2::Span;
    use visit::Visitor;

    pub fn tokens_helper<'ast, V: Visitor<'ast> + ?Sized, S: Spans>(visitor: &mut V, spans: &'ast S) {
        spans.visit(visitor);
    }

    pub trait Spans {
        fn visit<'ast, V: Visitor<'ast> + ?Sized>(&'ast self, visitor: &mut V);
    }

    impl Spans for Span {
        fn visit<'ast, V: Visitor<'ast> + ?Sized>(&'ast self, visitor: &mut V) {
            visitor.visit_span(self);
        }
    }

    impl Spans for [Span; 1] {
        fn visit<'ast, V: Visitor<'ast> + ?Sized>(&'ast self, visitor: &mut V) {
            visitor.visit_span(&self[0]);
        }
    }

    impl Spans for [Span; 2] {
        fn visit<'ast, V: Visitor<'ast> + ?Sized>(&'ast self, visitor: &mut V) {
            visitor.visit_span(&self[0]);
            visitor.visit_span(&self[1]);
        }
    }

    impl Spans for [Span; 3] {
        fn visit<'ast, V: Visitor<'ast> + ?Sized>(&'ast self, visitor: &mut V) {
            visitor.visit_span(&self[0]);
            visitor.visit_span(&self[1]);
            visitor.visit_span(&self[2]);
        }
    }
}

#[cfg(feature = "visit_mut")]
pub mod visit_mut {
    use proc_macro2::Span;
    use visit_mut::VisitorMut;

    pub fn tokens_helper<V: VisitorMut + ?Sized, S: Spans>(visitor: &mut V, spans: &mut S) {
        spans.visit_mut(visitor);
    }

    pub trait Spans {
        fn visit_mut<V: VisitorMut + ?Sized>(&mut self, visitor: &mut V);
    }

    impl Spans for Span {
        fn visit_mut<V: VisitorMut + ?Sized>(&mut self, visitor: &mut V) {
            visitor.visit_span_mut(self);
        }
    }

    impl Spans for [Span; 1] {
        fn visit_mut<V: VisitorMut + ?Sized>(&mut self, visitor: &mut V) {
            visitor.visit_span_mut(&mut self[0]);
        }
    }

    impl Spans for [Span; 2] {
        fn visit_mut<V: VisitorMut + ?Sized>(&mut self, visitor: &mut V) {
            visitor.visit_span_mut(&mut self[0]);
            visitor.visit_span_mut(&mut self[1]);
        }
    }

    impl Spans for [Span; 3] {
        fn visit_mut<V: VisitorMut + ?Sized>(&mut self, visitor: &mut V) {
            visitor.visit_span_mut(&mut self[0]);
            visitor.visit_span_mut(&mut self[1]);
            visitor.visit_span_mut(&mut self[2]);
        }
    }
}
