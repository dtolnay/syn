trait T {}

trait T: 'static + A + B<C> {}

pub unsafe trait T {}

trait T {
    const a: u8;
    const b: u8 = 0;
}

trait T {
    fn f<X>() -> Result<T>;
    fn f<X>(self);
    fn f<X>(mut self);
    fn f<X>(&self, &u8);
    fn f<X>(&mut self) {}
    fn f<X>(&'a self, &u8);
    fn f<X>(&'a mut self) {}
}

trait T {
    type X;
    type X: 'a + B;
    type X = ();
    type X: 'a + B = ();
}

trait T {
    mac!();
    mac!{}
    mac!();
    mac!{}
}
