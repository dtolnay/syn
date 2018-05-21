// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(feature = "extra-traits")]

extern crate proc_macro2;
extern crate syn;
use proc_macro2::*;
use syn::*;

macro_rules! assert_let {
    ($p:pat = $e:expr) => {
        assert_let!($p = $e; {})
    };

    ($p:pat = $e:expr; $body:block) => {
        if let $p = $e
            $body
        else {
            panic!("Expected to match {} but got {:?}", stringify!($p), $e)
        }
    };
}

#[test]
#[cfg(feature = "full")]
fn test_catch_expr() {
    // Taken from tests/rust/src/test/run-pass/catch-expr.rs
    let raw = r#"
        struct catch {}

        pub fn main() {
            let catch_result = do catch {
                let x = 5;
                x
            };
            assert_eq!(catch_result, 5);

            let mut catch = true;
            while catch { catch = false; }
            assert_eq!(catch, false);

            catch = if catch { false } else { true };
            assert_eq!(catch, true);

            match catch {
                _ => {}
            };

            let catch_err = do catch {
                Err(22)?;
                Ok(1)
            };
            assert_eq!(catch_err, Err(22));

            let catch_okay: Result<i32, i32> = do catch {
                if false { Err(25)?; }
                Ok::<(), i32>(())?;
                Ok(28)
            };
            assert_eq!(catch_okay, Ok(28));

            let catch_from_loop: Result<i32, i32> = do catch {
                for i in 0..10 {
                    if i < 5 { Ok::<i32, i32>(i)?; } else { Err(i)?; }
                }
                Ok(22)
            };
            assert_eq!(catch_from_loop, Err(5));

            let cfg_init;
            let _res: Result<(), ()> = do catch {
                cfg_init = 5;
                Ok(())
            };
            assert_eq!(cfg_init, 5);

            let cfg_init_2;
            let _res: Result<(), ()> = do catch {
                cfg_init_2 = 6;
                Err(())?;
                Ok(())
            };
            assert_eq!(cfg_init_2, 6);

            let my_string = "test".to_string();
            let res: Result<&str, ()> = do catch {
                Ok(&my_string)
            };
            assert_eq!(res, Ok("test"));
        }
    "#;

    let actual: File = syn::parse_str(raw).unwrap();

    assert_let!(Item::Struct(ItemStruct { ref ident, .. }) = actual.items[0]; {
        assert_eq!(ident, "catch");
    });

    assert_let!(Item::Fn(ItemFn { ref block, .. }) = actual.items[1]; {
        assert_let!(Stmt::Local(ref local) = block.stmts[0]; {
            assert_let!(Local { init: Some((_, ref init_expr)), .. } = *local; {
                assert_let!(Expr::Catch(..) = **init_expr);
            });
        });

        assert_let!(Stmt::Local(ref local) = block.stmts[2]; {
            assert_let!(Pat::Ident(PatIdent { by_ref: None, mutability: Some(_), ref ident, .. }) = local.pats.iter().next().unwrap(); {
                assert_eq!(ident, "catch");
            });
        });

        assert_let!(Stmt::Expr(ref expr) = block.stmts[3]; {
            assert_let!(Expr::While(ExprWhile { ref cond, .. }) = *expr; {
                assert_let!(Expr::Path(ExprPath { qself: None, ref path, .. }) = **cond; {
                    let name = Ident::new("catch", Span::call_site());
                    assert_eq!(*path, name.into());
                });
            });
        });

        assert_let!(Stmt::Semi(ref expr, _) = block.stmts[5]; {
            assert_let!(Expr::Assign(ExprAssign { ref left, ref right, .. }) = *expr; {
                assert_let!(Expr::Path(ExprPath { qself: None, ref path, .. }) = **left; {
                    let name = Ident::new("catch", Span::call_site());
                    assert_eq!(*path, name.into());
                });

                assert_let!(Expr::If(ExprIf { ref cond, .. }) = **right; {
                    assert_let!(Expr::Path(ExprPath { qself: None, ref path, .. }) = **cond; {
                        let name = Ident::new("catch", Span::call_site());
                        assert_eq!(*path, name.into());
                    });
                });
            });
        });

        assert_let!(Stmt::Semi(ref expr, _) = block.stmts[7]; {
            assert_let!(Expr::Match(ExprMatch { ref expr, .. }) = *expr; {
                assert_let!(Expr::Path(ExprPath { qself: None, ref path, .. }) = **expr; {
                    let name = Ident::new("catch", Span::call_site());
                    assert_eq!(*path, name.into());
                });
            });
        });
    });
}
