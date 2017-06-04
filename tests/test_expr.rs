#![cfg(feature = "extra-traits")]

extern crate syn;
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

    assert_let!(ItemKind::Struct(ItemStruct { ref ident, .. }) = actual.items[0].node; {
        assert_eq!(ident, "catch");
    });

    assert_let!(Item { node: ItemKind::Fn(ItemFn { ref block, .. }), .. } = actual.items[1]; {
        assert_let!(Stmt::Local(ref local) = block.stmts[0]; {
            assert_let!(Local { init: Some(ref init_expr), .. } = **local; {
                assert_let!(Expr { node: ExprKind::Catch(..), .. } = **init_expr);
            });
        });

        assert_let!(Stmt::Local(ref local) = block.stmts[2]; {
            assert_let!(Pat::Ident(PatIdent { mode: BindingMode::ByValue(Mutability::Mutable(_)), ref ident, .. }) = *local.pat; {
                assert_eq!(ident, "catch");
            });
        });

        assert_let!(Stmt::Expr(ref expr) = block.stmts[3]; {
            assert_let!(Expr { node: ExprKind::While(ExprWhile { ref cond, .. }), .. } = **expr; {
                assert_let!(Expr { node: ExprKind::Path(ExprPath { qself: None, ref path }), .. } = **cond; {
                    assert_eq!(*path, "catch".into());
                });
            });
        });

        assert_let!(Stmt::Semi(ref expr, _) = block.stmts[5]; {
            assert_let!(Expr { node: ExprKind::Assign(ExprAssign { ref left, ref right, .. }), .. } = **expr; {
                assert_let!(Expr { node: ExprKind::Path(ExprPath { qself: None, ref path }), .. } = **left; {
                    assert_eq!(*path, "catch".into());
                });

                assert_let!(Expr { node: ExprKind::If(ExprIf { ref cond, .. }), .. } = **right; {
                    assert_let!(Expr { node: ExprKind::Path(ExprPath { qself: None, ref path }), .. } = **cond; {
                        assert_eq!(*path, "catch".into());
                    });
                });
            });
        });

        assert_let!(Stmt::Semi(ref expr, _) = block.stmts[7]; {
            assert_let!(Expr { node: ExprKind::Match(ExprMatch { ref expr, .. }), .. } = **expr; {
                assert_let!(Expr { node: ExprKind::Path(ExprPath { qself: None, ref path }), .. } = **expr; {
                    assert_eq!(*path, "catch".into());
                });
            });
        });
    });
}
