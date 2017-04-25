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

    let actual = parse_crate(raw).unwrap();

    assert_eq!(&actual.items[0].ident, "catch");

    assert_let!(ItemKind::Struct(..) = actual.items[0].node);

    assert_let!(Item { node: ItemKind::Fn(_, _, _, _, _, ref body), .. } = actual.items[1]; {
        assert_let!(Stmt::Local(ref local) = body.stmts[0]; {
            assert_let!(Local { init: Some(ref init_expr), .. } = **local; {
                assert_let!(Expr { node: ExprKind::Catch(..), .. } = **init_expr);
            });
        });

        assert_let!(Stmt::Local(ref local) = body.stmts[2]; {
            assert_let!(Pat::Ident(BindingMode::ByValue(Mutability::Mutable), ref ident, None) = *local.pat; {
                assert_eq!(ident, "catch");
            });
        });

        assert_let!(Stmt::Expr(ref expr) = body.stmts[3]; {
            assert_let!(Expr { node: ExprKind::While(ref loop_expr, _, None), .. } = **expr; {
                assert_let!(Expr { node: ExprKind::Path(None, ref loop_var), .. } = **loop_expr; {
                    assert_eq!(*loop_var, "catch".into());
                });
            });
        });

        assert_let!(Stmt::Semi(ref expr) = body.stmts[5]; {
            assert_let!(Expr { node: ExprKind::Assign(ref left, ref right), .. } = **expr; {
                assert_let!(Expr { node: ExprKind::Path(None, ref loop_var), .. } = **left; {
                    assert_eq!(*loop_var, "catch".into());
                });

                assert_let!(Expr { node: ExprKind::If(ref if_expr, _, _), .. } = **right; {
                    assert_let!(Expr { node: ExprKind::Path(None, ref if_var), .. } = **if_expr; {
                        assert_eq!(*if_var, "catch".into());
                    });
                });
            });
        });

        assert_let!(Stmt::Semi(ref expr) = body.stmts[7]; {
            assert_let!(Expr { node: ExprKind::Match(ref match_expr, _), .. } = **expr; {
                assert_let!(Expr { node: ExprKind::Path(None, ref match_var), .. } = **match_expr; {
                    assert_eq!(*match_var, "catch".into());
                });
            });
        });
    });
}
