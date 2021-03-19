use crate::codegen::{Context, *};
use inkwell::OptimizationLevel;
use insta::*;

#[track_caller]
fn test_codegen(src: &str) { test::<()>(src, None) }

#[track_caller]
fn test_codegen_and_run<T>(src: &str, expected: T)
where
    T: PartialEq + std::fmt::Debug,
{
    test(src, Some(expected))
}

fn test<T>(src: &str, expected: Option<T>)
where
    T: PartialEq + std::fmt::Debug,
{
    let syntax = walrus_parser::parse(src);
    let hir = walrus_semantics::hir::lower(&syntax);
    let scopes = walrus_semantics::scopes::scopes(&hir);
    let types = walrus_semantics::ty::infer(hir.clone(), scopes.clone());

    let llvm = Context::create();
    let builder = llvm.create_builder();
    let module = llvm.create_module("module");

    let llvm_module = {
        let compiler = Compiler {
            llvm: &llvm,
            module,
            builder,

            hir: hir.data,
            scopes,
            types,
        };
        compiler.codegen_module()
    };

    let mut settings = insta::Settings::new();
    settings.set_snapshot_path("../snapshots");
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| assert_display_snapshot!(llvm_module.print_to_string().to_string()));

    if let Some(expected) = expected {
        let exec_engine = llvm_module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let f = unsafe { exec_engine.get_function::<unsafe extern "C" fn() -> T>("main") }.unwrap();
        assert_eq!(unsafe { f.call() }, expected);
    }
}

mod literals {
    use super::*;
    #[test]
    fn empty_fn() { test_codegen_and_run("fn main() -> _ {}", ()) }

    #[test]
    fn lit_true() { test_codegen_and_run(r#"fn main() -> _ { true }"#, true) }

    #[test]
    fn lit_false() { test_codegen_and_run(r#"fn main() -> _ { false }"#, false) }

    #[test]
    fn lit_int() { test_codegen_and_run(r#"fn main() -> _ { 1 }"#, 1_i32) }

    #[test]
    fn lit_float() { test_codegen_and_run(r#"fn main() -> _ { 1.234 }"#, 1.234_f32) }

    #[test]
    fn lit_char() { test_codegen_and_run(r#"fn main() -> _ { 'a' }"#, 'a') }
}

mod tuples {
    use super::*;
    #[test]
    fn tuple0() { test_codegen_and_run(r#"fn main() -> _ { () }"#, ()) }

    #[test]
    fn tuple1() { test_codegen_and_run(r#"fn main() -> _ { (1,) }"#, (1_i32,)) }

    #[test]
    fn tuple2() { test_codegen_and_run(r#"fn main() -> _ { (1, 2) }"#, (1_i32, 2_i64)) }
}

mod if_ {
    use super::*;
    #[test]
    fn if_then_else_true() {
        test_codegen_and_run("fn main() -> _ { if true {1} else {0} }", 1_i32)
    }

    #[test]
    fn if_then_else_false() {
        test_codegen_and_run("fn main() -> _ { if false {1} else {0} }", 0_i32)
    }

    #[test]
    fn if_then_true() { test_codegen_and_run(r#"fn main() -> _ { if true {1} }"#, ()); }
    #[test]
    fn if_then_false() { test_codegen_and_run(r#"fn main() -> _ { if false {1} }"#, ()); }
}

mod let_ {
    use super::*;

    #[test]
    fn let_var() { test_codegen_and_run(r#"fn main() -> _ { let x = 5; x }"#, 5_i32); }

    #[test]
    fn let_tuple2() { test_codegen_and_run(r#"fn main() -> _ { let (x, y) = (5, 6); x }"#, 5_i32) }

    #[test]
    fn let_tuple4() {
        test_codegen_and_run(
            r#"
fn main() -> _ {
    let (a, b, c, d) = (9, 8, 7, 6);
    let (x, (y, z), w) = (d, (c, b), a);
    x
}"#,
            6_i32,
        )
    }
}

mod fns {
    use super::*;
    #[test]
    fn id_fn() {
        test_codegen_and_run(
            r#"
fn main() -> _ { id(5) }
fn id(x: _) -> _ {x}
"#,
            5_i32,
        )
    }

    #[test]
    fn get_five_fn() {
        test_codegen_and_run(
            r#"
fn main() -> _ { get_five() }
fn get_five() -> _ {5}
"#,
            5_i64,
        )
    }
}

mod return_ {
    use super::*;
    #[test]
    fn early_return() {
        test_codegen_and_run(
            r#"
fn main() -> _ {
    return 5;
    6
}
"#,
            5_i64,
        )
    }

    #[test]
    fn multiple_return() {
        test_codegen_and_run(
            r#"
fn main() -> _ {
    return 5;
    return 6;
}
"#,
            5_i64,
        )
    }

    #[test]
    fn multi_expr_return() { test_codegen_and_run(r#"fn main() -> _ { 1 + (return 2) }"#, 2); }

    #[test]
    #[ignore]
    fn if_then_return() {
        test_codegen_and_run(
            r#"
fn main() -> _ {
    if true {return 1}
    else {0}
}
"#,
            1,
        )
    }
}

mod bool_ops {
    use super::*;
    #[test]
    fn bool_not() { test_codegen_and_run(r#"fn main() -> _  {!false}"#, true) }
    #[test]
    fn bool_or() { test_codegen_and_run(r#"fn main() -> _  {false || true}"#, true) }

    #[test]
    fn bool_and() { test_codegen_and_run(r#"fn main() -> _ {true && false}"#, false) }
}

mod int_ops {
    use super::*;

    #[test]
    fn int_neg() { test_codegen_and_run(r#"fn main() ->   _  {-1}"#, -1_i32) }
    #[test]
    fn int_plus() { test_codegen_and_run(r#"fn main() ->   _  {+1}"#, 1_i32) }

    #[test]
    fn int_add() { test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 1; x + y}"#, 2_i32) }
    #[test]
    fn int_sub() { test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 1; x - y}"#, 0_i32) }
    #[test]
    fn int_mul() { test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 2; x * y}"#, 2_i32) }
    #[test]
    fn int_div() { test_codegen_and_run(r#"fn main() -> _ {let x = 5; let y = 2; x / y}"#, 2_i32) }

    #[test]
    fn int_eq() { test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 1; x == y}"#, true) }
    #[test]
    fn int_neq() { test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 1; x != y}"#, false) }
    #[test]
    fn int_less() { test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 1; x < y}"#, false) }
    #[test]
    fn int_less_eq() {
        test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 1; x <= y}"#, true)
    }
    #[test]
    fn int_greater() {
        test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 1; x > y}"#, false)
    }
    #[test]
    fn int_greater_eq() {
        test_codegen_and_run(r#"fn main() -> _ {let x = 1; let y = 1; x >= y}"#, true)
    }
}

mod float_ops {
    use super::*;

    #[test]
    fn float_neg() { test_codegen_and_run(r#"fn main() -> _  {-1.0}"#, -1.0_f32) }
    #[test]
    fn float_plus() { test_codegen_and_run(r#"fn main() -> _  {+1.0}"#, 1.0_f32) }

    #[test]
    fn float_add() {
        test_codegen_and_run(
            r#"fn main() -> _ {let x = 1.0; let y = 1.0; x + y}"#,
            2.0_f32,
        )
    }
    #[test]
    fn float_sub() {
        test_codegen_and_run(
            r#"fn main() -> _ {let x = 1.0; let y = 1.0; x - y}"#,
            0.0_f32,
        )
    }
    #[test]
    fn float_mul() {
        test_codegen_and_run(
            r#"fn main() -> _ {let x = 1.0; let y = 2.0; x * y}"#,
            2.0_f32,
        )
    }
    #[test]
    fn float_div() {
        test_codegen_and_run(
            r#"fn main() -> _ {let x = 5.0; let y = 2.0; x / y}"#,
            2.5_f32,
        )
    }

    #[test]
    fn float_eq() {
        test_codegen_and_run(r#"fn main() -> _ {let x = 1.0; let y = 1.0; x == y}"#, true)
    }
    #[test]
    fn float_neq() {
        test_codegen_and_run(
            r#"fn main() -> _ {let x = 1.0; let y = 1.0; x != y}"#,
            false,
        )
    }
    #[test]
    fn float_less() {
        test_codegen_and_run(r#"fn main() -> _ {let x = 1.0; let y = 1.0; x < y}"#, false)
    }
    #[test]
    fn float_less_eq() {
        test_codegen_and_run(r#"fn main() -> _ {let x = 1.0; let y = 1.0; x <= y}"#, true)
    }
    #[test]
    fn float_greater() {
        test_codegen_and_run(r#"fn main() -> _ {let x = 1.0; let y = 1.0; x > y}"#, false)
    }
    #[test]
    fn float_greater_eq() {
        test_codegen_and_run(r#"fn main() -> _ {let x = 1.0; let y = 1.0; x >= y}"#, true)
    }
}

mod assign {
    use super::*;

    #[test]
    fn assign_var() {
        test_codegen_and_run(
            r#"fn main() -> _ {
        let x = 5;
        x = 6;
        x
    }"#,
            6_i32,
        )
    }

    #[test]
    fn assign_field() {
        test_codegen_and_run(
            r#"
            struct S {x: Int}

            fn main() -> _ {
                let s = S {x: 0};
                s.x = 1;
                s.x
        }"#,
            1_i32,
        )
    }
}

mod builtins {
    use super::*;

    #[test]
    fn builtin_exit() { test_codegen("fn main() -> Int { exit(1) }") }

    #[test]
    fn builtin_putchar() {
        test_codegen_and_run(
            r#"
fn main() -> () {
    putchar('h');
    putchar('e');
    putchar('l');
    putchar('l');
    putchar('o');
    putchar(' ');
    putchar('w');
    putchar('o');
    putchar('r');
    putchar('l');
    putchar('d');
}
"#,
            (),
        )
    }
}

mod loops {
    use super::*;

    // this successfully loops forever, which makes the test never complete!
    #[test]
    fn loop_forever() { test_codegen("fn main() -> Int { loop {} }") }

    // this successfully loops forever, which makes the test never complete!
    #[test]
    fn loop_and_continue() { test_codegen("fn main() -> () { loop {let x = 5; continue} }") }

    #[test]
    fn loop_and_break() {
        test_codegen_and_run("fn main() -> _ { loop {let x = 5; break x} }", 5_i32)
    }

    #[test]
    #[ignore]
    fn loop_and_break_and_continue() {
        test_codegen_and_run(
            "fn main() -> _ { loop { if true {continue} else {break} } }",
            (),
        )
    }
}

mod structs {
    use super::*;
    #[test]
    fn struct_construtor() {
        test_codegen_and_run(
            r#"
struct Foo {x: Int}
fn main() -> _ {
    let foo = Foo {x:5};
    foo
}
"#,
            5_i32,
        )
    }

    #[test]
    fn field_expr() {
        test_codegen_and_run(
            r#"
struct Foo {x: Int,y:Int}
fn main() -> _ {
    let foo = Foo {y:6, x:5};
    foo.y
}
"#,
            6_i32,
        )
    }
}

mod enums {
    use super::*;

    #[test]
    fn enum0() {
        test_codegen(
            r#"
        enum Void {}
        fn main() -> _ {
            let impossible: Void = exit(0);
            impossible
        }
    "#,
        )
    }

    #[test]
    fn enum1() {
        test_codegen_and_run(
            r#"
        enum Foo { 
            Bar {} 
        }
        fn main() -> _ {
            Foo::Bar{}
        }
    "#,
            (),
        )
    }

    #[test]
    fn enum1_with_payload() {
        test_codegen_and_run(
            r#"
        enum Foo { 
            Bar {x: Int} 
        }
        fn main() -> _ {
            Foo::Bar{x: 5}
        }
    "#,
            5_i32,
        )
    }

    fn enum2_variant0() {
        test_codegen_and_run(
            r#"
        enum Foo { 
            Bar {x: Int},
            Baz {y: Bool} 
        }
        fn main() -> _ {
            Foo::Bar{x: 5}
        }
    "#,
            0_u8,
        );
    }
    fn enum2_variant1() {
        test_codegen_and_run(
            r#"
        enum Foo { 
            Bar {}
            Baz {}
        }
        fn main() -> _ {
            Foo::Bar{}
        }
    "#,
            1_u8,
        )
    }

    #[test]
    fn enum2_with_payload_variant0() {
        test_codegen_and_run(
            r#"
        enum Foo { 
            Bar {x: Int},
            Baz {y: Bool} 
        }
        fn main() -> _ {
            Foo::Bar{x: 5}
        }
    "#,
            (0_u8, 5_i64),
        )
    }

    #[test]
    #[ignore]
    fn enum2_with_payload_variant1() {
        test_codegen_and_run(
            r#"
        enum Foo { 
            Bar {x: Int}, 
            Baz {y: Bool} 
        }
        fn main() -> _ {
            Foo::Baz{y: true}
        }
    "#,
            (1_u8, 1_i32),
        )
    }
}