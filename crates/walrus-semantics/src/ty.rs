use self::unify::TypeVarId;
use crate::hir::{EnumDefId, StructDefId};

mod infer;
mod unify;

pub use self::infer::{infer, InferenceId, InferenceResult, VarMode};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    Fn(FnType),
    Struct(StructDefId),
    Enum(EnumDefId),
    Tuple(Vec<Self>),
    Infer(InferType),
    Unknown,
}

impl Type {
    pub const fn is_integral(&self) -> bool {
        matches!(
            self,
            Type::Primitive(PrimitiveType::Int | PrimitiveType::Char | PrimitiveType::Bool)
        )
    }

    pub const fn is_floating(&self) -> bool {
        matches!(self, Type::Primitive(PrimitiveType::Float))
    }

    pub const fn is_int(&self) -> bool { matches!(self, Type::Primitive(PrimitiveType::Int)) }
    pub const fn is_float(&self) -> bool { matches!(self, Type::Primitive(PrimitiveType::Float)) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    Char,
    Never,
}

impl From<FnType> for Type {
    fn from(func: FnType) -> Self { Self::Fn(func) }
}

impl Type {
    pub const UNIT: Self = Self::Tuple(vec![]);
    pub const BOOL: Self = Self::Primitive(PrimitiveType::Bool);
    pub const INT: Self = Self::Primitive(PrimitiveType::Int);
    pub const FLOAT: Self = Self::Primitive(PrimitiveType::Float);
    pub const CHAR: Self = Self::Primitive(PrimitiveType::Char);
    pub const NEVER: Self = Self::Primitive(PrimitiveType::Never);

    pub fn function(params: Vec<Self>, ret: Self) -> Self {
        Self::Fn(FnType {
            params,
            ret: box ret,
        })
    }

    pub const fn as_fn(&self) -> Option<&FnType> {
        match self {
            Self::Fn(func) => Some(func),
            _ => None,
        }
    }
    pub fn as_tuple(&self) -> Option<&[Self]> {
        match self {
            Self::Tuple(tys) => Some(tys),
            _ => None,
        }
    }
    pub const fn as_struct(&self) -> Option<StructDefId> {
        match self {
            Self::Struct(id) => Some(*id),
            _ => None,
        }
    }
    pub const fn as_enum(&self) -> Option<EnumDefId> {
        match self {
            Self::Enum(id) => Some(*id),
            _ => None,
        }
    }

    fn walk_mut(&mut self, f: &mut impl FnMut(&mut Self)) {
        f(self);
        match self {
            Self::Tuple(tys) => {
                for ty in tys {
                    ty.walk_mut(f)
                }
            }
            Self::Fn(FnType { params, ret }) => {
                for ty in params {
                    ty.walk_mut(f)
                }
                ret.walk_mut(f)
            }
            _ => {}
        }
    }
}

impl Type {
    fn fold(mut self, f: &mut impl FnMut(Self) -> Self) -> Self {
        self.walk_mut(&mut |ty_mut| {
            let ty = std::mem::replace(ty_mut, Self::Unknown);
            *ty_mut = f(ty);
        });
        self
    }

    pub const fn is_stack(&self) -> bool { !matches!(self, Self::Enum(_) | Self::Struct(_)) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum InferType {
    Var(TypeVarId),
}

impl InferType {
    const fn fallback_value(self) -> Type {
        match self {
            Self::Var(_) => Type::Unknown,
        }
    }

    const fn to_inner(self) -> TypeVarId {
        match self {
            Self::Var(ty) => ty,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::*;

    macro_rules! test_infer {
        ($name:ident, $src:expr, $expected:expr) => {
            #[test]
            fn $name() { test_infer($src, &$expected) }
        };
    }

    fn test_infer(src: &str, expected: &Type) {
        let syntax = walrus_parser::parse(src);
        let hir = crate::hir::lower(&syntax);
        let scopes = crate::scopes::scopes(&hir);
        let types = infer(hir.clone(), scopes);

        let first_fn = &hir.hir.fn_defs.iter().next().unwrap();
        let ret_type = &types.type_of_fn[first_fn.0].ret;

        assert_eq!(ret_type.as_ref(), expected);

        let mut settings = insta::Settings::new();
        settings.set_snapshot_path("../snapshots/infer");
        settings.set_prepend_module_to_snapshot(false);
        settings.bind(|| assert_debug_snapshot!(types));
    }

    test_infer!(empty_fn, r#"fn f() {}"#, Type::UNIT);
    test_infer!(infer_ret_type, r#"fn f() -> _ {}"#, Type::UNIT);
    test_infer!(bool_lit, r#"fn f() -> _ {true}"#, Type::BOOL);
    test_infer!(int_lit, r#"fn f() -> _ {1}"#, Type::INT);
    test_infer!(float_lit, r#"fn f() -> _ {1.0}"#, Type::FLOAT);
    test_infer!(char_lit, r#"fn f() -> _ {'a'}"#, Type::CHAR);

    test_infer!(unit_stmt, r#"fn f() -> _ {1;}"#, Type::UNIT);
    test_infer!(let_var, r#"fn f() -> _ {let x = 5; x}"#, Type::INT);
    test_infer!(
        fn_var,
        r#"
fn f() -> _ {g}
fn g() -> _ {1}
"#,
        Type::function(vec![], Type::INT)
    );

    test_infer!(
        if_then_else,
        r#"fn f() -> _ {if true {1} else {0}}"#,
        Type::INT
    );

    test_infer!(
        tuple,
        r#"fn f() -> _ {(1,false)}"#,
        Type::Tuple(vec![Type::INT, Type::BOOL])
    );
    test_infer!(
        tuple_destructure,
        r#"fn f() -> _ {let (x, y) = (1, false); (y, x)}"#,
        Type::Tuple(vec![Type::BOOL, Type::INT])
    );
    test_infer!(
        tuple_field,
        r#"fn f() -> _ {let x = (1, false); (x.1, x.0)}"#,
        Type::Tuple(vec![Type::BOOL, Type::INT])
    );

    test_infer!(
        struct_constructor,
        r#"
struct Foo {
    x: Int,
    y: Bool,
}
fn f() -> _ { Foo { x: 0, y: false } }
"#,
        Type::Struct(StructDefId::new(0))
    );

    test_infer!(
        enum_constructor,
        r#"
enum Foo {
    X { x: Int },
    Y { y: Bool },
}
fn f() -> _ { Foo::X { x: 0 } }
"#,
        Type::Enum(EnumDefId::new(0))
    );

    test_infer!(
        struct_destructure,
        r#"fn f() -> _ {let (x, y) = (1, false); (y, x)}"#,
        Type::Tuple(vec![Type::BOOL, Type::INT])
    );

    test_infer!(
        struct_field,
        r#"
struct Foo {
    x: Int,
    y: Bool,
}
fn f() -> _ {
    let foo = Foo { x: 0, y: false };
    (foo.x, foo.y)
}
"#,
        Type::Tuple(vec![Type::INT, Type::BOOL])
    );

    test_infer!(
        lambda,
        r#"fn f() -> _ { (x: Int) => false }"#,
        Type::function(vec![Type::INT], Type::BOOL)
    );

    test_infer!(
        lambda2,
        r#"fn f() -> _ { (x) => x + 1 }"#,
        Type::function(vec![Type::INT], Type::INT)
    );

    test_infer!(
        lambda_call,
        r#"
fn f() -> _ {
    let id = (x) => x;
    id(1)
}"#,
        Type::INT
    );

    test_infer!(unary_sub, r#"fn f() -> _ {-0}"#, Type::INT);
    test_infer!(unary_add, r#"fn f() -> _ {+0}"#, Type::INT);

    test_infer!(cmp, r#"fn f() -> _ {0 == 1}"#, Type::BOOL);
    test_infer!(loop_never, r#"fn f() -> Never { loop {} }"#, Type::NEVER);
    test_infer!(loop_unit, r#"fn f() -> _ { loop { break } }"#, Type::UNIT);
    test_infer!(loop_int, r#"fn f() -> _ { loop { break 1 } }"#, Type::INT);

    test_infer!(return_unit, r#"fn f() -> _ { return }"#, Type::UNIT);
    test_infer!(return_int, r#"fn f() -> _ { return 1 }"#, Type::INT);

    test_infer!(
        lambda_return_unit,
        r#"fn f() -> _ { () => return }"#,
        Type::function(vec![], Type::UNIT)
    );
    test_infer!(
        lambda_return_int,
        r#"fn f() -> _ { () => return 1 }"#,
        Type::function(vec![], Type::INT)
    );

    test_infer!(
        fn_params,
        r#"
fn f() -> _ { g }
fn g(_: Int) -> _ {}
"#,
        Type::function(vec![Type::INT], Type::UNIT)
    );

    test_infer!(
        factorial,
        r#"
fn f() -> _ { factorial }
fn factorial(x: _) -> _ {
    if x == 0 { 1 }
    else      { x * factorial(x-1) }
}
"#,
        Type::function(vec![Type::INT], Type::INT)
    );

    test_infer!(
        add,
        r#"
fn f() -> _ { add }
fn add(x: _, y: _) -> _ { x + y + 1 }
"#,
        Type::function(vec![Type::INT, Type::INT], Type::INT)
    );

    test_infer!(
        parity,
        r#"
fn f() -> _ { (is_odd, is_even) }
fn is_odd(x: _)  -> _ { if x == 0 { false } else { is_even(x - 1) } }
fn is_even(x: _) -> _ { if x == 0 { true  } else { is_odd(x - 1)  } }
"#,
        Type::Tuple(vec![
            Type::function(vec![Type::INT], Type::BOOL),
            Type::function(vec![Type::INT], Type::BOOL),
        ])
    );

    test_infer!(
        match_expr,
        r#"
fn f() -> _ {
    match 5 {
        x => x,
    }
}"#,
        Type::INT
    );

    test_infer!(
        coerce_if_branches_then,
        r#"
fn f() -> _ {
    if true {loop{}} else {5}
}
        "#,
        Type::INT
    );

    test_infer!(
        coerce_if_branches_else,
        r#"
fn f() -> _ {
    if true {5} else {loop{}}
}
        "#,
        Type::INT
    );

    test_infer!(
        coerce_match_branches1,
        r#"
fn f() -> _ {
    match 5 {
        foo => 5,
        bar => loop {},
    }
}
    "#,
        Type::INT
    );

    test_infer!(
        coerce_match_branches2,
        r#"
fn f() -> _ {
    match 5 {
        foo => loop {},
        bar => 5,
    }
}
    "#,
        Type::INT
    );

    test_infer!(
        struct_pat,
        r#"
struct Foo {x: Int, y: Int}
fn main() -> _ {
    let Foo {y,x} = Foo{x: 5, y: 10};
    x
}
    "#,
        Type::INT
    );

    test_infer!(
        named_struct_pat,
        r#"
struct Foo {x: Int, y: Int}
fn main() -> _ {
    let Foo {y,x: z} = Foo{x: 5, y: 10};
    z
}
    "#,
        Type::INT
    );
}
