use crate::{hir::Var, ty::Type};
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinKind {
    Type,
    Value,
}

macro_rules! builtins {
    (
        $(
            $id:ident {
                name: $name:expr,
                kind: $kind:expr,
                ty: $ty:expr,
            }
        ),*
    ) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub enum Builtin {
            $($id),*
        }

        impl Builtin {
            pub const fn name(self) -> &'static str {
                match self {
                    $(Self::$id => $name),*
                }
            }

            pub const fn kind(self) -> BuiltinKind {
                match self {
                    $(Self::$id => $kind),*
                }
            }

            pub fn ty(self) -> Type {
                match self {
                    $(Self::$id => $ty),*
                }
            }

            pub fn lookup(var: &Var) -> Option<Self>{
                let builtin = match var.as_str() {
                    $($name => Self::$id),*,
                    _ => return None,
                };
                Some(builtin)
            }

            pub const fn all() -> &'static [Self] {
                &[$(Self::$id),*]
            }
        }
    };
}

builtins! {
    Bool {
        name: "Bool",
        kind: BuiltinKind::Type,
        ty: Type::BOOL,
    },
    Int {
        name: "Int",
        kind: BuiltinKind::Type,
        ty: Type::INT,
    },
    Float {
        name: "Float",
        kind: BuiltinKind::Type,
        ty: Type::FLOAT,
    },
    Char {
        name: "Char",
        kind: BuiltinKind::Type,
        ty: Type::CHAR,
    },
    Never {
        name: "Never",
        kind: BuiltinKind::Type,
        ty: Type::NEVER,
    },
    Exit {
        name: "exit",
        kind: BuiltinKind::Value,
        ty: Type::function(vec![Type::INT], Type::NEVER),
    },
    PutChar {
        name: "putchar",
        kind: BuiltinKind::Value,
        ty: Type::function(vec![Type::CHAR], Type::CHAR),
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.name()) }
}
