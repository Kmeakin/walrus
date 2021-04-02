use crate::{
    hir::Var,
    ty::{FnType, PrimitiveType, Type},
};
use std::{fmt::Display, lazy::Lazy};

const BUILTINS: Lazy<Vec<Builtin>> = Lazy::new(|| {
    vec![
        Builtin::Type {
            name: "Bool",
            ty: PrimitiveType::Bool,
        },
        Builtin::Type {
            name: "Int",
            ty: PrimitiveType::Int,
        },
        Builtin::Type {
            name: "Float",
            ty: PrimitiveType::Float,
        },
        Builtin::Type {
            name: "Char",
            ty: PrimitiveType::Char,
        },
        Builtin::Type {
            name: "Never",
            ty: PrimitiveType::Never,
        },
        Builtin::Fn {
            name: "exit",
            ty: FnType::new(&[Type::INT], Type::NEVER),
        },
        Builtin::Fn {
            name: "putchar",
            ty: FnType::new(&[Type::CHAR], Type::UNIT),
        },
    ]
});

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Builtin {
    Type {
        name: &'static str,
        ty: PrimitiveType,
    },
    Fn {
        name: &'static str,
        ty: FnType,
    },
}

impl Builtin {
    pub fn lookup(var: &Var) -> Option<Builtin> {
        BUILTINS.iter().find(|b| b.name() == var.as_str()).cloned()
    }

    pub fn name(&self) -> &str {
        match self {
            Builtin::Type { name, .. } | Builtin::Fn { name, .. } => name,
        }
    }

    pub fn ty(self) -> Type {
        match self {
            Self::Type { ty, .. } => ty.into(),
            Self::Fn { ty, .. } => ty.into(),
        }
    }

    pub fn is_type(&self) -> bool { matches!(self, Self::Type { .. }) }
    pub fn is_value(&self) -> bool { matches!(self, Self::Fn { .. }) }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Builtin::Type { .. } => write!(f, "builtin type"),
            Builtin::Fn { .. } => write!(f, "builtin function"),
        }
    }
}
