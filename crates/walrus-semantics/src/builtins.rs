use crate::{
    hir::Var,
    ty::{FnType, PrimitiveType, Type},
};
use std::fmt::Display;

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
    pub fn lookup(var: &Var) -> Option<Self> {
        Self::all()
            .iter()
            .find(|b| b.name() == var.as_str())
            .cloned()
    }

    pub const fn name(&self) -> &str {
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

    pub const fn is_type(&self) -> bool { matches!(self, Self::Type { .. }) }
    pub const fn is_value(&self) -> bool { matches!(self, Self::Fn { .. }) }

    pub fn all() -> Vec<Self> {
        vec![
            Self::Type {
                name: "Bool",
                ty: PrimitiveType::Bool,
            },
            Self::Type {
                name: "Int",
                ty: PrimitiveType::Int,
            },
            Self::Type {
                name: "Float",
                ty: PrimitiveType::Float,
            },
            Self::Type {
                name: "Char",
                ty: PrimitiveType::Char,
            },
            Self::Type {
                name: "String",
                ty: PrimitiveType::String,
            },
            Self::Type {
                name: "Never",
                ty: PrimitiveType::Never,
            },
            Self::Fn {
                name: "sqrt",
                ty: FnType::new(&[Type::FLOAT], Type::FLOAT),
            },
            Self::Fn {
                name: "exit",
                ty: FnType::new(&[Type::INT], Type::NEVER),
            },
            Self::Fn {
                name: "print",
                ty: FnType::new(&[Type::STRING], Type::UNIT),
            },
            Self::Fn {
                name: "print_error",
                ty: FnType::new(&[Type::STRING], Type::UNIT),
            },
            Self::Fn {
                name: "string_length",
                ty: FnType::new(&[Type::STRING], Type::INT),
            },
            Self::Fn {
                name: "bool_to_string",
                ty: FnType::new(&[Type::BOOL], Type::STRING),
            },
            Self::Fn {
                name: "int_to_string",
                ty: FnType::new(&[Type::INT], Type::STRING),
            },
            Self::Fn {
                name: "float_to_string",
                ty: FnType::new(&[Type::FLOAT], Type::STRING),
            },
            Self::Fn {
                name: "char_to_string",
                ty: FnType::new(&[Type::CHAR], Type::STRING),
            },
            Self::Fn {
                name: "string_append",
                ty: FnType::new(&[Type::STRING, Type::STRING], Type::STRING),
            },
            Self::Fn {
                name: "string_cmp",
                ty: FnType::new(&[Type::STRING, Type::STRING], Type::INT),
            },
        ]
    }

    /// # Panics
    /// panics if passed a name that does not exist
    pub fn get_by_name(n: &str) -> Self {
        Self::all().iter().find(|b| b.name() == n).cloned().unwrap()
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Builtin::Type { .. } => write!(f, "builtin type"),
            Builtin::Fn { .. } => write!(f, "builtin function"),
        }
    }
}
