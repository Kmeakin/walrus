use crate::{hir::Var, ty::Type};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Builtin {
    Value(BuiltinValue),
    Type(BuiltinType),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Bool,
    Int,
    Float,
    Char,
    Never,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinValue {
    Exit,
}

impl From<BuiltinType> for Type {
    fn from(other: BuiltinType) -> Self {
        match other {
            BuiltinType::Bool => Self::BOOL,
            BuiltinType::Int => Self::INT,
            BuiltinType::Float => Self::FLOAT,
            BuiltinType::Char => Self::CHAR,
            BuiltinType::Never => Self::NEVER,
        }
    }
}

impl BuiltinValue {
    pub fn ty(self) -> Type {
        match self {
            Self::Exit => Type::function(vec![Type::INT], Type::NEVER),
        }
    }
}

pub fn lookup(var: &Var) -> Option<Builtin> {
    let builtin = match var.as_str() {
        "Bool" => Builtin::Type(BuiltinType::Bool),
        "Int" => Builtin::Type(BuiltinType::Int),
        "Float" => Builtin::Type(BuiltinType::Float),
        "Char" => Builtin::Type(BuiltinType::Char),
        "Never" => Builtin::Type(BuiltinType::Never),
        "exit" => Builtin::Value(BuiltinValue::Exit),
        _ => return None,
    };
    Some(builtin)
}
