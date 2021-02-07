use super::*;
use ena::unify::{InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};

#[derive(Clone, Debug, Default)]
pub struct InferenceTable {
    pub(super) var_unification_table: InPlaceUnificationTable<TypeVarId>,
}

/// The ID of a type variable.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub(super) u32);

impl UnifyKey for TypeVarId {
    type Value = TypeVarValue;
    fn index(&self) -> u32 { self.0 }
    fn from_index(i: u32) -> Self { Self(i) }
    fn tag() -> &'static str { "TypeVarId" }
}

/// The value of a type variable: either we already know the type, or we don't
/// know it yet.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypeVarValue {
    Known(Type),
    Unknown,
}

impl TypeVarValue {
    const fn as_known(&self) -> Option<&Type> {
        match self {
            Self::Known(ty) => Some(ty),
            Self::Unknown => None,
        }
    }
}

impl UnifyValue for TypeVarValue {
    type Error = NoError;

    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, NoError> {
        match (value1, value2) {
            // We should never equate two type variables, both of which have
            // known types. Instead, we recursively equate those types.
            (Self::Known(t1), Self::Known(t2)) => panic!(
                "equating two type variables, both of which have known types: {:?} and {:?}",
                t1, t2
            ),

            // If one side is known, prefer that one.
            (Self::Known(..), Self::Unknown) => Ok(value1.clone()),
            (Self::Unknown, Self::Known(..)) => Ok(value2.clone()),

            (Self::Unknown, Self::Unknown) => Ok(Self::Unknown),
        }
    }
}

impl InferenceTable {
    pub fn new_type_var(&mut self) -> TypeVarId {
        self.var_unification_table.new_key(TypeVarValue::Unknown)
    }

    /// Propagates the type completely; type variables without known type are
    /// replaced by `Type::Unknown`.
    pub(crate) fn propagate_type_completely(&mut self, ty: &Type) -> Type {
        self.propagate_type_completely_inner(&mut Vec::new(), ty)
    }

    pub(crate) fn propagate_type_completely_inner(
        &mut self,
        tv_stack: &mut Vec<TypeVarId>,
        ty: &Type,
    ) -> Type {
        ty.clone().fold(&mut |ty| match ty {
            Type::Infer(tv) => {
                let inner = tv.to_inner();
                if tv_stack.contains(&inner) {
                    return tv.fallback_value();
                }
                self.var_unification_table
                    .inlined_probe_value(inner)
                    .as_known()
                    .map_or_else(
                        || tv.fallback_value(),
                        |known_ty| {
                            // known_ty may contain other variables that are known by now
                            tv_stack.push(inner);
                            let result = self.propagate_type_completely_inner(tv_stack, known_ty);
                            tv_stack.pop();
                            result
                        },
                    )
            }
            _ => ty,
        })
    }

    /// Propagates the type as far as currently possible, replacing type
    /// variables by their known types. All types returned by the `infer_*`
    /// functions should be propagated as far as possible, i.e. contain no
    /// type variables with known type.
    pub(crate) fn propagate_type_as_far_as_possible(&mut self, ty: &Type) -> Type {
        self.propagate_type_as_far_as_possible_inner(&mut Vec::new(), ty)
    }

    pub(crate) fn propagate_type_as_far_as_possible_inner(
        &mut self,
        tv_stack: &mut Vec<TypeVarId>,
        ty: &Type,
    ) -> Type {
        ty.clone().fold(&mut |ty| match ty {
            Type::Infer(tv) => {
                let inner = tv.to_inner();
                if tv_stack.contains(&inner) {
                    return tv.fallback_value();
                }
                self.var_unification_table
                    .inlined_probe_value(inner)
                    .as_known()
                    .map_or(ty, |known_ty| {
                        tv_stack.push(inner);
                        let result =
                            self.propagate_type_as_far_as_possible_inner(tv_stack, known_ty);
                        tv_stack.pop();
                        result
                    })
            }
            _ => ty,
        })
    }

    fn propagate_type_shallow(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Infer(tv) => {
                let inner = tv.to_inner();
                let value = self.var_unification_table.inlined_probe_value(inner);
                match value.as_known() {
                    Some(known_ty) => known_ty.clone(),
                    None => ty.clone(),
                }
            }
            _ => ty.clone(),
        }
    }

    pub fn unify(&mut self, t1: &Type, t2: &Type) -> bool {
        if t1 == t2 {
            return true;
        }

        let t1 = self.propagate_type_shallow(&t1.clone());
        let t2 = self.propagate_type_shallow(&t2.clone());
        match (t1, t2) {
            (
                Type::App {
                    ctor: ctor1,
                    params: params1,
                },
                Type::App {
                    ctor: ctor2,
                    params: params2,
                },
            ) if ctor1 == ctor2 => {
                params1.len() == params2.len()
                    && (params1.iter())
                        .zip(params2.iter())
                        .all(|(t1, t2)| self.unify(t1, t2))
            }
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Infer(InferType::Var(tv1)), Type::Infer(InferType::Var(tv2))) => {
                self.var_unification_table.union(tv1, tv2);
                true
            }

            (Type::Infer(InferType::Var(tv)), other) | (other, Type::Infer(InferType::Var(tv))) => {
                self.var_unification_table
                    .union_value(tv, TypeVarValue::Known(other));
                true
            }

            // below Type::Infer so that
            // unify(TypeVar, Never) => Never, instead of
            // unify(TypeVar, Never) => TypeVar
            (ty, _) | (_, ty) if ty == Type::NEVER => true,

            _ => false,
        }
    }
}
