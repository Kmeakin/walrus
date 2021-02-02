use super::*;

pub fn ty(input: Input) -> IResult<Type> {
    path_type
        .or(infer_type)
        .or(fn_type)
        .or(paren_type)
        .or(tuple_type)
        .parse(input)
}

pub fn ascription(input: Input) -> IResult<Ascription> {
    let (input, colon) = colon.parse(input)?;
    let (input, ty) = ty.parse(input)?;
    Ok((input, Ascription { colon, ty }))
}

pub fn ret_type(input: Input) -> IResult<RetType> {
    let (input, thin_arrow) = thin_arrow.parse(input)?;
    let (input, ty) = ty.parse(input)?;
    Ok((
        input,
        RetType {
            thin_arrow,
            ty: box ty,
        },
    ))
}

fn path_type(input: Input) -> IResult<Type> { path.map(Type::Path).parse(input) }
fn infer_type(input: Input) -> IResult<Type> { underscore.map(Type::Infer).parse(input) }
fn paren_type(input: Input) -> IResult<Type> { paren(ty).map(Type::Paren).parse(input) }
fn tuple_type(input: Input) -> IResult<Type> { tuple(ty).map(Type::Tuple).parse(input) }
fn fn_type(input: Input) -> IResult<Type> {
    let (input, args) = paren(punctuated0(ty, comma)).parse(input)?;
    let (input, ret) = ret_type.parse(input)?;
    Ok((input, Type::Fn { args, ret }))
}

#[cfg(test)]
mod tests {
    use super::*;

    test_parse!(path_type, ty, r#"x::y::Z"#);
    test_parse!(infer_type, ty, r#"_"#);
    test_parse!(tuple0_type, ty, r#"()"#);
    test_parse!(tuple1_type, ty, r#"(T,)"#);
    test_parse!(tuple2_type, ty, r#"(T,V)"#);
    test_parse!(paren_type, ty, r#"(T)"#);
    test_parse!(fn_type, ty, r#"(T) -> V"#);
    test_parse!(nested_fn_type, ty, r#"(A) -> (B) -> C"#);
}
