use super::*;

pub fn ty(input: Input) -> IResult<Type> {
    path_type
        .or(infer_type)
        .or(paren_type)
        .or(tuple_type)
        .or(fn_type)
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
