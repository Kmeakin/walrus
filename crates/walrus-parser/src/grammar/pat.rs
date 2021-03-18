use nom::sequence::pair;

use super::*;

pub fn pat(input: Input) -> IResult<Pat> {
    struct_pat
        .or(enum_pat)
        .or(var_pat)
        .or(ignore_pat)
        .or(paren_pat)
        .or(tuple_pat)
        .parse(input)
}

pub fn param(input: Input) -> IResult<Param> {
    let (input, pat) = pat.parse(input)?;
    let (input, ascription) = ascription.opt().parse(input)?;
    Ok((input, Param { pat, ascription }))
}

fn var_pat(input: Input) -> IResult<Pat> { var.map(Pat::Var).parse(input) }
fn ignore_pat(input: Input) -> IResult<Pat> { underscore.map(Pat::Ignore).parse(input) }
fn paren_pat(input: Input) -> IResult<Pat> { paren(pat).map(Pat::Paren).parse(input) }
fn tuple_pat(input: Input) -> IResult<Pat> { tuple(pat).map(Pat::Tuple).parse(input) }
fn struct_pat(input: Input) -> IResult<Pat> {
    let (input, name) = var.parse(input)?;
    let (input, fields) = curly(punctuated0(field_pat, comma)).parse(input)?;
    Ok((input, Pat::Struct(StructPat { name, fields })))
}
fn enum_pat(input: Input) -> IResult<Pat> {
    let (input, name) = var.parse(input)?;
    let (input, colon_colon) = colon_colon.parse(input)?;
    let (input, variant) = var.parse(input)?;
    let (input, fields) = curly(punctuated0(field_pat, comma)).parse(input)?;
    Ok((
        input,
        Pat::Enum(EnumPat {
            name,
            colon_colon,
            variant,
            fields,
        }),
    ))
}

fn field_pat(input: Input) -> IResult<FieldPat> {
    let (input, name) = var.parse(input)?;
    let (input, pat) = (pair(colon, pat)).opt().parse(input)?;
    Ok((input, FieldPat { name, pat }))
}

#[cfg(test)]
mod tests {
    use super::*;

    test_parse!(var_pat, pat, r#"a"#);
    test_parse!(ignore_pat, pat, r#"_"#);
    test_parse!(tuple0_pat, pat, r#"()"#);
    test_parse!(tuple1_pat, pat, r#"(x,)"#);
    test_parse!(tuple2_pat, pat, r#"(x,y)"#);
    test_parse!(paren_pat, pat, r#"(x)"#);
    test_parse!(struct_pat, pat, r#"Foo {x: y, z}"#);
    test_parse!(enum_pat, pat, r#"Foo::Bar {x: y, z}"#);
}
