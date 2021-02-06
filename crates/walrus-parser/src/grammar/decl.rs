use super::*;
use nom::multi::many0;

pub fn source_file(input: Input) -> IResult<SourceFile> {
    many0(decl)
        .map(|decls| SourceFile { decls })
        .all_consuming()
        .parse(input)
}

pub fn var(input: Input) -> IResult<Var> { (ident.map(Var)).parse(input) }

pub fn decl(input: Input) -> IResult<Decl> {
    fn_decl
        .map(Decl::Fn)
        .or(struct_decl.map(Decl::Struct))
        .parse(input)
}

fn fn_decl(input: Input) -> IResult<FnDef> {
    let (input, kw_fn) = kw_fn.parse(input)?;
    let (input, name) = var.parse(input)?;
    let (input, params) = param_list.parse(input)?;
    let (input, ret) = ret_type.opt().parse(input)?;
    let (input, expr) = block_expr.parse(input)?;
    Ok((
        input,
        FnDef {
            kw_fn,
            name,
            params,
            ret,
            expr,
        },
    ))
}

fn struct_decl(input: Input) -> IResult<StructDef> {
    let (input, kw_struct) = kw_struct.parse(input)?;
    let (input, name) = var.parse(input)?;
    let (input, fields) = curly(punctuated0(struct_field, comma)).parse(input)?;
    Ok((
        input,
        StructDef {
            kw_struct,
            name,
            fields,
        },
    ))
}

fn struct_field(input: Input) -> IResult<StructField> {
    let (input, name) = var.parse(input)?;
    let (input, colon) = colon.parse(input)?;
    let (input, ty) = ty.parse(input)?;
    Ok((input, StructField { name, colon, ty }))
}

pub fn param_list(input: Input) -> IResult<ParamList> {
    paren(punctuated0(param, comma)).map(ParamList).parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    test_parse!(empty_file, source_file, r#""#);
    test_parse!(empty_fn, source_file, r#"fn f() {}"#);
    test_parse!(empty_struct, source_file, r#"struct Foo {}"#);
    test_parse!(
        struct_with_fields,
        source_file,
        r#"struct Foo {x: Int, y: Float}"#
    );
}
