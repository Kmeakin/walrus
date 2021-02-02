use super::*;
use nom::multi::many0;

pub fn source_file(input: Input) -> IResult<SourceFile> {
    many0(decl).map(|decls| SourceFile { decls }).parse(input)
}

pub fn var(input: Input) -> IResult<Var> { (ident.map(Var)).parse(input) }

pub fn decl(input: Input) -> IResult<Decl> { fn_decl.parse(input) }

fn fn_decl(input: Input) -> IResult<Decl> {
    let (input, kw_fn) = kw_fn.parse(input)?;
    let (input, name) = var.parse(input)?;
    let (input, params) = param_list.parse(input)?;
    let (input, ret) = ret_type.opt().parse(input)?;
    let (input, block) = block.parse(input)?;
    Ok((
        input,
        Decl::Fn(FnDef {
            kw_fn,
            name,
            params,
            ret,
            block,
        }),
    ))
}

pub fn param_list(input: Input) -> IResult<ParamList> {
    paren(punctuated0(param, comma)).map(ParamList).parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    test_parse!(empty_file, source_file, r#""#);
    test_parse!(empty_fn, source_file, r#"fn f() {}"#);
}
