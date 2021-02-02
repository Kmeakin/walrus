use super::*;

pub fn decl(input: Input) -> IResult<Decl> { fn_decl.parse(input) }

fn fn_decl(input: Input) -> IResult<Decl> {
    let (input, kw_fn) = kw_fn.parse(input)?;
    let (input, params) = param_list.parse(input)?;
    let (input, ret) = ret_type.opt().parse(input)?;
    let (input, block) = block.parse(input)?;
    Ok((
        input,
        Decl::Fn {
            kw_fn,
            params,
            ret,
            block,
        },
    ))
}

pub fn param_list(input: Input) -> IResult<ParamList> {
    paren(punctuated0(param, comma)).map(ParamList).parse(input)
}
