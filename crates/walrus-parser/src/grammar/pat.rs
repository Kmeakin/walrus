use super::*;

pub fn pat(input: Input) -> IResult<Pat> {
    bind_pat
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

fn bind_pat(input: Input) -> IResult<Pat> { var.map(Pat::Var).parse(input) }
fn ignore_pat(input: Input) -> IResult<Pat> { underscore.map(Pat::Ignore).parse(input) }
fn paren_pat(input: Input) -> IResult<Pat> { paren(pat).map(Pat::Paren).parse(input) }
fn tuple_pat(input: Input) -> IResult<Pat> { tuple(pat).map(Pat::Tuple).parse(input) }
