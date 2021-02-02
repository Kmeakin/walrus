use super::*;

pub fn pat(input: Input) -> IResult<Pat> {
    var_pat
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

#[cfg(test)]
mod tests {
    use super::*;

    test_parse!(var_pat, pat, r#"a"#);
    test_parse!(ignore_pat, pat, r#"_"#);
    test_parse!(tuple0_pat, pat, r#"()"#);
    test_parse!(tuple1_pat, pat, r#"(x,)"#);
    test_parse!(tuple2_pat, pat, r#"(x,y)"#);
    test_parse!(paren_pat, pat, r#"(x)"#);
}
