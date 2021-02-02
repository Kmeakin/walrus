use super::*;
use nom::{multi::many0, sequence::pair};

pub fn paren<'a, InnerP, Inner>(inner: InnerP) -> impl Parser<Input<'a>, Paren<Inner>, Err>
where
    InnerP: Parser<Input<'a>, Inner, Err>,
{
    delimited(lparen, inner, rparen)
}

pub fn tuple<'a, InnerP, Inner>(inner: InnerP) -> impl Parser<Input<'a>, Tuple<Inner>, Err>
where
    InnerP: Parser<Input<'a>, Inner, Err>,
{
    paren(punctuated0(inner, comma))
}

pub fn curly<'a, InnerP, Inner>(inner: InnerP) -> impl Parser<Input<'a>, Curly<Inner>, Err>
where
    InnerP: Parser<Input<'a>, Inner, Err>,
{
    delimited(lcurly, inner, rcurly)
}

pub const fn delimited<Open, Inner, Close>(
    open: Open,
    inner: Inner,
    close: Close,
) -> DelimitedP<Open, Inner, Close> {
    DelimitedP { open, inner, close }
}

pub fn punctuated0<'a, Inner, Sep, InnerP, SepP>(
    mut inner: InnerP,
    mut sep: SepP,
) -> impl FnMut(Input<'a>) -> IResult<Punctuated0<Inner, Sep>>
where
    InnerP: Parser<Input<'a>, Inner, Err>,
    SepP: Parser<Input<'a>, Sep, Err>,
{
    move |input| match punctuated1(inner.by_ref(), sep.by_ref()).opt().parse(input) {
        Ok((input, Some(Punctuated1 { first, tail, trail }))) => Ok((
            input,
            Punctuated0 {
                first: Some(first),
                tail,
                trail,
            },
        )),
        Ok((input, None)) => Ok((input, Punctuated0::default())),
        Err(nom::Err::Error(_)) => Ok((input, Punctuated0::default())),
        Err(e) => Err(e),
    }
}

pub fn punctuated0_no_trail<'a, Inner, Sep, InnerP, SepP>(
    mut inner: InnerP,
    mut sep: SepP,
) -> impl FnMut(Input<'a>) -> IResult<Punctuated0NoTrail<Inner, Sep>>
where
    InnerP: Parser<Input<'a>, Inner, Err>,
    SepP: Parser<Input<'a>, Sep, Err>,
{
    move |input| match punctuated1_no_trail(inner.by_ref(), sep.by_ref())
        .opt()
        .parse(input)
    {
        Ok((input, Some(Punctuated1NoTrail { first, tail }))) => Ok((
            input,
            Punctuated0NoTrail {
                first: Some(first),
                tail,
            },
        )),
        Ok((input, None)) => Ok((input, Punctuated0NoTrail::default())),
        Err(nom::Err::Error(_)) => Ok((input, Punctuated0NoTrail::default())),
        Err(e) => Err(e),
    }
}

pub fn punctuated1<'a, Inner, Sep, InnerP, SepP>(
    mut inner: InnerP,
    mut sep: SepP,
) -> impl FnMut(Input<'a>) -> IResult<Punctuated1<Inner, Sep>>
where
    InnerP: Parser<Input<'a>, Inner, Err>,
    SepP: Parser<Input<'a>, Sep, Err>,
{
    move |input| {
        let (input, first) = inner.parse(input)?;
        let (input, tail) = many0(pair(sep.by_ref(), inner.by_ref())).parse(input)?;
        let (input, trail) = sep.by_ref().opt().parse(input)?;
        Ok((input, Punctuated1 { first, tail, trail }))
    }
}

pub fn punctuated1_no_trail<'a, Inner, Sep, InnerP, SepP>(
    mut inner: InnerP,
    mut sep: SepP,
) -> impl FnMut(Input<'a>) -> IResult<Punctuated1NoTrail<Inner, Sep>>
where
    InnerP: Parser<Input<'a>, Inner, Err>,
    SepP: Parser<Input<'a>, Sep, Err>,
{
    move |input| {
        let (input, first) = inner.parse(input)?;
        let (input, tail) = many0(pair(sep.by_ref(), inner.by_ref())).parse(input)?;
        Ok((input, Punctuated1NoTrail { first, tail }))
    }
}

pub struct DelimitedP<Open, Inner, Close> {
    open: Open,
    inner: Inner,
    close: Close,
}

impl<I, E, Open, Inner, Close, OpenP, InnerP, CloseP> Parser<I, Delimited<Inner, Open, Close>, E>
    for DelimitedP<OpenP, InnerP, CloseP>
where
    OpenP: Parser<I, Open, E>,
    CloseP: Parser<I, Close, E>,
    InnerP: Parser<I, Inner, E>,
{
    fn parse(&mut self, input: I) -> nom::IResult<I, Delimited<Inner, Open, Close>, E> {
        let (input, open) = self.open.parse(input)?;
        let (input, inner) = self.inner.parse(input)?;
        let (input, close) = self.close.parse(input)?;
        Ok((
            input,
            Delimited {
                open,
                inner: box inner,
                close,
            },
        ))
    }
}
