use super::*;
use walrus_lexer::TokenKind;

fn token<'a, T>(kind: TokenKind, input: Input<'a>) -> IResult<T>
where
    T: From<Token<'a>>,
{
    match input.first() {
        Some(token) if token.kind == kind => Ok((&input[1..], T::from(*token))),
        _ => Err(nom::Err::Error(())),
    }
}
pub fn kw_break(input: Input) -> IResult<KwBreak> { token(TokenKind::KwBreak, input) }
pub fn kw_continue(input: Input) -> IResult<KwContinue> { token(TokenKind::KwContinue, input) }
pub fn kw_else(input: Input) -> IResult<KwElse> { token(TokenKind::KwElse, input) }
pub fn kw_false(input: Input) -> IResult<KwFalse> { token(TokenKind::KwFalse, input) }
pub fn kw_fn(input: Input) -> IResult<KwFn> { token(TokenKind::KwFn, input) }
pub fn kw_if(input: Input) -> IResult<KwIf> { token(TokenKind::KwIf, input) }
pub fn kw_import(input: Input) -> IResult<KwImport> { token(TokenKind::KwImport, input) }
pub fn kw_let(input: Input) -> IResult<KwLet> { token(TokenKind::KwLet, input) }
pub fn kw_loop(input: Input) -> IResult<KwLoop> { token(TokenKind::KwLoop, input) }
pub fn kw_return(input: Input) -> IResult<KwReturn> { token(TokenKind::KwReturn, input) }
pub fn kw_true(input: Input) -> IResult<KwTrue> { token(TokenKind::KwTrue, input) }

pub fn ident(input: Input) -> IResult<Ident> { token(TokenKind::Ident, input) }
pub fn dec_int(input: Input) -> IResult<DecInt> { token(TokenKind::DecInt, input) }
pub fn bin_int(input: Input) -> IResult<BinInt> { token(TokenKind::BinInt, input) }
pub fn hex_int(input: Input) -> IResult<HexInt> { token(TokenKind::HexInt, input) }
pub fn float(input: Input) -> IResult<Float> { token(TokenKind::Float, input) }
pub fn simple_char(input: Input) -> IResult<SimpleChar> { token(TokenKind::SimpleChar, input) }
pub fn escaped_char(input: Input) -> IResult<EscapedChar> { token(TokenKind::EscapedChar, input) }
pub fn unicode_char(input: Input) -> IResult<UnicodeChar> { token(TokenKind::UnicodeChar, input) }

pub fn lparen(input: Input) -> IResult<LParen> { token(TokenKind::LParen, input) }
pub fn rparen(input: Input) -> IResult<RParen> { token(TokenKind::RParen, input) }
pub fn lcurly(input: Input) -> IResult<LCurly> { token(TokenKind::LCurly, input) }
pub fn rcurly(input: Input) -> IResult<RCurly> { token(TokenKind::RCurly, input) }
pub fn colon(input: Input) -> IResult<Colon> { token(TokenKind::Colon, input) }
pub fn comma(input: Input) -> IResult<Comma> { token(TokenKind::Comma, input) }
pub fn dot(input: Input) -> IResult<Dot> { token(TokenKind::Dot, input) }
pub fn eq(input: Input) -> IResult<Eq> { token(TokenKind::Eq, input) }
pub fn fat_arrow(input: Input) -> IResult<FatArrow> { token(TokenKind::FatArrow, input) }
pub fn semicolon(input: Input) -> IResult<Semicolon> { token(TokenKind::Semicolon, input) }
pub fn thin_arrow(input: Input) -> IResult<ThinArrow> { token(TokenKind::ThinArrow, input) }
pub fn underscore(input: Input) -> IResult<Underscore> { token(TokenKind::Underscore, input) }

pub fn bang(input: Input) -> IResult<Bang> { token(TokenKind::Bang, input) }
pub fn and_and(input: Input) -> IResult<AndAnd> { token(TokenKind::AndAnd, input) }
pub fn or_or(input: Input) -> IResult<OrOr> { token(TokenKind::OrOr, input) }

pub fn plus(input: Input) -> IResult<Plus> { token(TokenKind::Plus, input) }
pub fn minus(input: Input) -> IResult<Minus> { token(TokenKind::Minus, input) }
pub fn star(input: Input) -> IResult<Star> { token(TokenKind::Star, input) }
pub fn slash(input: Input) -> IResult<Slash> { token(TokenKind::Slash, input) }
pub fn eq_eq(input: Input) -> IResult<EqEq> { token(TokenKind::EqEq, input) }
pub fn bang_eq(input: Input) -> IResult<BangEq> { token(TokenKind::BangEq, input) }
pub fn less(input: Input) -> IResult<Less> { token(TokenKind::Less, input) }
pub fn less_eq(input: Input) -> IResult<LessEq> { token(TokenKind::LessEq, input) }
pub fn greater(input: Input) -> IResult<Greater> { token(TokenKind::Greater, input) }
pub fn greater_eq(input: Input) -> IResult<GreaterEq> { token(TokenKind::GreaterEq, input) }
