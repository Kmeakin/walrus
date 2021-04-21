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

macro_rules! token_parser {
    ($name:ident, $kind:ident) => {
        pub fn $name(input: Input) -> IResult<$kind> { token(TokenKind::$kind, input) }
    };
}

token_parser!(kw_break, KwBreak);
token_parser!(kw_continue, KwContinue);
token_parser!(kw_else, KwElse);
token_parser!(kw_enum, KwEnum);
token_parser!(kw_false, KwFalse);
token_parser!(kw_fn, KwFn);
token_parser!(kw_if, KwIf);
token_parser!(kw_let, KwLet);
token_parser!(kw_loop, KwLoop);
token_parser!(kw_match, KwMatch);
token_parser!(kw_mut, KwMut);
token_parser!(kw_return, KwReturn);
token_parser!(kw_struct, KwStruct);
token_parser!(kw_true, KwTrue);

token_parser!(ident, Ident);
token_parser!(dec_int, DecInt);
token_parser!(bin_int, BinInt);
token_parser!(hex_int, HexInt);
token_parser!(float, Float);
token_parser!(string, String);

token_parser!(simple_char, SimpleChar);
token_parser!(escaped_char, EscapedChar);
token_parser!(unicode_char, UnicodeChar);

token_parser!(lparen, LParen);
token_parser!(rparen, RParen);
token_parser!(lcurly, LCurly);
token_parser!(rcurly, RCurly);
token_parser!(colon, Colon);
token_parser!(colon_colon, ColonColon);
token_parser!(comma, Comma);
token_parser!(dot, Dot);
token_parser!(eq, Eq);
token_parser!(fat_arrow, FatArrow);
token_parser!(semicolon, Semicolon);
token_parser!(thin_arrow, ThinArrow);
token_parser!(underscore, Underscore);

token_parser!(bang, Bang);
token_parser!(and_and, AndAnd);
token_parser!(or_or, OrOr);

token_parser!(plus, Plus);
token_parser!(minus, Minus);
token_parser!(star, Star);
token_parser!(slash, Slash);
token_parser!(percent, Percent);
token_parser!(eq_eq, EqEq);
token_parser!(bang_eq, BangEq);
token_parser!(less, Less);
token_parser!(less_eq, LessEq);
token_parser!(greater, Greater);
token_parser!(greater_eq, GreaterEq);
