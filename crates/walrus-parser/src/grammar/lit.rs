use super::*;

/// DecInt ::= '[0-9]'
/// DecInt_ ::= '[0-9_]'
/// BinInt ::= '[0-1]'
/// BinInt_ ::= '[0-1_]'
/// HexInt ::= '[0-9a-fA-F]'
/// HexInt_ ::= '[0-9a-fA-F_]'
/// DecInt ::= DecDigit DecDigit_*
/// BinInt ::= BinDigit BinDigit_*
/// HexInt ::= HexDigit HexDigit_*
/// Float  ::= DecInt "." DecInt
/// SimpleChar ::= '[^']'
/// EscapedChar ::= '\\.'
/// UnicodeChar ::= '\\u HexInt HexInt? HexInt? HexInt?'
pub fn lit(input: Input) -> IResult<Lit> {
    bool_lit
        .map(Lit::Bool)
        .or(int_lit.map(Lit::Int))
        .or(float_lit.map(Lit::Float))
        .or(char_lit.map(Lit::Char))
        .parse(input)
}
fn bool_lit(input: Input) -> IResult<BoolLit> {
    (kw_true.map(BoolLit::True))
        .or(kw_false.map(BoolLit::False))
        .parse(input)
}
fn int_lit(input: Input) -> IResult<IntLit> {
    (dec_int.map(IntLit::Dec))
        .or(bin_int.map(IntLit::Bin))
        .or(hex_int.map(IntLit::Hex))
        .parse(input)
}
fn float_lit(input: Input) -> IResult<FloatLit> { float.map(FloatLit).parse(input) }
fn char_lit(input: Input) -> IResult<CharLit> {
    (simple_char.map(CharLit::Simple))
        .or(escaped_char.map(CharLit::Escaped))
        .or(unicode_char.map(CharLit::Unicode))
        .parse(input)
}

#[cfg(test)]
mod tests {
    use super::{lit, test_parse};

    test_parse!(true_lit, lit, "true");
    test_parse!(false_lit, lit, "false");
    test_parse!(dec_int_lit, lit, "1234");
    test_parse!(bin_int_lit, lit, "0b1010");
    test_parse!(hex_int_lit, lit, "0x123abcdef");
    test_parse!(float_lit, lit, "1234.56789");
    test_parse!(simple_char_lit, lit, "'a'");
    test_parse!(escaped_char_lit, lit, r"'\a'");
    test_parse!(unicode_char_lit, lit, r"'\u{0a}'");
}
