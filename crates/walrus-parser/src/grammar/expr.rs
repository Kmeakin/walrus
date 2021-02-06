use super::*;
use nom::{
    multi::{fold_many0, many0},
    sequence::pair,
};

// See `https://doc.rust-lang.org/reference/expressions.html#expression-precedence`
// for expression precedence

pub fn expr(input: Input) -> IResult<Expr> {
    lambda_expr
        .or(struct_expr)
        .or(return_expr)
        .or(break_expr)
        .or(continue_expr)
        .or(assign_expr)
        .or(cmp_expr)
        .parse(input)
}
pub fn expr_no_struct(input: Input) -> IResult<Expr> {
    lambda_expr
        .or(return_expr)
        .or(break_expr)
        .or(continue_expr)
        .or(assign_expr)
        .or(cmp_expr)
        .parse(input)
}
fn lambda_expr(input: Input) -> IResult<Expr> {
    let (input, params) = param_list.parse(input)?;
    let (input, fat_arrow) = fat_arrow.parse(input)?;
    let (input, expr) = expr.parse(input)?;
    Ok((
        input,
        Expr::Lambda(LambdaExpr {
            params,
            fat_arrow,
            expr: box expr,
        }),
    ))
}
fn struct_expr(input: Input) -> IResult<Expr> {
    let (input, name) = var.parse(input)?;
    let (input, fields) = curly(punctuated0(struct_expr_field, comma)).parse(input)?;
    Ok((input, Expr::Struct(StructExpr { name, fields })))
}
fn struct_expr_field(input: Input) -> IResult<StructExprField> {
    let (input, name) = var.parse(input)?;
    let (input, colon) = colon.parse(input)?;
    let (input, val) = expr.parse(input)?;
    Ok((input, StructExprField { name, colon, val }))
}
fn return_expr(input: Input) -> IResult<Expr> {
    let (input, kw_return) = kw_return.parse(input)?;
    let (input, expr) = expr.opt().parse(input)?;
    Ok((
        input,
        Expr::Return(ReturnExpr {
            kw_return,
            expr: expr.map(Box::new),
        }),
    ))
}
fn break_expr(input: Input) -> IResult<Expr> {
    let (input, kw_break) = kw_break.parse(input)?;
    let (input, expr) = expr.opt().parse(input)?;
    Ok((
        input,
        Expr::Break(BreakExpr {
            kw_break,
            expr: expr.map(Box::new),
        }),
    ))
}
fn continue_expr(input: Input) -> IResult<Expr> {
    let (input, kw_continue) = kw_continue.parse(input)?;
    Ok((input, Expr::Continue(ContinueExpr { kw_continue })))
}
fn assign_op(input: Input) -> IResult<Binop> { eq.map(Binop::Assign).parse(input) }
fn assign_expr(input: Input) -> IResult<Expr> {
    let (input, lhs) = or_expr.parse(input)?;
    let (input, op) = assign_op.parse(input)?;
    let (input, rhs) = expr.parse(input)?;
    Ok((
        input,
        Expr::Binary(BinaryExpr {
            lhs: box lhs,
            op,
            rhs: box rhs,
        }),
    ))
}
fn or_op(input: Input) -> IResult<Binop> { or_or.map(Binop::Or).parse(input) }
fn or_expr(input: Input) -> IResult<Expr> {
    let (input, init) = and_expr.parse(input)?;
    fold_many0(pair(or_op, add_expr), init, |lhs, (op, rhs)| {
        Expr::Binary(BinaryExpr {
            lhs: box lhs,
            op,
            rhs: box rhs,
        })
    })
    .parse(input)
}
fn and_op(input: Input) -> IResult<Binop> { and_and.map(Binop::And).parse(input) }
fn and_expr(input: Input) -> IResult<Expr> {
    let (input, init) = cmp_expr.parse(input)?;
    fold_many0(pair(and_op, add_expr), init, |lhs, (op, rhs)| {
        Expr::Binary(BinaryExpr {
            lhs: box lhs,
            op,
            rhs: box rhs,
        })
    })
    .parse(input)
}
fn cmp_op(input: Input) -> IResult<Binop> {
    (eq_eq.map(Binop::Eq))
        .or(bang_eq.map(Binop::NotEq))
        .or(less.map(Binop::Less))
        .or(less_eq.map(Binop::LessEq))
        .or(greater.map(Binop::Greater))
        .or(greater_eq.map(Binop::GreaterEq))
        .parse(input)
}
fn cmp_expr(input: Input) -> IResult<Expr> {
    let (input, init) = add_expr.parse(input)?;
    fold_many0(pair(cmp_op, add_expr), init, |lhs, (op, rhs)| {
        Expr::Binary(BinaryExpr {
            lhs: box lhs,
            op,
            rhs: box rhs,
        })
    })
    .parse(input)
}
fn add_op(input: Input) -> IResult<Binop> {
    (plus.map(Binop::Add))
        .or(minus.map(Binop::Sub))
        .parse(input)
}
fn add_expr(input: Input) -> IResult<Expr> {
    let (input, init) = mul_expr.parse(input)?;
    fold_many0(pair(add_op, mul_expr), init, |lhs, (op, rhs)| {
        Expr::Binary(BinaryExpr {
            lhs: box lhs,
            op,
            rhs: box rhs,
        })
    })
    .parse(input)
}
fn mul_op(input: Input) -> IResult<Binop> {
    (star.map(Binop::Mul))
        .or(slash.map(Binop::Div))
        .parse(input)
}
fn mul_expr(input: Input) -> IResult<Expr> {
    let (input, init) = unary_expr.parse(input)?;
    fold_many0(pair(mul_op, unary_expr), init, |lhs, (op, rhs)| {
        Expr::Binary(BinaryExpr {
            lhs: box lhs,
            op,
            rhs: box rhs,
        })
    })
    .parse(input)
}
fn unary_op(input: Input) -> IResult<Unop> {
    bang.map(Unop::Not)
        .or(plus.map(Unop::Add))
        .or(minus.map(Unop::Sub))
        .parse(input)
}
fn unary_expr(input: Input) -> IResult<Expr> {
    pair(unary_op, unary_expr)
        .map(|(op, expr)| Expr::Unary(UnaryExpr { op, expr: box expr }))
        .or(suffix_expr)
        .parse(input)
}
enum Suffix {
    Call(ArgList),
    Field(Dot, Field),
}
fn arg_list(input: Input) -> IResult<ArgList> {
    paren(punctuated0(expr, comma)).map(ArgList).parse(input)
}
fn field(input: Input) -> IResult<Field> {
    (dec_int.map(Field::Tuple))
        .or(var.map(Field::Named))
        .parse(input)
}
fn suffix(input: Input) -> IResult<Suffix> {
    (arg_list.map(Suffix::Call))
        .or(pair(dot, field).map(|(dot, field)| Suffix::Field(dot, field)))
        .parse(input)
}
fn suffix_expr(input: Input) -> IResult<Expr> {
    let (input, init) = atom_expr.parse(input)?;
    fold_many0(suffix, init, |expr, suffix| match suffix {
        Suffix::Call(args) => Expr::Call(CallExpr {
            func: box expr,
            args,
        }),
        Suffix::Field(dot, field) => Expr::Field(FieldExpr {
            base: box expr,
            dot,
            field,
        }),
    })
    .parse(input)
}
fn atom_expr(input: Input) -> IResult<Expr> {
    lit_expr
        .or(var_expr)
        .or(paren_expr)
        .or(tuple_expr)
        .or(loop_expr)
        .or(if_expr)
        .or(block_expr)
        .parse(input)
}
fn lit_expr(input: Input) -> IResult<Expr> { lit.map(Expr::Lit).parse(input) }
fn var_expr(input: Input) -> IResult<Expr> { var.map(Expr::Var).parse(input) }
fn paren_expr(input: Input) -> IResult<Expr> { paren(expr).map(Expr::Paren).parse(input) }
fn tuple_expr(input: Input) -> IResult<Expr> { tuple(expr).map(Expr::Tuple).parse(input) }
fn if_expr(input: Input) -> IResult<Expr> {
    let (input, kw_if) = kw_if.parse(input)?;
    let (input, test_expr) = expr_no_struct.parse(input)?;
    let (input, then_branch) = block_expr.parse(input)?;
    let (input, else_branch) = else_expr.opt().parse(input)?;
    Ok((
        input,
        Expr::If(IfExpr {
            kw_if,
            test_expr: box test_expr,
            then_branch: box then_branch,
            else_branch,
        }),
    ))
}
fn else_expr(input: Input) -> IResult<ElseExpr> {
    let (input, kw_else) = kw_else.parse(input)?;
    (if_expr.map(|if_expr| ElseExpr::ElseIf {
        kw_else,
        if_expr: box if_expr,
    }))
    .or(block_expr.map(|block| ElseExpr::ElseBlock {
        kw_else,
        block: box block,
    }))
    .parse(input)
}
fn loop_expr(input: Input) -> IResult<Expr> {
    let (input, kw_loop) = kw_loop.parse(input)?;
    let (input, expr) = block_expr.parse(input)?;
    Ok((
        input,
        Expr::Loop(LoopExpr {
            kw_loop,
            expr: box expr,
        }),
    ))
}
pub fn block(input: Input) -> IResult<Block> {
    let (input, lcurly) = lcurly.parse(input)?;
    let (input, stmts) = many0(stmt).parse(input)?;
    let (input, expr) = expr.opt().parse(input)?;
    let (input, rcurly) = rcurly.parse(input)?;

    let (stmts, expr) = match (stmts.as_slice(), &expr) {
        (
            [Stmt::Expr {
                expr,
                semicolon: None,
            }],
            None,
        ) => (vec![], Some(expr.clone())),
        _ => (stmts, expr),
    };
    Ok((
        input,
        Block {
            lcurly,
            stmts,
            expr: box expr,
            rcurly,
        },
    ))
}
pub fn block_expr(input: Input) -> IResult<Expr> { block.map(Expr::Block).parse(input) }
fn stmt(input: Input) -> IResult<Stmt> {
    blocklike_expr_stmt
        .or(expr_stmt)
        .or(let_stmt)
        .or(semicolon_stmt)
        .parse(input)
}
fn blocklike_expr_stmt(input: Input) -> IResult<Stmt> {
    let (input, expr) = if_expr.or(loop_expr).or(block_expr).parse(input)?;
    Ok((
        input,
        Stmt::Expr {
            expr,
            semicolon: None,
        },
    ))
}
fn expr_stmt(input: Input) -> IResult<Stmt> {
    let (input, expr) = expr.parse(input)?;
    let (input, semicolon) = semicolon.parse(input)?;
    Ok((
        input,
        Stmt::Expr {
            expr,
            semicolon: Some(semicolon),
        },
    ))
}
fn let_stmt(input: Input) -> IResult<Stmt> {
    let (input, kw_let) = kw_let.parse(input)?;
    let (input, pat) = pat.parse(input)?;
    let (input, ascription) = ascription.opt().parse(input)?;
    let (input, eq) = eq.parse(input)?;
    let (input, expr) = expr.parse(input)?;
    let (input, semicolon) = semicolon.parse(input)?;
    Ok((
        input,
        (Stmt::Let {
            kw_let,
            pat,
            ascription,
            eq,
            expr,
            semicolon,
        }),
    ))
}
fn semicolon_stmt(input: Input) -> IResult<Stmt> { semicolon.map(Stmt::Semicolon).parse(input) }

#[cfg(test)]
mod tests {
    use super::*;

    test_parse!(true_expr, expr, r#"true"#);
    test_parse!(false_expr, expr, r#"false"#);
    test_parse!(int_expr, expr, r#"123"#);
    test_parse!(assign_expr, expr, r#"1=2"#);
    test_parse!(nested_assign_expr, expr, r#"1=2=3"#);
    test_parse!(add_expr, expr, r#"1+2"#);
    test_parse!(nested_add_expr, expr, r#"1+2+3"#);
    test_parse!(mul_expr, expr, r#"1*2"#);
    test_parse!(nested_mul_expr, expr, r#"1*2*3"#);
    test_parse!(prefix_expr, expr, r#"-2"#);
    test_parse!(nested_prefix_expr, expr, r#"--2"#);
    test_parse!(call_expr, expr, r#"f()"#);
    test_parse!(nested_call_expr, expr, r#"f()()"#);
    test_parse!(tuple_field_expr, expr, r#"x.0"#);
    test_parse!(named_field_expr, expr, r#"x.y"#);
    test_parse!(nested_field_expr, expr, r#"x._0._0"#);
    test_parse!(mixed_expr, expr, r#"-1+2"#);
    test_parse!(lambda_expr, expr, r#"() => 1"#);
    test_parse!(nested_lambda_expr, expr, r#"(x) => (_) => x"#);
    test_parse!(if_expr, expr, r#"if true {}"#);
    test_parse!(if_else_expr, expr, r#"if true {} else {}"#);
    test_parse!(if_else_if_expr, expr, r#"if true {} else if false {}"#);
    test_parse!(loop_expr, expr, r#"loop {}"#);
    test_parse!(return_expr, expr, r#"return 5"#);
    test_parse!(break_expr, expr, r#"break 5"#);
    test_parse!(continue_expr, expr, r#"continue"#);
    test_parse!(block_expr, expr, r#"{x; y; z}"#);
    test_parse!(block_expr2, expr, r#"{if true {} loop {} {} x}"#);
    test_parse!(struct_expr, expr, r#"Foo {x: 1, y: 2}"#);
}
