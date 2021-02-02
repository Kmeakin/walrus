use super::*;
use nom::{
    multi::{fold_many0, many0},
    sequence::pair,
};

pub fn expr(input: Input) -> IResult<Expr> {
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
fn return_expr(input: Input) -> IResult<Expr> {
    let (input, kw_return) = kw_return.parse(input)?;
    let (input, expr) = expr.parse(input)?;
    Ok((
        input,
        Expr::Return(ReturnExpr {
            kw_return,
            expr: box expr,
        }),
    ))
}
fn break_expr(input: Input) -> IResult<Expr> {
    let (input, kw_break) = kw_break.parse(input)?;
    let (input, expr) = expr.parse(input)?;
    Ok((
        input,
        Expr::Break(BreakExpr {
            kw_break,
            expr: box expr,
        }),
    ))
}
fn continue_expr(input: Input) -> IResult<Expr> {
    let (input, kw_continue) = kw_continue.parse(input)?;
    Ok((input, Expr::Continue(ContinueExpr { kw_continue })))
}
fn assign_op(input: Input) -> IResult<BinaryOp> { eq.map(BinaryOp::Assign).parse(input) }
fn assign_expr(input: Input) -> IResult<Expr> {
    let (input, lhs) = cmp_expr.parse(input)?;
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
fn cmp_op(input: Input) -> IResult<BinaryOp> {
    (eq_eq.map(BinaryOp::Eq))
        .or(bang_eq.map(BinaryOp::NotEq))
        .or(less.map(BinaryOp::Less))
        .or(less_eq.map(BinaryOp::LessEq))
        .or(greater.map(BinaryOp::Greater))
        .or(greater_eq.map(BinaryOp::GreaterEq))
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
fn add_op(input: Input) -> IResult<BinaryOp> {
    (plus.map(BinaryOp::Add))
        .or(minus.map(BinaryOp::Sub))
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
fn mul_op(input: Input) -> IResult<BinaryOp> {
    (star.map(BinaryOp::Mul))
        .or(slash.map(BinaryOp::Div))
        .parse(input)
}
fn mul_expr(input: Input) -> IResult<Expr> {
    let (input, init) = prefix_expr.parse(input)?;
    fold_many0(pair(mul_op, prefix_expr), init, |lhs, (op, rhs)| {
        Expr::Binary(BinaryExpr {
            lhs: box lhs,
            op,
            rhs: box rhs,
        })
    })
    .parse(input)
}
fn prefix_op(input: Input) -> IResult<UnaryOp> {
    (plus.map(UnaryOp::Add))
        .or(minus.map(UnaryOp::Sub))
        .parse(input)
}
fn prefix_expr(input: Input) -> IResult<Expr> {
    pair(prefix_op, prefix_expr)
        .map(|(op, expr)| Expr::Unary(UnaryExpr { op, expr: box expr }))
        .or(suffix_expr)
        .parse(input)
}
enum Suffix {
    Call(ArgList),
    Field(Dot, Var),
}
fn arg_list(input: Input) -> IResult<ArgList> {
    paren(punctuated0(expr, comma)).map(ArgList).parse(input)
}
fn suffix(input: Input) -> IResult<Suffix> {
    (arg_list.map(Suffix::Call))
        .or(pair(dot, var).map(|(dot, var)| Suffix::Field(dot, var)))
        .parse(input)
}
fn suffix_expr(input: Input) -> IResult<Expr> {
    let (input, init) = atom_expr.parse(input)?;
    fold_many0(suffix, init, |expr, suffix| match suffix {
        Suffix::Call(args) => Expr::Call(CallExpr {
            func: box expr,
            args,
        }),
        Suffix::Field(dot, var) => Expr::Field(FieldExpr {
            expr: box expr,
            dot,
            var,
        }),
    })
    .parse(input)
}
fn atom_expr(input: Input) -> IResult<Expr> {
    lit_expr
        .or(path_expr)
        .or(paren_expr)
        .or(tuple_expr)
        .or(loop_expr)
        .or(if_expr.map(Expr::If))
        .or(block.map(Expr::Block))
        .parse(input)
}
fn lit_expr(input: Input) -> IResult<Expr> { lit.map(Expr::Lit).parse(input) }
fn path_expr(input: Input) -> IResult<Expr> { path.map(Expr::Path).parse(input) }
fn paren_expr(input: Input) -> IResult<Expr> { paren(expr).map(Expr::Paren).parse(input) }
fn tuple_expr(input: Input) -> IResult<Expr> { tuple(expr).map(Expr::Tuple).parse(input) }
fn if_expr(input: Input) -> IResult<IfExpr> {
    let (input, kw_if) = kw_if.parse(input)?;
    let (input, test_expr) = expr.parse(input)?;
    let (input, then_block) = block.parse(input)?;
    let (input, else_expr) = else_expr.opt().parse(input)?;
    Ok((
        input,
        IfExpr {
            kw_if,
            test_expr: box test_expr,
            then_block,
            else_expr,
        },
    ))
}
fn else_expr(input: Input) -> IResult<ElseExpr> {
    let (input, kw_else) = kw_else.parse(input)?;
    (if_expr.map(|if_expr| ElseExpr::ElseIf {
        kw_else,
        if_expr: box if_expr,
    }))
    .or(block.map(|block| ElseExpr::ElseBlock { kw_else, block }))
    .parse(input)
}
fn loop_expr(input: Input) -> IResult<Expr> {
    let (input, kw_loop) = kw_loop.parse(input)?;
    let (input, block) = block.parse(input)?;
    Ok((input, Expr::Loop(LoopExpr { kw_loop, block })))
}
pub fn block(input: Input) -> IResult<Block> {
    let (input, lcurly) = lcurly.parse(input)?;
    let (input, stmts) = many0(stmt).parse(input)?;
    let (input, expr) = expr.opt().parse(input)?;
    let (input, rcurly) = rcurly.parse(input)?;
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
fn stmt(input: Input) -> IResult<Stmt> { expr_stmt.or(let_stmt).or(semicolon_stmt).parse(input) }
fn expr_stmt(input: Input) -> IResult<Stmt> {
    let (input, expr) = expr.parse(input)?;
    let (input, semicolon) = semicolon.parse(input)?;
    Ok((input, (Stmt::Expr { expr, semicolon })))
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
