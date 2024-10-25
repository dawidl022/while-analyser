mod rotation;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{all_consuming, map, opt},
};
use rotation::{rec_aexp, rec_bexp};

use crate::ast;

pub fn parse(input: &str) -> Result<ast::Stmt, ParseError> {
    let (_, stmt) = all_consuming(statement)(input)?;

    Ok(stmt)
}

fn statement(input: &str) -> nom::IResult<&str, ast::Stmt> {
    let (input, first) = alt((assign, skip, if_stmt, while_loop))(input)?;
    let (input, second) = opt(second_statement)(input)?;

    Ok(match second {
        Some(second) => (
            input,
            ast::Stmt::Seq(ast::Seq {
                first: Box::new(first),
                second: Box::new(second),
            }),
        ),
        None => (input, first),
    })
}

fn second_statement(input: &str) -> nom::IResult<&str, ast::Stmt> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(";")(input)?;
    let (input, _) = multispace0(input)?;

    statement(input)
}

fn assign(input: &str) -> nom::IResult<&str, ast::Stmt> {
    let (input, var) = var(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":=")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, exp) = aexp(input)?;

    Ok((input, ast::Stmt::Assign(ast::Assign { var, exp })))
}

fn var(input: &str) -> nom::IResult<&str, ast::Var> {
    let (input, name) = nom::character::complete::alpha1(input)?;
    Ok((input, ast::Var { name: name.into() }))
}

fn aexp(input: &str) -> nom::IResult<&str, ast::AExp> {
    let (input, first) = first_aexp(input)?;
    let (input, second) = opt(second_aexp)(input)?;

    Ok(match second {
        Some((op, second)) => {
            let expr = combine_aexps(first, second, op);
            let expr = expr.rotate_associativity();
            (input, expr)
        }
        None => (input, first),
    })
}

fn first_aexp(input: &str) -> nom::IResult<&str, ast::AExp> {
    alt((map(num, ast::AExp::Num), (map(var, ast::AExp::Var))))(input)
}

fn second_aexp(input: &str) -> nom::IResult<&str, (char, ast::AExp)> {
    let (input, _) = multispace0(input)?;
    let (input, op) = nom::character::complete::one_of("+-*")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, second) = rec_aexp(input)?;

    Ok((input, (op, second)))
}

fn combine_aexps(first: ast::AExp, second: ast::AExp, op: char) -> ast::AExp {
    let left = Box::new(first);
    let right = Box::new(second);

    match op {
        '+' => ast::AExp::Add(ast::AddExp { left, right }),
        '-' => ast::AExp::Sub(ast::SubExp { left, right }),
        '*' => ast::AExp::Mul(ast::MulExp { left, right }),
        _ => unreachable!(),
    }
}

fn num(input: &str) -> nom::IResult<&str, ast::Num> {
    let (input, value) = nom::character::complete::digit1(input)?;
    let value = value.parse().unwrap();
    Ok((input, ast::Num { value }))
}

fn skip(input: &str) -> nom::IResult<&str, ast::Stmt> {
    let (input, _) = tag("skip")(input)?;
    Ok((input, ast::Stmt::Skip))
}

fn if_stmt(input: &str) -> nom::IResult<&str, ast::Stmt> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, cond) = bexp(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("then")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, then) = statement(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("else")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, r#else) = statement(input)?;

    Ok((
        input,
        ast::Stmt::If(ast::If {
            cond,
            then: Box::new(then),
            r#else: Box::new(r#else),
        }),
    ))
}

fn bexp(input: &str) -> nom::IResult<&str, ast::BExp> {
    let (input, first) = first_bexp(input)?;
    let (input, second) = opt(second_bexp)(input)?;

    Ok(match second {
        Some((op, second)) => {
            let expr = combine_bexps(first, second, op);
            let expr = expr.rotate_associativity();
            (input, expr)
        }
        None => (input, first),
    })
}

fn first_bexp(input: &str) -> nom::IResult<&str, ast::BExp> {
    alt((
        map(tag("true"), |_| ast::BExp::True),
        map(tag("false"), |_| ast::BExp::False),
        map(not, ast::BExp::Not),
        map(rel_op, ast::BExp::RelOp),
    ))(input)
}

fn second_bexp(input: &str) -> nom::IResult<&str, (&str, ast::BExp)> {
    let (input, _) = multispace0(input)?;
    let (input, op) = alt((tag("and"), (tag("or"))))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, second) = rec_bexp(input)?;

    Ok((input, (op, second)))
}

fn combine_bexps(first: ast::BExp, second: ast::BExp, op: &str) -> ast::BExp {
    let left = Box::new(first);
    let right = Box::new(second);

    match op {
        "and" => ast::BExp::BoolOp(ast::BoolOp::And(ast::AndExp { left, right })),
        "or" => ast::BExp::BoolOp(ast::BoolOp::Or(ast::OrExp { left, right })),
        _ => unreachable!(),
    }
}

fn not(input: &str) -> nom::IResult<&str, ast::NotExp> {
    let (input, _) = tag("not")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, exp) = bexp(input)?;

    Ok((input, ast::NotExp { exp: Box::new(exp) }))
}

fn rel_op(input: &str) -> nom::IResult<&str, ast::RelOp> {
    let (input, left) = aexp(input)?;
    let (input, _) = multispace0(input)?;
    let (input, op) = alt((
        tag("="),
        tag("!="),
        tag("<="),
        tag("<"),
        tag(">="),
        tag(">"),
    ))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, right) = aexp(input)?;

    Ok((
        input,
        match op {
            "=" => ast::RelOp::Eq(ast::EqExp { left, right }),
            "!=" => ast::RelOp::Neq(ast::NeqExp { left, right }),
            "<" => ast::RelOp::Lt(ast::LtExp { left, right }),
            "<=" => ast::RelOp::Leq(ast::LeqExp { left, right }),
            ">" => ast::RelOp::Gt(ast::GtExp { left, right }),
            ">=" => ast::RelOp::Geq(ast::GeqExp { left, right }),
            _ => unreachable!(),
        },
    ))
}

fn while_loop(input: &str) -> nom::IResult<&str, ast::Stmt> {
    let (input, _) = tag("while")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, cond) = bexp(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("do")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, body) = statement(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((
        input,
        ast::Stmt::While(ast::While {
            cond,
            body: Box::new(body),
        }),
    ))
}

#[derive(Debug)]
pub struct ParseError<'a> {
    #[allow(dead_code)]
    inner: nom::Err<nom::error::Error<&'a str>>,
}

impl<'a> From<nom::Err<nom::error::Error<&'a str>>> for ParseError<'a> {
    fn from(inner: nom::Err<nom::error::Error<&'a str>>) -> Self {
        Self { inner }
    }
}

#[cfg(test)]
mod tests {
    use super::parse;
    use crate::ast::{self, Num};

    #[test]
    fn single_skip() {
        let p = parse("skip").unwrap();
        assert!(matches!(p, ast::Stmt::Skip));
    }

    #[test]
    fn skip_and_some_chars() {
        parse("skip;").err().unwrap();
    }

    #[test]
    fn single_assignment() {
        let p = parse("x := 1").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Num(Num { value: 1 }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn single_assignment_no_spaces() {
        let p = parse("x:=1").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Num(Num { value: 1 }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_followed_by_skip() {
        let p = parse("x := 1; skip").unwrap();
        let expected = ast::Stmt::Seq(ast::Seq {
            first: Box::new(ast::Stmt::Assign(ast::Assign {
                var: ast::Var { name: "x".into() },
                exp: ast::AExp::Num(Num { value: 1 }),
            })),
            second: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_followed_by_no_spaces_skip() {
        let p = parse("x := 1;skip").unwrap();
        let expected = ast::Stmt::Seq(ast::Seq {
            first: Box::new(ast::Stmt::Assign(ast::Assign {
                var: ast::Var { name: "x".into() },
                exp: ast::AExp::Num(Num { value: 1 }),
            })),
            second: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn sequence_of_3_statements() {
        let p = parse("x := 1; y := 2; z := 3").unwrap();
        let expected = ast::Stmt::Seq(ast::Seq {
            first: Box::new(ast::Stmt::Assign(ast::Assign {
                var: ast::Var { name: "x".into() },
                exp: ast::AExp::Num(Num { value: 1 }),
            })),
            second: Box::new(ast::Stmt::Seq(ast::Seq {
                first: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "y".into() },
                    exp: ast::AExp::Num(Num { value: 2 }),
                })),
                second: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "z".into() },
                    exp: ast::AExp::Num(Num { value: 3 }),
                })),
            })),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn variable_assignment() {
        let p = parse("x := y").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Var(ast::Var { name: "y".into() }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_constant_addition() {
        let p = parse("x := 1 + 2").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Add(ast::AddExp {
                left: Box::new(ast::AExp::Num(Num { value: 1 })),
                right: Box::new(ast::AExp::Num(Num { value: 2 })),
            }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_nested_constant_addition() {
        let p = parse("x := 1 + 2 + 3").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Add(ast::AddExp {
                left: Box::new(ast::AExp::Add(ast::AddExp {
                    left: Box::new(ast::AExp::Num(Num { value: 1 })),
                    right: Box::new(ast::AExp::Num(Num { value: 2 })),
                })),
                right: Box::new(ast::AExp::Num(Num { value: 3 })),
            }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_constant_subtraction() {
        let p = parse("x := 1 - 2").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Sub(ast::SubExp {
                left: Box::new(ast::AExp::Num(Num { value: 1 })),
                right: Box::new(ast::AExp::Num(Num { value: 2 })),
            }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_constant_subtraction_associates_left() {
        let p = parse("x := 1 - 2 - 3").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Sub(ast::SubExp {
                left: Box::new(ast::AExp::Sub(ast::SubExp {
                    left: Box::new(ast::AExp::Num(Num { value: 1 })),
                    right: Box::new(ast::AExp::Num(Num { value: 2 })),
                })),
                right: Box::new(ast::AExp::Num(Num { value: 3 })),
            }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_constant_subtraction_associates_left_for_multiple_levels() {
        let p = parse("x := 1 - 2 - 3 - 4").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Sub(ast::SubExp {
                left: Box::new(ast::AExp::Sub(ast::SubExp {
                    left: Box::new(ast::AExp::Sub(ast::SubExp {
                        left: Box::new(ast::AExp::Num(Num { value: 1 })),
                        right: Box::new(ast::AExp::Num(Num { value: 2 })),
                    })),
                    right: Box::new(ast::AExp::Num(Num { value: 3 })),
                })),
                right: Box::new(ast::AExp::Num(Num { value: 4 })),
            }),
        });
        assert_eq!(p, expected);

        let p = parse("x := 1 - 2 - 3 - 4 - 5").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Sub(ast::SubExp {
                left: Box::new(ast::AExp::Sub(ast::SubExp {
                    left: Box::new(ast::AExp::Sub(ast::SubExp {
                        left: Box::new(ast::AExp::Sub(ast::SubExp {
                            left: Box::new(ast::AExp::Num(Num { value: 1 })),
                            right: Box::new(ast::AExp::Num(Num { value: 2 })),
                        })),
                        right: Box::new(ast::AExp::Num(Num { value: 3 })),
                    })),
                    right: Box::new(ast::AExp::Num(Num { value: 4 })),
                })),
                right: Box::new(ast::AExp::Num(Num { value: 5 })),
            }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_constant_multiplication() {
        let p = parse("x := 1 * 2").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Mul(ast::MulExp {
                left: Box::new(ast::AExp::Num(Num { value: 1 })),
                right: Box::new(ast::AExp::Num(Num { value: 2 })),
            }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_multiplication_has_higher_precedence() {
        let p = parse("x := 1 + 2 * 3").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Add(ast::AddExp {
                left: Box::new(ast::AExp::Num(Num { value: 1 })),
                right: Box::new(ast::AExp::Mul(ast::MulExp {
                    left: Box::new(ast::AExp::Num(Num { value: 2 })),
                    right: Box::new(ast::AExp::Num(Num { value: 3 })),
                })),
            }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn assignment_multiplication_has_higher_precedence_when_mult_comes_first() {
        let p = parse("x := 1 * 2 + 3").unwrap();
        let expected = ast::Stmt::Assign(ast::Assign {
            var: ast::Var { name: "x".into() },
            exp: ast::AExp::Add(ast::AddExp {
                left: Box::new(ast::AExp::Mul(ast::MulExp {
                    left: Box::new(ast::AExp::Num(Num { value: 1 })),
                    right: Box::new(ast::AExp::Num(Num { value: 2 })),
                })),
                right: Box::new(ast::AExp::Num(Num { value: 3 })),
            }),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_true_cond() {
        let p = parse("if true then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::True,
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_false_cond() {
        let p = parse("if false then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::False,
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_negation_in_cond() {
        let p = parse("if not true then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::Not(ast::NotExp {
                exp: Box::new(ast::BExp::True),
            }),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_and_cond() {
        let p = parse("if true and false then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::BoolOp(ast::BoolOp::And(ast::AndExp {
                left: Box::new(ast::BExp::True),
                right: Box::new(ast::BExp::False),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_or_cond() {
        let p = parse("if true or false then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::BoolOp(ast::BoolOp::Or(ast::OrExp {
                left: Box::new(ast::BExp::True),
                right: Box::new(ast::BExp::False),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_multiple_and() {
        let p = parse("if true and true and false then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::BoolOp(ast::BoolOp::And(ast::AndExp {
                left: Box::new(ast::BExp::True),
                right: Box::new(ast::BExp::BoolOp(ast::BoolOp::And(ast::AndExp {
                    left: Box::new(ast::BExp::True),
                    right: Box::new(ast::BExp::False),
                }))),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_and_has_precedence_over_or() {
        let p = parse("if false and false or true then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::BoolOp(ast::BoolOp::Or(ast::OrExp {
                left: Box::new(ast::BExp::BoolOp(ast::BoolOp::And(ast::AndExp {
                    left: Box::new(ast::BExp::False),
                    right: Box::new(ast::BExp::False),
                }))),
                right: Box::new(ast::BExp::True),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_eq_check() {
        let p = parse("if x = 1 then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::RelOp(ast::RelOp::Eq(ast::EqExp {
                left: ast::AExp::Var(ast::Var { name: "x".into() }),
                right: ast::AExp::Num(Num { value: 1 }),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_neq_check() {
        let p = parse("if x != 1 then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::RelOp(ast::RelOp::Neq(ast::NeqExp {
                left: ast::AExp::Var(ast::Var { name: "x".into() }),
                right: ast::AExp::Num(Num { value: 1 }),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_lt_check() {
        let p = parse("if x < 1 then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::RelOp(ast::RelOp::Lt(ast::LtExp {
                left: ast::AExp::Var(ast::Var { name: "x".into() }),
                right: ast::AExp::Num(Num { value: 1 }),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_leq_check() {
        let p = parse("if x <= 1 then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::RelOp(ast::RelOp::Leq(ast::LeqExp {
                left: ast::AExp::Var(ast::Var { name: "x".into() }),
                right: ast::AExp::Num(Num { value: 1 }),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_gt_check() {
        let p = parse("if x > 1 then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::RelOp(ast::RelOp::Gt(ast::GtExp {
                left: ast::AExp::Var(ast::Var { name: "x".into() }),
                right: ast::AExp::Num(Num { value: 1 }),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_geq_check() {
        let p = parse("if x >= 1 then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::RelOp(ast::RelOp::Geq(ast::GeqExp {
                left: ast::AExp::Var(ast::Var { name: "x".into() }),
                right: ast::AExp::Num(Num { value: 1 }),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn if_statement_and_relational_ops() {
        let p = parse("if x = 1 and y = 2 then skip else skip").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::BoolOp(ast::BoolOp::And(ast::AndExp {
                left: Box::new(ast::BExp::RelOp(ast::RelOp::Eq(ast::EqExp {
                    left: ast::AExp::Var(ast::Var { name: "x".into() }),
                    right: ast::AExp::Num(Num { value: 1 }),
                }))),
                right: Box::new(ast::BExp::RelOp(ast::RelOp::Eq(ast::EqExp {
                    left: ast::AExp::Var(ast::Var { name: "y".into() }),
                    right: ast::AExp::Num(Num { value: 2 }),
                }))),
            })),
            then: Box::new(ast::Stmt::Skip),
            r#else: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn sequence_in_if_statement_branches() {
        let p = parse("if true then x := 1; y := 2 else x := 3; y := 4").unwrap();
        let expected = ast::Stmt::If(ast::If {
            cond: ast::BExp::True,
            then: Box::new(ast::Stmt::Seq(ast::Seq {
                first: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "x".into() },
                    exp: ast::AExp::Num(Num { value: 1 }),
                })),
                second: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "y".into() },
                    exp: ast::AExp::Num(Num { value: 2 }),
                })),
            })),
            r#else: Box::new(ast::Stmt::Seq(ast::Seq {
                first: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "x".into() },
                    exp: ast::AExp::Num(Num { value: 3 }),
                })),
                second: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "y".into() },
                    exp: ast::AExp::Num(Num { value: 4 }),
                })),
            })),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn while_loop() {
        let p = parse("while true do (skip)").unwrap();
        let expected = ast::Stmt::While(ast::While {
            cond: ast::BExp::True,
            body: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn while_loop_followed_by_statement() {
        let p = parse("while true do (skip); skip").unwrap();
        let expected = ast::Stmt::Seq(ast::Seq {
            first: Box::new(ast::Stmt::While(ast::While {
                cond: ast::BExp::True,
                body: Box::new(ast::Stmt::Skip),
            })),
            second: Box::new(ast::Stmt::Skip),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn while_loop_multiple_statements_in_body() {
        let p = parse("while true do (x := 1; y := 2)").unwrap();
        let expected = ast::Stmt::While(ast::While {
            cond: ast::BExp::True,
            body: Box::new(ast::Stmt::Seq(ast::Seq {
                first: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "x".into() },
                    exp: ast::AExp::Num(Num { value: 1 }),
                })),
                second: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "y".into() },
                    exp: ast::AExp::Num(Num { value: 2 }),
                })),
            })),
        });
        assert_eq!(p, expected);
    }

    #[test]
    fn complete_program() {
        let p = parse("y := x; x := 1; while y > 1 do (z := z * y; y := y - 1); y := 0").unwrap();
        let expected = ast::Stmt::Seq(ast::Seq {
            first: Box::new(ast::Stmt::Assign(ast::Assign {
                var: ast::Var { name: "y".into() },
                exp: ast::AExp::Var(ast::Var { name: "x".into() }),
            })),
            second: Box::new(ast::Stmt::Seq(ast::Seq {
                first: Box::new(ast::Stmt::Assign(ast::Assign {
                    var: ast::Var { name: "x".into() },
                    exp: ast::AExp::Num(Num { value: 1 }),
                })),
                second: Box::new(ast::Stmt::Seq(ast::Seq {
                    first: Box::new(ast::Stmt::While(ast::While {
                        cond: ast::BExp::RelOp(ast::RelOp::Gt(ast::GtExp {
                            left: ast::AExp::Var(ast::Var { name: "y".into() }),
                            right: ast::AExp::Num(Num { value: 1 }),
                        })),
                        body: Box::new(ast::Stmt::Seq(ast::Seq {
                            first: Box::new(ast::Stmt::Assign(ast::Assign {
                                var: ast::Var { name: "z".into() },
                                exp: ast::AExp::Mul(ast::MulExp {
                                    left: Box::new(ast::AExp::Var(ast::Var { name: "z".into() })),
                                    right: Box::new(ast::AExp::Var(ast::Var { name: "y".into() })),
                                }),
                            })),
                            second: Box::new(ast::Stmt::Assign(ast::Assign {
                                var: ast::Var { name: "y".into() },
                                exp: ast::AExp::Sub(ast::SubExp {
                                    left: Box::new(ast::AExp::Var(ast::Var { name: "y".into() })),
                                    right: Box::new(ast::AExp::Num(Num { value: 1 })),
                                }),
                            })),
                        })),
                    })),
                    second: Box::new(ast::Stmt::Assign(ast::Assign {
                        var: ast::Var { name: "y".into() },
                        exp: ast::AExp::Num(Num { value: 0 }),
                    })),
                })),
            })),
        });

        assert_eq!(p, expected);
    }
}
