mod rotation;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{all_consuming, map, opt},
};
use rotation::{rec_aexp, rotate_associativity};

use crate::ast;

pub fn parse(input: &str) -> Result<ast::Stmt, ParseError> {
    let (_, stmt) = all_consuming(statement)(input)?;

    Ok(stmt)
}

fn statement(input: &str) -> nom::IResult<&str, ast::Stmt> {
    let (input, first) = alt((assign, skip))(input)?;
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
            let expr = rotate_associativity(expr);
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

    // TODO would this have the right precedence?
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
}
