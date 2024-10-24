use nom::combinator::opt;

use crate::ast;

use super::{combine_aexps, first_aexp, second_aexp};

pub fn rotate_associativity(mut expr: ast::AExp) -> ast::AExp {
    if height(expr.right()) > 1 {
        if let ast::AExp::Mul(_) = expr.right().unwrap() {
            return expr;
        }

        let mut root = expr.right().unwrap().clone();
        expr.set_right(expr.right().unwrap().left().unwrap().clone());
        root.set_left(expr);

        rotate_associativity(root)
    } else {
        expr
    }
}

/// Same as [super::aexp], but does not rotate the tree, as that should
/// only be done at the root of the AExp.
pub fn rec_aexp(input: &str) -> nom::IResult<&str, ast::AExp> {
    let (input, first) = first_aexp(input)?;
    let (input, second) = opt(second_aexp)(input)?;

    Ok(match second {
        Some((op, second)) => {
            let expr = combine_aexps(first, second, op);
            // don't rotate in the recursive case, only at the root of the AExp
            (input, expr)
        }
        None => (input, first),
    })
}

impl ast::AExp {
    fn left(&self) -> Option<&Self> {
        match self {
            ast::AExp::Num(_) => None,
            ast::AExp::Var(_) => None,
            ast::AExp::Add(exp) => Some(exp.left.as_ref()),
            ast::AExp::Sub(exp) => Some(exp.left.as_ref()),
            ast::AExp::Mul(exp) => Some(exp.left.as_ref()),
        }
    }

    fn right(&self) -> Option<&Self> {
        match self {
            ast::AExp::Num(_) => None,
            ast::AExp::Var(_) => None,
            ast::AExp::Add(exp) => Some(exp.right.as_ref()),
            ast::AExp::Sub(exp) => Some(exp.right.as_ref()),
            ast::AExp::Mul(exp) => Some(exp.right.as_ref()),
        }
    }

    fn height(&self) -> usize {
        match self {
            ast::AExp::Num(_) => 1,
            ast::AExp::Var(_) => 1,
            ast::AExp::Add(exp) => 1 + exp.left.height().max(exp.right.height()),
            ast::AExp::Sub(exp) => 1 + exp.left.height().max(exp.right.height()),
            ast::AExp::Mul(exp) => 1 + exp.left.height().max(exp.right.height()),
        }
    }

    fn set_left(&mut self, left: Self) {
        match self {
            ast::AExp::Num(_) => unreachable!(),
            ast::AExp::Var(_) => unreachable!(),
            ast::AExp::Add(exp) => exp.left = Box::new(left),
            ast::AExp::Sub(exp) => exp.left = Box::new(left),
            ast::AExp::Mul(exp) => exp.left = Box::new(left),
        }
    }

    fn set_right(&mut self, right: Self) {
        match self {
            ast::AExp::Num(_) => unreachable!(),
            ast::AExp::Var(_) => unreachable!(),
            ast::AExp::Add(exp) => exp.right = Box::new(right),
            ast::AExp::Sub(exp) => exp.right = Box::new(right),
            ast::AExp::Mul(exp) => exp.right = Box::new(right),
        }
    }
}

fn height(node: Option<&ast::AExp>) -> usize {
    match node {
        None => 0,
        Some(node) => node.height(),
    }
}
