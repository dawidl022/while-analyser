use nom::combinator::opt;

use crate::ast;

use super::{combine_aexps, combine_bexps, first_aexp, first_bexp, second_aexp, second_bexp};

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

/// Same as [super::bexp], but does not rotate the tree, as that should
/// only be done at the root of the AExp.
pub fn rec_bexp(input: &str) -> nom::IResult<&str, ast::BExp> {
    let (input, first) = first_bexp(input)?;
    let (input, second) = opt(second_bexp)(input)?;

    Ok(match second {
        Some((op, second)) => {
            let expr = combine_bexps(first, second, op);
            (input, expr)
        }
        None => (input, first),
    })
}

impl ast::AExp {
    // TODO can extract out rotate_associativity between this and BExp
    pub fn rotate_associativity(mut self) -> Self {
        if Self::opt_height(self.right()) > 1 {
            if let ast::AExp::Mul(_) = self.right().unwrap() {
                return self;
            }

            let mut root = self.right().unwrap().clone();
            self.set_right(self.right().unwrap().left().unwrap().clone());
            root.set_left(self);

            root.rotate_associativity()
        } else {
            self
        }
    }

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

    fn opt_height(node: Option<&ast::AExp>) -> usize {
        match node {
            None => 0,
            Some(node) => node.height(),
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

impl ast::BExp {
    pub fn rotate_associativity(mut self) -> Self {
        if Self::opt_height(self.right()) > 1 {
            if let ast::BExp::BoolOp(ast::BoolOp::And(_)) = self.right().unwrap() {
                return self;
            }

            let mut root = self.right().unwrap().clone();
            self.set_right(self.right().unwrap().left().unwrap().clone());
            root.set_left(self);

            root.rotate_associativity()
        } else {
            self
        }
    }

    pub fn left(&self) -> Option<&Self> {
        match self {
            ast::BExp::True => None,
            ast::BExp::False => None,
            ast::BExp::Not(_) => None,
            ast::BExp::RelOp(_) => None,
            ast::BExp::BoolOp(bool_op) => Some(match bool_op {
                ast::BoolOp::And(and_exp) => and_exp.left.as_ref(),
                ast::BoolOp::Or(or_exp) => or_exp.left.as_ref(),
            }),
        }
    }

    pub fn right(&self) -> Option<&Self> {
        match self {
            ast::BExp::True => None,
            ast::BExp::False => None,
            ast::BExp::Not(_) => None,
            ast::BExp::RelOp(_) => None,
            ast::BExp::BoolOp(bool_op) => Some(match bool_op {
                ast::BoolOp::And(and_exp) => and_exp.right.as_ref(),
                ast::BoolOp::Or(or_exp) => or_exp.right.as_ref(),
            }),
        }
    }

    fn height(&self) -> usize {
        match self {
            ast::BExp::True => 1,
            ast::BExp::False => 1,
            ast::BExp::Not(_) => 1,
            ast::BExp::RelOp(_) => 1,
            ast::BExp::BoolOp(bool_op) => bool_op.height(),
        }
    }

    fn opt_height(node: Option<&ast::BExp>) -> usize {
        match node {
            None => 0,
            Some(node) => node.height(),
        }
    }

    fn set_left(&mut self, left: Self) {
        match self {
            ast::BExp::True => unreachable!(),
            ast::BExp::False => unreachable!(),
            ast::BExp::Not(_) => unreachable!(),
            ast::BExp::RelOp(_) => unreachable!(),
            ast::BExp::BoolOp(bool_op) => match bool_op {
                ast::BoolOp::And(and_exp) => and_exp.left = Box::new(left),
                ast::BoolOp::Or(or_exp) => or_exp.left = Box::new(left),
            },
        }
    }

    fn set_right(&mut self, right: Self) {
        match self {
            ast::BExp::True => unreachable!(),
            ast::BExp::False => unreachable!(),
            ast::BExp::Not(_) => unreachable!(),
            ast::BExp::RelOp(_) => unreachable!(),
            ast::BExp::BoolOp(bool_op) => match bool_op {
                ast::BoolOp::And(and_exp) => and_exp.right = Box::new(right),
                ast::BoolOp::Or(or_exp) => or_exp.right = Box::new(right),
            },
        }
    }
}

impl ast::BoolOp {
    fn height(&self) -> usize {
        match self {
            ast::BoolOp::And(and_exp) => 1 + and_exp.left.height().max(and_exp.right.height()),
            ast::BoolOp::Or(or_exp) => 1 + or_exp.left.height().max(or_exp.right.height()),
        }
    }
}
