use crate::ast::{self, LabAssign, LabSkip, LabStmt};

pub fn label(s: ast::Stmt) -> ast::LabStmt {
    s.label(1).0
}

impl ast::Stmt {
    fn label(self, l: u32) -> (ast::LabStmt, u32) {
        match self {
            ast::Stmt::Skip => (LabStmt::Skip(LabSkip { label: l }), l + 1),
            ast::Stmt::Assign(_) => todo!(),
            ast::Stmt::Seq(seq) => {
                let (seq, l) = seq.label(l);
                (LabStmt::Seq(seq), l)
            }
            ast::Stmt::If(_) => todo!(),
            ast::Stmt::While(_) => todo!(),
        }
    }
}

impl ast::Seq {
    fn label(self, l: u32) -> (ast::LabSeq, u32) {
        let (first, l) = self.first.label(l);
        let (second, l) = self.second.label(l);
        (
            ast::LabSeq {
                first: Box::new(first),
                second: Box::new(second),
            },
            l,
        )
    }
}

impl std::fmt::Display for LabStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_stmt(f, self, 0)
    }
}

fn write_stmt(
    f: &mut std::fmt::Formatter<'_>,
    stmt: &ast::LabStmt,
    nesting: usize,
) -> std::fmt::Result {
    match stmt {
        LabStmt::Assign(x) => write!(f, "{}", x),
        LabStmt::Skip(x) => write!(f, "{}", x),
        LabStmt::Seq(x) => write_seq(f, x, nesting),
        LabStmt::If(x) => write_if(f, x, nesting),
        LabStmt::While(x) => write_while(f, x, nesting),
    }
}

impl std::fmt::Display for LabAssign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]¹", self.inner)
    }
}

impl std::fmt::Display for ast::Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} := {}", self.var, self.exp)
    }
}

impl std::fmt::Display for ast::Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for ast::Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl std::fmt::Display for ast::AExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ast::AExp::Num(x) => write!(f, "{}", x),
            ast::AExp::Var(x) => write!(f, "{}", x),
            ast::AExp::Add(x) => write!(f, "{} + {}", x.left, x.right),
            ast::AExp::Sub(x) => write!(f, "{} - {}", x.left, x.right),
            ast::AExp::Mul(x) => write!(f, "{} * {}", x.left, x.right),
        }
    }
}

impl std::fmt::Display for ast::BExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ast::BExp::True => write!(f, "true"),
            ast::BExp::False => write!(f, "false"),
            ast::BExp::Not(x) => write!(f, "{}", x),
            ast::BExp::BoolOp(x) => write!(f, "{}", x),
            ast::BExp::RelOp(x) => write!(f, "{}", x),
        }
    }
}

impl std::fmt::Display for ast::NotExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "not {}", self.exp)
    }
}

impl std::fmt::Display for ast::BoolOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ast::BoolOp::And(x) => write!(f, "{} ∧ {}", x.left, x.right),
            ast::BoolOp::Or(x) => write!(f, "{} ∨ {}", x.left, x.right),
        }
    }
}

impl std::fmt::Display for ast::RelOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ast::RelOp::Eq(x) => write!(f, "{} = {}", x.left, x.right),
            ast::RelOp::Neq(x) => write!(f, "{} ≠ {}", x.left, x.right),
            ast::RelOp::Lt(x) => write!(f, "{} < {}", x.left, x.right),
            ast::RelOp::Leq(x) => write!(f, "{} ≤ {}", x.left, x.right),
            ast::RelOp::Gt(x) => write!(f, "{} > {}", x.left, x.right),
            ast::RelOp::Geq(x) => write!(f, "{} ≥ {}", x.left, x.right),
        }
    }
}

impl std::fmt::Display for LabSkip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[skip]{}", superscript(self.label))
    }
}

fn superscript(l: u32) -> String {
    let mut s = String::new();
    for c in l.to_string().chars() {
        match c {
            '0' => s.push('⁰'),
            '1' => s.push('¹'),
            '2' => s.push('²'),
            '3' => s.push('³'),
            '4' => s.push('⁴'),
            '5' => s.push('⁵'),
            '6' => s.push('⁶'),
            '7' => s.push('⁷'),
            '8' => s.push('⁸'),
            '9' => s.push('⁹'),
            _ => unreachable!(),
        }
    }
    s
}

fn write_seq(
    f: &mut std::fmt::Formatter<'_>,
    stmt: &ast::LabSeq,
    nesting: usize,
) -> std::fmt::Result {
    write!(
        f,
        "{};\n{}{}",
        stmt.first,
        "\t".repeat(nesting),
        stmt.second
    )
}

fn write_if(
    f: &mut std::fmt::Formatter<'_>,
    stmt: &ast::LabIf,
    nesting: usize,
) -> Result<(), std::fmt::Error> {
    write!(f, "if {} then\n{}", stmt.cond, "\t".repeat(nesting + 1),)?;
    write_stmt(f, stmt.then.as_ref(), nesting + 1)?;
    write!(
        f,
        "\n{}else\n{}",
        "\t".repeat(nesting),
        "\t".repeat(nesting + 1),
    )?;
    write_stmt(f, stmt.r#else.as_ref(), nesting + 1)
}

fn write_while(
    f: &mut std::fmt::Formatter<'_>,
    stmt: &ast::LabWhile,
    nesting: usize,
) -> Result<(), std::fmt::Error> {
    write!(f, "while {} do (\n{}", stmt.cond, "\t".repeat(nesting + 1))?;
    write_stmt(f, stmt.body.as_ref(), nesting + 1)?;
    write!(f, ")")
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;

    use super::label;

    #[test]
    fn single_skip() {
        let p = parse("skip").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[skip]¹");
    }

    #[test]
    fn two_skips() {
        let p = parse("skip; skip").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[skip]¹;\n[skip]²");
    }
}
