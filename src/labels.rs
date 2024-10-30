use crate::ast::{self, LabAssign, LabSkip, LabStmt};

pub fn label(s: ast::Stmt) -> ast::LabStmt {
    s.label(1).0
}

impl ast::Stmt {
    fn label(self, l: u32) -> (ast::LabStmt, u32) {
        match self {
            ast::Stmt::Skip => (LabStmt::Skip(LabSkip { label: l }), l + 1),
            ast::Stmt::Assign(a) => {
                let (a, l) = (LabAssign { inner: a, label: l }, l + 1);
                (LabStmt::Assign(a), l)
            }
            ast::Stmt::Seq(seq) => {
                let (seq, l) = seq.label(l);
                (LabStmt::Seq(seq), l)
            }
            ast::Stmt::If(i) => {
                let (i, l) = i.label(l);
                (LabStmt::If(i), l)
            }
            ast::Stmt::While(w) => {
                let (w, l) = w.label(l);
                (LabStmt::While(w), l)
            }
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

impl ast::If {
    fn label(self, l: u32) -> (ast::LabIf, u32) {
        let cond_label = l;
        let (then, l) = self.then.label(l + 1);
        let (r#else, l) = self.r#else.label(l);
        (
            ast::LabIf {
                cond: self.cond,
                cond_label,
                then: Box::new(then),
                r#else: Box::new(r#else),
            },
            l,
        )
    }
}

impl ast::While {
    fn label(self, l: u32) -> (ast::LabWhile, u32) {
        let cond_label = l;
        let (body, l) = self.body.label(l + 1);
        (
            ast::LabWhile {
                cond: self.cond,
                cond_label,
                body: Box::new(body),
            },
            l,
        )
    }
}

/// Soft-tab represented by 4 spaces
const TAB: &str = "    ";

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
        write!(f, "[{}]{}", self.inner, superscript(self.label))
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
    write!(f, "{};\n{}{}", stmt.first, TAB.repeat(nesting), stmt.second)
}

fn write_if(
    f: &mut std::fmt::Formatter<'_>,
    stmt: &ast::LabIf,
    nesting: usize,
) -> Result<(), std::fmt::Error> {
    write!(
        f,
        "if [{}]{} then\n{}",
        stmt.cond,
        superscript(stmt.cond_label),
        TAB.repeat(nesting + 1),
    )?;
    write_stmt(f, stmt.then.as_ref(), nesting + 1)?;
    write!(
        f,
        "\n{}else\n{}",
        TAB.repeat(nesting),
        TAB.repeat(nesting + 1),
    )?;
    write_stmt(f, stmt.r#else.as_ref(), nesting + 1)
}

fn write_while(
    f: &mut std::fmt::Formatter<'_>,
    stmt: &ast::LabWhile,
    nesting: usize,
) -> Result<(), std::fmt::Error> {
    write!(
        f,
        "while [{}]{} do (\n{}",
        stmt.cond,
        superscript(stmt.cond_label),
        TAB.repeat(nesting + 1)
    )?;
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

    #[test]
    fn three_skips() {
        let p = parse("skip; skip; skip").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[skip]¹;\n[skip]²;\n[skip]³");
    }

    #[test]
    fn two_digit_labels() {
        let p = parse("skip; skip; skip; skip; skip; skip; skip; skip; skip; skip").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[skip]¹;\n[skip]²;\n[skip]³;\n[skip]⁴;\n[skip]⁵;\n[skip]⁶;\n[skip]⁷;\n[skip]⁸;\n[skip]⁹;\n[skip]¹⁰");
    }

    #[test]
    fn single_assignment() {
        let p = parse("x := 1").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[x := 1]¹");
    }

    #[test]
    fn two_assignments() {
        let p = parse("x := 1; y := 2").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[x := 1]¹;\n[y := 2]²");
    }

    #[test]
    fn assignment_followed_by_skip() {
        let p = parse("x := 1; skip").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[x := 1]¹;\n[skip]²");
    }

    #[test]
    fn variable_assignment() {
        let p = parse("x := y").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[x := y]¹");
    }

    #[test]
    fn assignment_constant_addition() {
        let p = parse("x := 1 + 2").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[x := 1 + 2]¹");
    }

    #[test]
    fn assignment_nested_constant_addition() {
        let p = parse("x := 1 + 2 + 3").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[x := 1 + 2 + 3]¹");
    }

    #[test]
    fn assignment_constant_subtraction() {
        let p = parse("x := 1 - 2").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[x := 1 - 2]¹");
    }

    #[test]
    fn assignment_constant_multiplication() {
        let p = parse("x := 1 * 2").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "[x := 1 * 2]¹");
    }

    #[test]
    fn if_statement_true_cond() {
        let p = parse("if true then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [true]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_false_cond() {
        let p = parse("if false then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [false]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_eq_check() {
        let p = parse("if 1 = 2 then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [1 = 2]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_neq_check() {
        let p = parse("if 1 != 2 then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [1 ≠ 2]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_lt_check() {
        let p = parse("if 1 < 2 then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [1 < 2]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_leq_check() {
        let p = parse("if 1 <= 2 then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [1 ≤ 2]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_gt_check() {
        let p = parse("if 1 > 2 then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [1 > 2]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_geq_check() {
        let p = parse("if 1 >= 2 then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [1 ≥ 2]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_and_cond() {
        let p = parse("if true and false then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [true ∧ false]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_or_cond() {
        let p = parse("if true or false then skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [true ∨ false]¹ then\n    [skip]²\nelse\n    [skip]³"
        );
    }

    #[test]
    fn if_statement_nested() {
        let p = parse("if 1 = 2 then if 3 = 4 then skip else skip else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [1 = 2]¹ then\n    if [3 = 4]² then\n        [skip]³\n    else\n        [skip]⁴\nelse\n    [skip]⁵"
        );
    }

    #[test]
    fn if_statement_nested_with_seq() {
        let p = parse("if 1 = 2 then x := 1; y := 2 else skip").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "if [1 = 2]¹ then\n    [x := 1]²;\n    [y := 2]³\nelse\n    [skip]⁴"
        );
    }

    #[test]
    fn while_loop() {
        let p = parse("while true do (skip)").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "while [true]¹ do (\n    [skip]²)");
    }

    #[test]
    fn while_loop_nested() {
        let p = parse("while true do (while false do (skip))").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "while [true]¹ do (\n    while [false]² do (\n        [skip]³))"
        );
    }

    #[test]
    fn while_loop_followed_by_statement() {
        let p = parse("while true do (skip); skip").unwrap();
        let l = label(p);

        assert_eq!(l.to_string(), "while [true]¹ do (\n    [skip]²);\n[skip]³");
    }

    #[test]
    fn complete_program() {
        let p = parse("y := x; x := 1; while y > 1 do (z := z * y; y := y - 1); y := 0").unwrap();
        let l = label(p);

        assert_eq!(
            l.to_string(),
            "[y := x]¹;\n[x := 1]²;\nwhile [y > 1]³ do (\n    [z := z * y]⁴;\n    [y := y - 1]⁵);\n[y := 0]⁶"
        );
    }
}
