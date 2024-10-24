#[derive(Debug, Clone, PartialEq)]
pub enum AExp {
    Var(Var),
    Num(Num),
    Add(AddExp),
    Sub(SubExp),
    Mul(MulExp),
}

#[derive(Debug, PartialEq)]
pub enum BExp {
    True(TrueExp),
    False(FalseExp),
    Not(NotExp),
    BoolOp(BoolOp),
    RelOp(RelOp),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Assign(Assign),
    Skip,
    Seq(Seq),
    If(If),
    While(While),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    pub value: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AddExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SubExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MulExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, PartialEq)]
pub struct TrueExp;

#[derive(Debug, PartialEq)]
pub struct FalseExp;

#[derive(Debug, PartialEq)]
pub struct NotExp {
    pub exp: Box<BExp>,
}

#[derive(Debug, PartialEq)]
pub enum BoolOp {
    And(AndExp),
    Or(OrExp),
}

#[derive(Debug, PartialEq)]
pub struct AndExp {
    pub left: Box<BExp>,
    pub right: Box<BExp>,
}

#[derive(Debug, PartialEq)]
pub struct OrExp {
    pub left: Box<BExp>,
    pub right: Box<BExp>,
}

#[derive(Debug, PartialEq)]
pub enum RelOp {
    Eq(EqExp),
    Neq(NeqExp),
    Lt(LtExp),
    Leq(LeqExp),
    Gt(GtExp),
    Geq(GeqExp),
}

#[derive(Debug, PartialEq)]
pub struct EqExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, PartialEq)]
pub struct NeqExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, PartialEq)]
pub struct LtExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, PartialEq)]
pub struct LeqExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, PartialEq)]
pub struct GtExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, PartialEq)]
pub struct GeqExp {
    pub left: Box<AExp>,
    pub right: Box<AExp>,
}

#[derive(Debug, PartialEq)]
pub struct Assign {
    pub var: Var,
    pub exp: AExp,
}

#[derive(Debug, PartialEq)]
pub struct Seq {
    pub first: Box<Stmt>,
    pub second: Box<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub cond: BExp,
    pub then: Box<Stmt>,
    pub r#else: Box<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct While {
    pub cond: BExp,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq)]
pub enum LabStmt {
    Assign(LabAssign),
    Skip(LabSkip),
    Seq(LabSeq),
    If(LabIf),
    While(LabWhile),
}

#[derive(Debug, PartialEq)]
pub struct LabAssign {
    pub var: Var,
    pub exp: AExp,
    pub label: u32,
}

#[derive(Debug, PartialEq)]
pub struct LabSkip {
    pub label: u32,
}

#[derive(Debug, PartialEq)]
pub struct LabSeq {
    pub first: Box<LabStmt>,
    pub second: Box<LabStmt>,
}

#[derive(Debug, PartialEq)]
pub struct LabIf {
    pub cond: BExp,
    pub cond_label: u32,
    pub then: Box<LabStmt>,
    pub r#else: Box<LabStmt>,
}

#[derive(Debug, PartialEq)]
pub struct LabWhile {
    pub cond: BExp,
    pub cond_label: u32,
    pub body: Box<LabStmt>,
}