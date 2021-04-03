use error::Pos;

pub type NodeId = u32;

pub trait Node {
    fn id(&self) -> NodeId;
}

macro_rules! impl_node {
    ($name:ident) => {
        impl Node for $name {
            fn id(&self) -> NodeId {
                self.id
            }
        }
    };
}
impl_node!(Fun);
impl_node!(FunArg);
impl_node!(Block);
impl_node!(VarDecl);
impl_node!(Lit);
impl_node!(Ident);
impl_node!(Binary);
impl_node!(Unary);

macro_rules! Enum {
    ($name:ident [$(($Var:ident, $var:ident)),*]) => {
        #[derive(Debug)]
        pub enum $name {
            $(
                $Var($Var),
            )*
        }

        impl Node for $name {
            fn id(&self) -> NodeId {
                match self {
                    $(
                        $name::$Var(v) => v.id(),
                    )*
                }
            }
        }

        $(
            impl Into<$name> for $Var {
                fn into(self) -> $name {
                    $name::$Var(self)
                }
            }

            impl $name {
                paste::item! {
                    pub fn [<into_ $var >](self) -> $Var {
                        if let $name::$Var(v) = self {
                            return v;
                        }
                        panic!()
                    }
                }
            }

        )*
    };
}

pub struct Fun {
    pub id: NodeId,
    pub name: Ident,
    pub args: FunArgs,
    pub ret_ty: Option<Ident>,
    pub block: Block,
    pub pos: Pos,
}

impl Fun {
    pub fn new(
        id: NodeId,
        name: Ident,
        args: FunArgs,
        ret_ty: Option<Ident>,
        block: Block,
        pos: Pos,
    ) -> Self {
        Fun {
            id,
            name,
            args,
            ret_ty,
            block,
            pos,
        }
    }
}

pub type FunArgs = Vec<FunArg>;

pub struct FunArg {
    pub id: NodeId,
    pub name: Ident,
    pub ty: Ident,
}

impl FunArg {
    pub fn new(id: NodeId, name: Ident, ty: Ident) -> Self {
        FunArg { id, name, ty }
    }
}

pub struct Block {
    pub id: NodeId,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn new(id: NodeId, stmts: Vec<Stmt>) -> Self {
        Block { id, stmts }
    }
}

Enum!(Stmt [
    (VarDecl, var_decl),
    (Expr, expr)
]);

Enum!(Expr [
    (Lit, lit),
    (Ident, ident),
    (Binary, binary),
    (Unary, unary)
]);

impl Expr {
    pub fn pos(&self) -> Pos {
        match self {
            Expr::Lit(lit) => lit.pos,
            Expr::Ident(ident) => ident.pos,
            Expr::Binary(binary) => binary.lhs.pos(),
            Expr::Unary(unary) => unary.pos,
        }
    }
}

/// var <name> = <expr>;
#[derive(Debug)]
pub struct VarDecl {
    pub id: NodeId,
    pub name: Ident,
    pub init: Box<Expr>,
}

impl VarDecl {
    pub fn new(id: NodeId, name: Ident, init: Expr) -> Self {
        VarDecl {
            id,
            name,
            init: Box::new(init),
        }
    }
}

#[derive(Debug)]
pub struct Lit {
    pub id: NodeId,
    pub kind: LitKind,
    pub pos: Pos,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LitKind {
    Int(u64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl Lit {
    pub fn new(id: NodeId, kind: LitKind, pos: Pos) -> Self {
        Lit { id, kind, pos }
    }

    pub fn into_int(self) -> u64 {
        if let LitKind::Int(v) = self.kind {
            return v;
        }
        panic!()
    }

    pub fn into_float(self) -> f64 {
        if let LitKind::Float(v) = self.kind {
            return v;
        }
        panic!()
    }

    pub fn into_bool(self) -> bool {
        if let LitKind::Bool(v) = self.kind {
            return v;
        }
        panic!()
    }

    pub fn into_string(self) -> String {
        if let LitKind::String(v) = self.kind {
            return v;
        }
        panic!()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub id: NodeId,
    pub raw: String,
    pub pos: Pos,
}

impl Ident {
    pub fn new(id: NodeId, raw: String, pos: Pos) -> Self {
        Ident { id, raw, pos }
    }
}

#[derive(Debug)]
pub struct Binary {
    pub id: NodeId,
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Binary {
    pub fn new(id: NodeId, op: BinOp, lhs: Expr, rhs: Expr) -> Self {
        Binary {
            id,
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub symbol: String,
    pub precedence: u8,
}

impl BinOp {
    pub fn new(symbol: String, precedence: u8) -> Self {
        Self { symbol, precedence }
    }
}

#[derive(Debug)]
pub struct Unary {
    pub id: NodeId,
    pub op: UnaryOp,
    pub expr: Box<Expr>,
    pub pos: Pos,
}

impl Unary {
    pub fn new(id: NodeId, op: UnaryOp, expr: Expr, pos: Pos) -> Self {
        Unary {
            id,
            op,
            expr: Box::new(expr),
            pos,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    /// +
    Add,
    /// -
    Sub,
    /// !
    Not,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Add => "+",
                UnaryOp::Sub => "-",
                UnaryOp::Not => "!",
            }
        )
    }
}
