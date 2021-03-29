pub type NodeId = u32;

pub trait Node {
    fn id(&self) -> NodeId;
}

macro_rules! implement_node {
    ($($name:ident),*) => {
        $(
            impl Node for $name {
                fn id(&self) -> NodeId {
                    self.id
                }
            }
        )*
    }
}
implement_node!(VarDecl, Lit, Ident, Binary, Unary);

macro_rules! Enum {
    ($name:ident [$({$Var:ident, $var:ident}),*]) => {
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

Enum!(Stmt [{Expr, expr}, {VarDecl, var_decl}]);
Enum!(Expr [
    {Lit, lit},
    {Ident, ident},
    {Binary, binary},
    {Unary, unary}
]);

#[derive(Debug, PartialEq)]
pub struct Ident {
    pub id: NodeId,
    pub raw: String,
}

impl Ident {
    pub fn new(id: NodeId, raw: String) -> Self {
        Ident { id, raw }
    }
}

#[derive(Debug)]
pub struct Lit {
    pub id: NodeId,
    pub kind: LitKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LitKind {
    Int(u64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl Lit {
    pub fn new(id: NodeId, kind: LitKind) -> Self {
        Lit { id, kind }
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
}

impl Unary {
    pub fn new(id: NodeId, op: UnaryOp, expr: Expr) -> Self {
        Unary {
            id,
            op,
            expr: Box::new(expr),
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

/// let <name> = <expr>;
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
