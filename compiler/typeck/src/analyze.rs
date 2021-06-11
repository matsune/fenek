use crate::scope::*;
use ast::Node;
use error::{CompileError, Result, TypeCkError};
use hir::def;
use pos::Pos;
use std::collections::{HashMap, HashSet};
use typed_arena::Arena;
use types::infer::InferTy;
use types::solver::Solver;
use types::ty;

pub type NodeMap<T> = HashMap<ast::NodeId, T>;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

type Def<'infer> = def::Def<&'infer InferTy<'infer>>;

pub struct TyAnalyzer<'infer> {
    def_arena: &'infer Arena<Def<'infer>>,
    node_ty_map: NodeMap<&'infer InferTy<'infer>>,
    node_def_map: NodeMap<Def<'infer>>,
    node_struct_map: NodeMap<&'infer ty::StructType>,
    struct_map: HashMap<String, &'infer ty::StructType>,
    scopes: Scopes<'infer>,
    solver: &'infer Solver<'infer>,
}

impl<'infer> TyAnalyzer<'infer> {
    pub fn new(def_arena: &'infer Arena<Def<'infer>>, solver: &'infer Solver<'infer>) -> Self {
        TyAnalyzer {
            def_arena,
            node_ty_map: NodeMap::new(),
            node_def_map: NodeMap::new(),
            node_struct_map: NodeMap::new(),
            struct_map: HashMap::new(),
            scopes: Scopes::default(),
            solver,
        }
    }

    fn get_infer_type(&self, ty: &ast::Ty) -> Result<&'infer InferTy<'infer>> {
        let ty = match &ty.kind {
            ast::TyKind::Raw(tok) => match tok.raw.as_str() {
                "i8" => self.solver.ty_arena.alloc_i8(),
                "i16" => self.solver.ty_arena.alloc_i16(),
                "i32" => self.solver.ty_arena.alloc_i32(),
                "i64" => self.solver.ty_arena.alloc_i64(),
                "f32" => self.solver.ty_arena.alloc_f32(),
                "f64" => self.solver.ty_arena.alloc_f64(),
                "bool" => self.solver.ty_arena.alloc_bool(),
                // "string" => self.solver.ty_arena.alloc_string(),
                "void" => self.solver.ty_arena.alloc_void(),
                _ => {
                    let struct_id = self.struct_map.get(&tok.raw).ok_or_else(|| {
                        compile_error(tok.pos, TypeCkError::UndefinedType(tok.raw.clone()))
                    })?;
                    self.solver.ty_arena.alloc_struct(*struct_id)
                }
            },
            ast::TyKind::Ptr(ty) => self.solver.ty_arena.alloc_ptr(self.get_infer_type(ty)?),
        };
        Ok(ty)
    }

    pub fn analyze_module(
        mut self,
        module: &ast::Module,
    ) -> Result<(NodeMap<&'infer InferTy<'infer>>, NodeMap<Def<'infer>>)> {
        self.analyze_structs(&module.structs)?;

        for fun in &module.funs {
            if self.scopes.lookup_fun(&fun.name.raw, true).is_some() {
                return Err(compile_error(
                    fun.name.pos,
                    TypeCkError::AlreadyDefinedFun(fun.name.raw.clone()),
                ));
            }
            let fun_def = self.make_fun_def(&fun)?;
            self.scopes.insert(fun.name.raw.clone(), fun_def);
            self.node_def_map.insert(fun.id, fun_def.clone());
        }

        for fun in &module.funs {
            self.analyze_fun(&fun)?;
        }
        Ok((self.node_ty_map, self.node_def_map))
    }

    fn get_type(&self, ty: &ast::Ty) -> Result<ty::Type> {
        let ty = match &ty.kind {
            ast::TyKind::Raw(ident) => match ident.raw.as_str() {
                "i8" => ty::Type::Int(ty::IntType::I8),
                "i16" => ty::Type::Int(ty::IntType::I16),
                "i32" => ty::Type::Int(ty::IntType::I32),
                "i64" => ty::Type::Int(ty::IntType::I64),
                "f32" => ty::Type::Float(ty::FloatType::F32),
                "f64" => ty::Type::Float(ty::FloatType::F64),
                "bool" => ty::Type::Bool,
                "void" => ty::Type::Void,
                _ => {
                    let strukt = self.struct_map.get(&ident.raw).ok_or_else(|| {
                        compile_error(ident.pos, TypeCkError::UndefinedType(ident.raw.clone()))
                    })?;
                    ty::Type::Struct((*strukt).clone())
                }
            },
            ast::TyKind::Ptr(ty) => ty::Type::Ptr(Box::new(self.get_type(&ty)?)),
        };
        Ok(ty)
    }

    pub fn analyze_structs(&mut self, structs: &[ast::Struct]) -> Result<()> {
        for strukt in structs {
            if self.struct_map.contains_key(&strukt.name.raw) {
                return Err(compile_error(
                    strukt.name.pos,
                    TypeCkError::AlreadyDefinedStruct(strukt.name.raw.clone()),
                ));
            }
            let struct_ty = self.solver.add_struct(&strukt.name.raw);
            self.struct_map.insert(strukt.name.raw.clone(), struct_ty);
            self.node_struct_map.insert(strukt.id, struct_ty);
        }

        for strukt in structs {
            let struct_ty = self.node_struct_map[&strukt.id];
            let mut members = Vec::new();
            for member in &strukt.members {
                let is_mut = member.is_mut();
                let name = member.name.raw.clone();
                let ty = self.get_type(&member.ty)?;
                self.check_infinite_size_struct(struct_ty, &ty)
                    .map_err(|err| compile_error(member.ty.pos(), err))?;
                members.push(ty::StructMember { is_mut, name, ty });
            }
            struct_ty.members.replace(members);
        }

        Ok(())
    }

    fn check_infinite_size_struct(
        &self,
        strukt: &ty::StructType,
        member_ty: &ty::Type,
    ) -> std::result::Result<(), TypeCkError> {
        if let ty::Type::Struct(ty) = member_ty {
            if strukt.id == ty.id {
                return Err(TypeCkError::RecursiveType(strukt.name.clone()));
            }
            for member in ty.members.borrow().iter() {
                self.check_infinite_size_struct(strukt, &member.ty)?;
            }
        }
        Ok(())
    }

    fn make_fun_def(
        &mut self,
        fun: &ast::Fun,
    ) -> Result<&'infer def::Def<&'infer InferTy<'infer>>> {
        let mut arg_muts = Vec::new();
        let mut arg_tys = Vec::new();
        let mut arg_names = HashSet::new();
        for arg in &fun.args {
            arg_muts.push(arg.is_mut());

            let arg_name = &arg.name.raw;
            if arg_names.contains(arg_name) {
                return Err(compile_error(
                    arg.name.pos,
                    TypeCkError::AlreadyDefinedVariable(arg_name.clone()),
                ));
            }
            arg_names.insert(arg_name);
            let arg_ty = self.get_infer_type(&arg.ty)?;
            // if !arg_ty.is_variable() {
            //     return Err(compile_error(pos, TypeCkError::InvalidType));
            // }
            arg_tys.push(arg_ty);
            self.node_ty_map.insert(arg.id, arg_ty);
        }

        let (ret_mut, ret_ty) = match &fun.ret_ty {
            Some(ret_ty) => {
                let ret_ty_id = ret_ty.ty.id;
                let ty = self.get_infer_type(&ret_ty.ty)?;
                self.node_ty_map.insert(ret_ty_id, ty);
                (ret_ty.is_mut(), ty)
            }
            None => (false, self.solver.ty_arena.alloc_void()),
        };
        let fun_ty = self.solver.ty_arena.alloc_fun(arg_tys, ret_ty);

        Ok(self.alloc_def_fun(fun_ty, arg_muts, ret_mut))
    }

    fn alloc_def_fun(
        &self,
        ty: &'infer InferTy<'infer>,
        arg_muts: Vec<bool>,
        ret_mut: bool,
    ) -> &'infer Def<'infer> {
        let id = self.def_arena.len();
        self.def_arena.alloc(def::Def::Fun(def::DefFun {
            id,
            ty,
            arg_muts,
            ret_mut,
        }))
    }

    fn alloc_def_var(&self, ty: &'infer InferTy<'infer>, is_mut: bool) -> &'infer Def<'infer> {
        let id = self.def_arena.len();
        self.def_arena
            .alloc(def::Def::Var(def::DefVar { id, ty, is_mut }))
    }

    fn analyze_fun(&mut self, fun: &ast::Fun) -> Result<()> {
        let fun_def = self.scopes.lookup_fun(&fun.name.raw, true).unwrap();
        self.scopes.push_scope();

        for (idx, arg) in fun.args.iter().enumerate() {
            let ty = fun_def.ty.as_fun().args[idx];
            let def = self.alloc_def_var(ty, arg.is_mut());
            self.scopes.insert(arg.name.raw.clone(), def);
            self.node_def_map.insert(arg.id, def.clone());
        }

        self.analyze_block(&fun.block, &fun_def.ty.as_fun().ret)?;

        self.scopes.pop_scope();
        Ok(())
    }

    fn analyze_block(&mut self, block: &ast::Block, ret_ty: &'infer InferTy<'infer>) -> Result<()> {
        for stmt in &block.stmts {
            self.analyze_stmt(&stmt, &ret_ty)?;
        }
        Ok(())
    }

    fn analyze_stmt(
        &mut self,
        stmt: &ast::Stmt,
        current_fn_ret_ty: &'infer InferTy<'infer>,
    ) -> Result<()> {
        match &stmt {
            ast::Stmt::Empty(_) => Ok(()),
            ast::Stmt::Expr(expr) => self.analyze_expr(&expr).map(|_| ()),
            ast::Stmt::VarDecl(var_decl) => self.analyze_var_decl(&var_decl),
            ast::Stmt::Ret(ret_stmt) => self.analyze_ret_stmt(&ret_stmt, current_fn_ret_ty),
            ast::Stmt::Assign(assign) => self.analyze_assign(&assign),
            ast::Stmt::If(if_stmt) => self.analyze_if_stmt(&if_stmt, current_fn_ret_ty),
        }
    }

    fn analyze_var_decl(&mut self, var_decl: &ast::VarDecl) -> Result<()> {
        if self.scopes.lookup_var(&var_decl.name.raw, true).is_some() {
            return Err(compile_error(
                var_decl.name.pos,
                TypeCkError::AlreadyDefinedVariable(var_decl.name.raw.clone()),
            ));
        }
        let init_ty = self.analyze_expr(&var_decl.init)?;
        let var_ty = match &var_decl.ty {
            Some(ty) => {
                let pos = ty.pos();
                let ty = self.get_infer_type(&ty)?;
                // if !ty.kind.is_variable() {
                //     return Err(compile_error(pos, TypeCkError::InvalidType));
                // }
                ty
            }
            None => self.solver.ty_arena.alloc_any(),
        };
        self.solver
            .bind(var_ty, init_ty)
            .map_err(|err| CompileError::new(var_decl.name.pos, err.into()))?;
        let def = self.alloc_def_var(init_ty, var_decl.is_mut());
        self.scopes.insert(var_decl.name.raw.clone(), def);
        self.node_def_map.insert(var_decl.id, def.clone());
        Ok(())
    }

    fn analyze_ret_stmt(
        &mut self,
        ret_stmt: &ast::RetStmt,
        current_fn_ret_ty: &'infer InferTy<'infer>,
    ) -> Result<()> {
        let ty = match &ret_stmt.expr {
            Some(expr) => self.analyze_expr(&expr)?,
            None => self.solver.ty_arena.alloc_void(),
        };
        let pos = ret_stmt
            .expr
            .as_ref()
            .map(|e| e.pos())
            .unwrap_or(ret_stmt.pos());
        self.solver
            .bind(ty, current_fn_ret_ty)
            .map_err(|err| CompileError::new(pos, err.into()))?;
        Ok(())
    }

    fn analyze_assign(&mut self, assign: &ast::Assign) -> Result<()> {
        let left_ty = self.analyze_expr(&assign.left)?;
        let right_ty = self.analyze_expr(&assign.right)?;
        self.solver
            .bind(left_ty, right_ty)
            .map_err(|err| CompileError::new(assign.pos(), err.into()))?;
        Ok(())
    }

    fn analyze_if_stmt(
        &mut self,
        if_stmt: &ast::IfStmt,
        current_fn_ret_ty: &'infer InferTy<'infer>,
    ) -> Result<()> {
        let expr_ty = self.analyze_expr(&if_stmt.expr.as_ref().unwrap())?;
        self.solver
            .bind(expr_ty, self.solver.ty_arena.alloc_bool())
            .map_err(|err| CompileError::new(if_stmt.expr.as_ref().unwrap().pos(), err.into()))?;
        self.analyze_block(&if_stmt.block, &current_fn_ret_ty)?;
        if let Some(else_if) = &if_stmt.else_if {
            self.analyze_else(&else_if, &current_fn_ret_ty)?;
        }
        Ok(())
    }

    fn analyze_else(
        &mut self,
        else_stmt: &ast::IfStmt,
        current_fn_ret_ty: &'infer InferTy<'infer>,
    ) -> Result<()> {
        if let Some(expr) = &else_stmt.expr {
            let expr_ty = self.analyze_expr(&expr)?;
            self.solver
                .bind(expr_ty, self.solver.ty_arena.alloc_bool())
                .map_err(|err| CompileError::new(expr.pos(), err.into()))?;
        }
        self.analyze_block(&else_stmt.block, &current_fn_ret_ty)?;
        match &else_stmt.else_if {
            Some(else_if) => self.analyze_else(&else_if, current_fn_ret_ty)?,
            None => {}
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &ast::Expr) -> Result<&'infer InferTy<'infer>> {
        let ty = match &expr {
            ast::Expr::Lit(lit) => self.analyze_lit(lit),
            ast::Expr::Path(path) => self.analyze_path(path)?,
            ast::Expr::Call(call) => self.analyze_call(call)?,
            ast::Expr::Binary(binary) => self.analyze_binary(binary)?,
            ast::Expr::Unary(unary) => self.analyze_unary(unary)?,
        };
        self.node_ty_map.insert(expr.id(), ty);
        Ok(ty)
    }

    fn analyze_lit(&mut self, lit: &ast::Lit) -> &'infer InferTy<'infer> {
        match lit.kind {
            ast::LitKind::Int { base: _ } => self.solver.ty_arena.alloc_int_lit(),
            ast::LitKind::Float => self.solver.ty_arena.alloc_float_lit(),
            ast::LitKind::Bool => self.solver.ty_arena.alloc_bool(),
            ast::LitKind::String => unimplemented!(),
        }
    }

    fn analyze_path(&mut self, path: &ast::Path) -> Result<&'infer InferTy<'infer>> {
        let def = self
            .scopes
            .lookup_var(&path.ident.raw, false)
            .ok_or_else(|| {
                compile_error(
                    path.pos(),
                    TypeCkError::UndefinedVariable(path.ident.raw.clone()),
                )
            })?;
        self.node_def_map.insert(path.id, Def::Var(def.clone()));
        Ok(def.ty)
    }

    fn analyze_call(&mut self, call: &ast::Call) -> Result<&'infer InferTy<'infer>> {
        let def = self
            .scopes
            .lookup_fun(&call.path.ident.raw, false)
            .ok_or_else(|| {
                compile_error(
                    call.path.pos(),
                    TypeCkError::UndefinedFun(call.path.ident.raw.clone()),
                )
            })?;
        self.node_def_map
            .insert(call.path.id, Def::Fun(def.clone()));
        if def.ty.as_fun().args.len() != call.args.len() {
            return Err(compile_error(call.pos(), TypeCkError::InvalidArgsCount));
        }
        let ty = self.solver.ty_arena.alloc_any();
        let arg_tys = &def.ty.as_fun().args;
        for (idx, arg) in call.args.iter().enumerate() {
            let ty = arg_tys[idx];
            let arg_ty = self.analyze_expr(&arg)?;
            self.solver
                .bind(arg_ty, ty)
                .map_err(|err| CompileError::new(arg.pos(), err.into()))?;
        }
        self.solver
            .bind(ty, &def.ty.as_fun().ret)
            .map_err(|err| CompileError::new(call.pos(), err.into()))?;
        Ok(ty)
    }

    fn analyze_binary(&mut self, binary: &ast::Binary) -> Result<&'infer InferTy<'infer>> {
        let lhs_ty = self.analyze_expr(&binary.lhs)?;
        let rhs_ty = self.analyze_expr(&binary.rhs)?;
        let ty = match binary.op.kind {
            ast::BinOpKind::Lt | ast::BinOpKind::Gt | ast::BinOpKind::Le | ast::BinOpKind::Ge => {
                self.solver
                    .bind(lhs_ty, rhs_ty)
                    .map_err(|err| CompileError::new(binary.lhs.pos(), err.into()))?;
                self.solver.ty_arena.alloc_bool()
            }
            _ => self
                .solver
                .bind(lhs_ty, rhs_ty)
                .map_err(|err| CompileError::new(binary.lhs.pos(), err.into()))?,
        };
        Ok(ty)
    }

    fn analyze_unary(&mut self, unary: &ast::Unary) -> Result<&'infer InferTy<'infer>> {
        let expr_ty = self.analyze_expr(&unary.expr)?;
        let ty = match unary.op.kind {
            ast::UnOpKind::Ref => self.solver.ty_arena.alloc_ptr(expr_ty),
            ast::UnOpKind::Deref => {
                let elem = self.solver.ty_arena.alloc_any();
                let ptr = self.solver.ty_arena.alloc_ptr(elem);
                self.solver
                    .bind(ptr, expr_ty)
                    .map_err(|err| CompileError::new(unary.expr.pos(), err.into()))?;
                elem
            }
            _ => {
                let ty = self.solver.ty_arena.alloc_any();
                self.solver
                    .bind(ty, expr_ty)
                    .map_err(|err| CompileError::new(unary.expr.pos(), err.into()))?
            }
        };
        Ok(ty)
    }
}
