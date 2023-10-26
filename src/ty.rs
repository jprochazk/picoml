use crate::error::{Error, Result};
use crate::syn::BinaryOp;
use crate::syn::Expr;
use crate::syn::ExprKind;
use crate::syn::Ident;
use crate::syn::Span;
use ena::unify::EqUnifyValue;
use ena::unify::InPlaceUnificationTable;
use ena::unify::UnifyKey;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Deref;
use std::ops::DerefMut;

// TODO: Spanned<Expr> -> Typed<Expr>
pub fn infer(e: Expr) -> Result<TExpr> {
  let mut ctx = InferCtx::new();
  match infer_(&mut ctx, e) {
    Ok(mut e) => {
      /* println!("constraints:");
      for constraint in &ctx.constraints {
        println!("  {constraint}");
      } */

      /* println!("type (before unification):");
      println!("  {ty}"); */

      let mut bounds = Bounds::new();
      let mut unbound = HashSet::new();
      unify(&mut ctx, &mut bounds)?;
      substitute_in(&mut ctx, &mut unbound, &mut e);

      if !bounds.is_empty() {
        println!("bounds:");
        for (var, bounds) in &bounds {
          print!("  {var} : ");
          let mut bounds = bounds.iter().peekable();
          while let Some(bound) = bounds.next() {
            print!("{bound:?}");
            if bounds.peek().is_some() {
              print!(" + ");
            }
          }
          println!();
        }
      }

      /* println!("type (after unification):");
      println!("  {ty}"); */

      Ok(e)
    }
    Err(e) => Err(e),
  }
}

struct InferCtx<'a> {
  table: InPlaceUnificationTable<TypeVar>,
  constraints: Vec<Constraint>,
  bindings: Bindings<'a>,
}

impl<'a> InferCtx<'a> {
  fn new() -> Self {
    Self {
      table: InPlaceUnificationTable::default(),
      constraints: Vec::new(),
      bindings: Bindings::default(),
    }
  }

  fn type_var(&mut self) -> TypeVar {
    self.table.new_key(None)
  }

  fn with<T>(
    &mut self,
    (name, ty): (&'a str, Type),
    f: impl FnOnce(&mut Self) -> Result<T>,
  ) -> Result<T> {
    let prev = self.bindings.insert(name, ty);
    let out = f(self);
    self.bindings.restore(prev);
    out
  }

  fn eq(&mut self, lhs: Type, rhs: Type) {
    self.constraints.push(Constraint::Eq(lhs, rhs))
  }

  fn supports(&mut self, ty: Type, op: BinaryOp) {
    self.constraints.push(Constraint::Supports(ty, op))
  }
}

fn unify(ctx: &mut InferCtx<'_>, bounds: &mut Bounds) -> Result<()> {
  for c in std::mem::take(&mut ctx.constraints) {
    match c {
      Constraint::Eq(lhs, rhs) => unify_eq(ctx, bounds, lhs, rhs)?,
      Constraint::Supports(ty, op) => unify_supports(ctx, bounds, ty, op)?,
    }
  }

  Ok(())
}

fn unify_eq(ctx: &mut InferCtx<'_>, bounds: &mut Bounds, lhs: Type, rhs: Type) -> Result<()> {
  let lhs = normalize(ctx, bounds, lhs)?;
  let rhs = normalize(ctx, bounds, rhs)?;
  match (lhs, rhs) {
    (Type::Int, Type::Int) => Ok(()),
    (Type::Bool, Type::Bool) => Ok(()),
    (Type::Fn(param0, ret0), Type::Fn(param1, ret1)) => {
      unify_eq(ctx, bounds, *param0, *param1)?;
      unify_eq(ctx, bounds, *ret0, *ret1)
    }
    (Type::Var(a), Type::Var(b)) => ctx
      .table
      .unify_var_var(a, b)
      .map_err(|(l, r)| type_error_mismatch(l, r)),
    (Type::Var(var), ty) | (ty, Type::Var(var)) => {
      if var.occurs_in(&ty) {
        return Err(type_error_infinite_size(ty));
      }

      ctx
        .table
        .unify_var_value(var, Some(ty))
        .map_err(|(l, r)| type_error_mismatch(l, r))
    }
    (l, r) => Err(type_error_mismatch(l, r)),
  }
}

fn unify_supports(
  ctx: &mut InferCtx<'_>,
  bounds: &mut Bounds,
  ty: Type,
  op: BinaryOp,
) -> Result<()> {
  let ty = normalize(ctx, bounds, ty)?;
  match (ty, op) {
    (
      Type::Int,
      BinaryOp::Add
      | BinaryOp::Subtract
      | BinaryOp::Multiply
      | BinaryOp::Divide
      | BinaryOp::Equal
      | BinaryOp::NotEqual
      | BinaryOp::LessThan
      | BinaryOp::LessThanOrEqual
      | BinaryOp::GreaterThan
      | BinaryOp::GreaterThanOrEqual,
    ) => Ok(()),
    (Type::Bool, BinaryOp::Equal | BinaryOp::NotEqual) => Ok(()),
    (Type::Var(var), op) => {
      let bound = Bound::Supports(op);
      bounds
        .entry(var)
        .and_modify(|v| {
          v.insert(bound);
        })
        .or_insert_with(|| [bound].into_iter().collect());
      Ok(())
    }
    (ty, op) => Err(type_error_unsupported_op(ty, op)),
  }
}

impl TypeVar {
  fn occurs_in(&self, ty: &Type) -> bool {
    match ty {
      Type::Int => false,
      Type::Bool => false,
      Type::Fn(param, ret) => self.occurs_in(param) || self.occurs_in(ret),
      Type::Var(var) => self == var,
    }
  }
}

fn type_error_mismatch(l: Type, r: Type) -> Error {
  Error::new(format!(
    "type mismatch between\
    \n  {l}\
    \nand\
    \n  {r}"
  ))
}

fn type_error_infinite_size(ty: Type) -> Error {
  Error::new(format!(
    "the type\
    \n  `{ty}`\
    \nhas an infinite size"
  ))
}

fn type_error_unsupported_op(ty: Type, op: BinaryOp) -> Error {
  let name = match op {
    BinaryOp::Add => "addition",
    BinaryOp::Subtract => "subtraction",
    BinaryOp::Multiply => "multiplication",
    BinaryOp::Divide => "division",
    BinaryOp::Equal
    | BinaryOp::NotEqual
    | BinaryOp::LessThan
    | BinaryOp::LessThanOrEqual
    | BinaryOp::GreaterThan
    | BinaryOp::GreaterThanOrEqual => "comparison",
  };
  Error::new(format!(
    "the type\
    \n  `{ty}`\
    \ndoes not support {name}"
  ))
}

fn substitute_in(ctx: &mut InferCtx<'_>, unbound: &mut HashSet<TypeVar>, e: &mut TExpr) {
  substitute(ctx, unbound, &mut e.ty)
}

fn substitute(ctx: &mut InferCtx<'_>, unbound: &mut HashSet<TypeVar>, ty: &mut Type) {
  match ty {
    Type::Int => {}
    Type::Bool => {}
    Type::Fn(param, ret) => {
      substitute(ctx, unbound, param);
      substitute(ctx, unbound, ret);
    }
    Type::Var(var) => {
      let root = ctx.table.find(*var);
      let Some(mut inner) = ctx.table.probe_value(root) else {
        unbound.insert(*var);
        return;
      };
      substitute(ctx, unbound, &mut inner);
      *ty = inner;
    }
  }
}

fn normalize(ctx: &mut InferCtx<'_>, bounds: &mut Bounds, ty: Type) -> Result<Type> {
  let normalized = match ty {
    Type::Int => Type::Int,
    Type::Bool => Type::Bool,
    Type::Fn(param, ret) => Type::fn_(
      normalize(ctx, bounds, *param)?,
      normalize(ctx, bounds, *ret)?,
    ),
    Type::Var(var) => match ctx.table.probe_value(var) {
      Some(ty) => {
        let ty = normalize(ctx, bounds, ty)?;
        if let Some(var_bounds) = bounds.get(&var).cloned() {
          for bound in var_bounds {
            match bound {
              Bound::Supports(op) => unify_supports(ctx, bounds, ty.clone(), op)?,
            }
          }
        }
        ty
      }
      None => Type::Var(var),
    },
  };
  Ok(normalized)
}

type Bounds = HashMap<TypeVar, HashSet<Bound>>;

fn infer_<'src>(ctx: &mut InferCtx<'src>, e: Expr<'src>) -> Result<TExpr<'src>> {
  let span = e.span;
  match e.into_inner() {
    ExprKind::App { callee, arg } => {
      let arg = infer_(ctx, *arg)?;
      let ret = Type::Var(ctx.type_var());
      let callee = check_(ctx, *callee, Type::fn_(arg.ty.clone(), ret.clone()))?;
      Ok(Typed::new(span, ret, TExprKind::app(callee, arg)))
    }
    ExprKind::Let { name, value, in_ } => {
      let value = infer_(ctx, *value)?;
      let in_ = ctx.with((&name, value.ty.clone()), |ctx| infer_(ctx, *in_))?;
      Ok(Typed::new(
        span,
        in_.ty.clone(),
        TExprKind::let_(name, value, in_),
      ))
    }
    ExprKind::Fn { param, body } => {
      let param_ty = Type::Var(ctx.type_var());
      let ret = ctx.with((&param, param_ty.clone()), |ctx| infer_(ctx, *body))?;
      Ok(Typed::new(
        span,
        Type::fn_(param_ty, ret.ty.clone()),
        TExprKind::fn_(param, ret),
      ))
    }
    ExprKind::If { cond, then, else_ } => {
      let cond = check_(ctx, *cond, Type::Bool)?;
      let then = infer_(ctx, *then)?;
      let else_ = check_(ctx, *else_, then.ty.clone())?;
      Ok(Typed::new(
        span,
        else_.ty.clone(),
        TExprKind::if_(cond, then, else_),
      ))
    }
    ExprKind::Int(value) => Ok(Typed::new(span, Type::Int, TExprKind::Int(value))),
    ExprKind::Bool(value) => Ok(Typed::new(span, Type::Bool, TExprKind::Bool(value))),
    ExprKind::Var(name) => {
      let ty = ctx
        .bindings
        .get(&name)
        .cloned()
        .ok_or_else(|| Error::new(format!("undefined variable {name}")))?;
      Ok(Typed::new(span, ty, TExprKind::Var(name)))
    }
    ExprKind::Binary { lhs, op, rhs } => {
      let lhs = infer_(ctx, *lhs)?;
      let rhs = check_(ctx, *rhs, lhs.ty.clone())?;
      ctx.supports(rhs.ty.clone(), op);
      Ok(Typed::new(
        span,
        rhs.ty.clone(),
        TExprKind::binary(lhs, op, rhs),
      ))
    }
  }
}

fn check_<'src>(ctx: &mut InferCtx<'src>, e: Expr<'src>, ty: Type) -> Result<TExpr<'src>> {
  let span = e.span;
  match (e.into_inner(), ty) {
    (ExprKind::Int(value), Type::Int) => Ok(Typed::new(span, Type::Int, TExprKind::Int(value))),
    (ExprKind::Bool(value), Type::Bool) => Ok(Typed::new(span, Type::Bool, TExprKind::Bool(value))),
    (ExprKind::Fn { param, body }, Type::Fn(param_ty, ret_ty)) => {
      let body = ctx.with((&param, Type::clone(&param_ty)), |ctx| {
        check_(ctx, *body, *ret_ty)
      })?;
      Ok(Typed::new(
        span,
        Type::fn_(*param_ty, body.ty.clone()),
        TExprKind::fn_(param, body),
      ))
    }
    (lhs, rhs) => {
      let lhs = infer_(ctx, Expr::new(span, lhs))?;
      ctx.eq(lhs.ty.clone(), rhs);
      Ok(lhs)
    }
  }
}

#[derive(Default)]
pub struct Bindings<'a> {
  next_index: usize,
  by_index: HashMap<usize, Type>,
  by_name: HashMap<&'a str, usize>,
}

impl<'a> Bindings<'a> {
  fn insert(&mut self, name: &'a str, ty: Type) -> Option<(&'a str, usize)> {
    let index = self.next_index;
    self.next_index += 1;
    self.by_index.insert(index, ty);
    replace_in_map(&mut self.by_name, name, index)
  }

  fn restore(&mut self, pair: Option<(&'a str, usize)>) {
    if let Some((name, index)) = pair {
      let _ = replace_in_map(&mut self.by_name, name, index);
    }
  }

  fn get(&self, name: &str) -> Option<&Type> {
    self.by_name.get(name).map(|i| &self.by_index[i])
  }
}

fn replace_in_map<K, V>(m: &mut HashMap<K, V>, k: K, v: V) -> Option<(K, V)>
where
  K: Eq + Hash + Clone,
{
  match m.entry(k) {
    Entry::Occupied(mut entry) => {
      let k = entry.key().clone();
      let v = std::mem::replace(entry.get_mut(), v);
      Some((k, v))
    }
    Entry::Vacant(entry) => {
      entry.insert(v);
      None
    }
  }
}

#[derive(Debug)]
pub enum Constraint {
  Eq(Type, Type),
  Supports(Type, BinaryOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Bound {
  Supports(BinaryOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  Int,
  Bool,
  Fn(Box<Type>, Box<Type>),
  Var(TypeVar),
}

impl Type {
  fn fn_(param: Type, ret: Type) -> Self {
    Self::Fn(Box::new(param), Box::new(ret))
  }
}

impl EqUnifyValue for Type {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(u32);

impl UnifyKey for TypeVar {
  type Value = Option<Type>;

  fn index(&self) -> u32 {
    self.0
  }

  fn from_index(u: u32) -> Self {
    Self(u)
  }

  fn tag() -> &'static str {
    "TypeVar"
  }
}

impl Default for TypeVar {
  fn default() -> Self {
    Self(u32::MAX)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binding(u32);

impl Display for Constraint {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Constraint::Eq(lhs, rhs) => write!(f, "{lhs} == {rhs}"),
      Constraint::Supports(ty, op) => write!(f, "{ty}: {op:?}"),
    }
  }
}

impl Display for TypeVar {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "'{}", self.0)
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Int => f.write_str("int"),
      Type::Bool => f.write_str("bool"),
      Type::Fn(param_ty, ret_ty) => write!(f, "{param_ty} -> {ret_ty}"),
      Type::Var(var) => write!(f, "{var}"),
    }
  }
}

#[derive(Debug)]
pub enum TExprKind<'src> {
  App {
    callee: Box<TExpr<'src>>,
    arg: Box<TExpr<'src>>,
  },
  Let {
    name: Ident<'src>,
    value: Box<TExpr<'src>>,
    in_: Box<TExpr<'src>>,
  },
  Fn {
    param: Ident<'src>,
    body: Box<TExpr<'src>>,
  },
  If {
    cond: Box<TExpr<'src>>,
    then: Box<TExpr<'src>>,
    else_: Box<TExpr<'src>>,
  },
  Binary {
    lhs: Box<TExpr<'src>>,
    op: BinaryOp,
    rhs: Box<TExpr<'src>>,
  },
  Int(i64),
  Bool(bool),
  Var(Ident<'src>),
}

impl<'src> TExprKind<'src> {
  pub fn app(callee: TExpr<'src>, arg: TExpr<'src>) -> Self {
    Self::App {
      callee: Box::new(callee),
      arg: Box::new(arg),
    }
  }

  pub fn let_(name: Ident<'src>, value: TExpr<'src>, in_: TExpr<'src>) -> Self {
    Self::Let {
      name,
      value: Box::new(value),
      in_: Box::new(in_),
    }
  }

  pub fn fn_(param: Ident<'src>, body: TExpr<'src>) -> Self {
    Self::Fn {
      param,
      body: Box::new(body),
    }
  }

  pub fn if_(cond: TExpr<'src>, then: TExpr<'src>, else_: TExpr<'src>) -> Self {
    Self::If {
      cond: Box::new(cond),
      then: Box::new(then),
      else_: Box::new(else_),
    }
  }

  pub fn binary(lhs: TExpr<'src>, op: BinaryOp, rhs: TExpr<'src>) -> Self {
    Self::Binary {
      lhs: Box::new(lhs),
      op,
      rhs: Box::new(rhs),
    }
  }
}

pub type TExpr<'src> = Typed<TExprKind<'src>>;

pub struct Typed<T> {
  pub span: Span,
  pub ty: Type,
  inner: T,
}

impl<T> Typed<T> {
  pub fn new(span: Span, ty: Type, inner: T) -> Self {
    Self { span, ty, inner }
  }

  pub fn into_inner(self) -> T {
    self.inner
  }
}

impl<T: Debug> Debug for Typed<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Debug::fmt(&self.inner, f)
  }
}

impl<T> Deref for Typed<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.inner
  }
}

impl<T> DerefMut for Typed<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.inner
  }
}

impl Display for TExpr<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.inner {
      TExprKind::App { callee, arg } => write!(f, "{callee} {arg}"),
      TExprKind::Let { name, value, in_ } => {
        write!(f, "let {name} : {} = {value} in {in_}", value.ty)
      }
      TExprKind::Fn { param, body } => {
        if let Type::Fn(param_ty, _) = &self.ty {
          write!(f, "fn {param}: {param_ty} -> {body}")
        } else {
          write!(f, "fn {param} -> {body}")
        }
      }
      TExprKind::If { cond, then, else_ } => write!(f, "if {cond} then {then} else {else_}"),
      TExprKind::Int(value) => write!(f, "{value}"),
      TExprKind::Bool(value) => write!(f, "{value}"),
      TExprKind::Var(name) => write!(f, "{name}"),
      TExprKind::Binary { lhs, op, rhs } => write!(f, "{lhs} {op} {rhs}"),
    }
  }
}
