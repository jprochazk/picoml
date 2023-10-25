use crate::error::{Error, Result};
use crate::syn::Expr;
use crate::syn::Meta;
use crate::syn::Spanned;
use ena::unify::EqUnifyValue;
use ena::unify::InPlaceUnificationTable;
use ena::unify::UnifyKey;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Deref;

pub type Typed<T> = Meta<T, Type>;

// TODO: Spanned<Expr> -> Typed<Expr>
pub fn infer(e: Spanned<Expr>) -> Result<Type> {
  let mut ctx = InferCtx::new();
  match infer_(&mut ctx, &e) {
    Ok(mut ty) => {
      /* println!("constraints:");
      for constraint in &ctx.constraints {
        println!("  {constraint}");
      } */

      /* println!("type (before unification):");
      println!("  {ty}"); */

      unify(&mut ctx)?;
      let mut unbound = HashSet::new();
      substitute(&mut ctx, &mut unbound, &mut ty);

      /* println!("type (after unification):");
      println!("  {ty}"); */

      Ok(ty)
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
}

fn unify(ctx: &mut InferCtx<'_>) -> Result<()> {
  for c in std::mem::take(&mut ctx.constraints) {
    match c {
      Constraint::Eq(lhs, rhs) => unify_eq(ctx, lhs, rhs)?,
    }
  }

  Ok(())
}

fn unify_eq(ctx: &mut InferCtx<'_>, lhs: Type, rhs: Type) -> Result<()> {
  let lhs = normalize(ctx, lhs);
  let rhs = normalize(ctx, rhs);
  match (lhs, rhs) {
    (Type::Int, Type::Int) => Ok(()),
    (Type::Bool, Type::Bool) => Ok(()),
    (Type::Fn(param0, ret0), Type::Fn(param1, ret1)) => {
      unify_eq(ctx, *param0, *param1)?;
      unify_eq(ctx, *ret0, *ret1)
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

fn normalize(ctx: &mut InferCtx<'_>, ty: Type) -> Type {
  match ty {
    Type::Int => Type::Int,
    Type::Bool => Type::Bool,
    Type::Fn(param, ret) => Type::fn_(normalize(ctx, *param), normalize(ctx, *ret)),
    Type::Var(var) => ctx
      .table
      .probe_value(var)
      .map(|ty| normalize(ctx, ty))
      .unwrap_or(Type::Var(var)),
  }
}

fn infer_<'a>(ctx: &mut InferCtx<'a>, e: &'a Spanned<Expr>) -> Result<Type> {
  match e.deref() {
    Expr::App { callee, arg } => {
      let arg_ty = infer_(ctx, arg)?;
      let ret_ty = Type::Var(ctx.type_var());
      let callee_ty = Type::fn_(arg_ty, ret_ty.clone());

      check_(ctx, callee, callee_ty).map(|_| ret_ty)
    }
    Expr::Let { name, value, in_ } => {
      let value_ty = infer_(ctx, value)?;
      ctx.with((name, value_ty), |ctx| infer_(ctx, in_))
    }
    Expr::Fn { param, body } => {
      let param_ty = Type::Var(ctx.type_var());
      let ret_ty = ctx.with((param, param_ty.clone()), |ctx| infer_(ctx, body))?;
      Ok(Type::fn_(param_ty, ret_ty))
    }
    Expr::If { cond, then, else_ } => {
      check_(ctx, cond, Type::Bool)?;
      let ty1 = infer_(ctx, then)?;
      check_(ctx, else_, ty1.clone())?;
      Ok(ty1)
    }
    Expr::Bool(_) => Ok(Type::Bool),
    Expr::Int(_) => Ok(Type::Int),
    Expr::Var(name) => ctx
      .bindings
      .get(name)
      .cloned()
      .ok_or_else(|| Error::new(format!("undefined variable {name}"))),
  }
}

fn check_<'a>(ctx: &mut InferCtx<'a>, e: &'a Spanned<Expr>, ty: Type) -> Result<()> {
  match (e.deref(), ty) {
    (Expr::Int(_), Type::Int) => Ok(()),
    (Expr::Bool(_), Type::Bool) => Ok(()),
    (Expr::Fn { param, body }, Type::Fn(param_ty, ret_ty)) => {
      ctx.with((param, *param_ty), |ctx| check_(ctx, body, *ret_ty))?;
      Ok(())
    }
    (_, rhs) => {
      let lhs = infer_(ctx, e)?;
      ctx.eq(lhs, rhs);
      Ok(())
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
    }
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Int => f.write_str("int"),
      Type::Bool => f.write_str("bool"),
      Type::Fn(param_ty, ret_ty) => write!(f, "{param_ty} -> {ret_ty}"),
      Type::Var(var) => write!(f, "'{}", var.0),
    }
  }
}
