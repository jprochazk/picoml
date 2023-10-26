#[macro_use]
mod macros;

/*
EXPR =
  | APP
  | LET
  | FN
  | IF
  | BOOL
  | INT
  | VAR
  | GROUP

APP = EXPR EXPR
LET = "let" IDENT "=" EXPR "in" EXPR
FN = "fn" IDENT "->" EXPR
IF = "if" EXPR "then" EXPR "else" EXPR
BOOL = "true" | "false"
INT = [0-9] | ([1-9][0-9]+)
VAR = IDENT
GROUP = "(" EXPR ")"
*/

use crate::error::{Error, Result};
use logos::Lexer;
use logos::Logos;
use logos::SpannedIter;
use std::borrow::Borrow;
use std::fmt::Debug;
use std::fmt::Display;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Index;
use std::ops::Range;

pub struct Parser<'src> {
  src: &'src str,
  lexer: SpannedIter<'src, TokenKind>,
  current: Token,
  previous: Token,
}

const EOF: Token = Token {
  kind: t![eof],
  span: Span::empty(),
};

impl<'src> Parser<'src> {
  fn new(src: &'src str) -> Self {
    Self {
      src,
      lexer: Lexer::new(src).spanned(),
      current: EOF,
      previous: EOF,
    }
  }

  fn lexeme(&self, token: &Token) -> &'src str {
    &self.src[token.span]
  }

  fn end(&self) -> bool {
    self.current.kind == TokenKind::Eof
  }

  fn current(&self) -> &Token {
    &self.current
  }

  fn previous(&self) -> &Token {
    &self.previous
  }

  fn kind(&self) -> &TokenKind {
    &self.current.kind
  }

  fn at(&self, t: TokenKind) -> bool {
    self.current.kind == t
  }

  fn eat(&mut self, t: TokenKind) -> Result<bool> {
    if self.at(t) {
      self.bump()?;
      Ok(true)
    } else {
      Ok(false)
    }
  }

  fn can_begin_expr(&self) -> bool {
    for t in [
      t![let],
      t![fn],
      t![if],
      t![bool],
      t![int],
      t![ident],
      t!["("],
    ] {
      if self.at(t) {
        return true;
      }
    }
    false
  }

  fn bump(&mut self) -> Result<&Token> {
    let token = match self.lexer.next() {
      Some((Err(_), span)) => {
        return Err(Error::new(format!(
          "unexpected token `{}`",
          &self.src[span]
        )))
      }
      Some((Ok(kind), span)) => Token {
        kind,
        span: span.into(),
      },
      None => EOF,
    };
    self.previous = std::mem::replace(&mut self.current, token);
    Ok(self.previous())
  }

  fn must(&mut self, t: TokenKind) -> Result<()> {
    if self.current().kind == t {
      self.bump()?;
      Ok(())
    } else {
      Err(Error::new(format!(
        "expected {t:?}, got {:?}",
        self.current()
      )))
    }
  }
}

pub fn parse(src: &str) -> Result<Expr<'_>> {
  if src.is_empty() {
    return Err(Error::new("expected some input"));
  }

  let mut tokens = Parser::new(src);
  tokens.bump()?;
  let e = expr(&mut tokens)?;
  if !tokens.end() {
    return Err(Error::new("expected end of input"));
  }
  Ok(e)
}

fn expr<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  app(p)
}

fn app<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let mut lhs = eq(p)?;
  while !p.end() && p.can_begin_expr() {
    let rhs = eq(p)?;
    lhs = Spanned::new(lhs.span.to(rhs.span), ExprKind::app(lhs, rhs));
  }
  Ok(lhs)
}

fn eq<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let mut lhs = cmp(p)?;
  while !p.end() {
    let op = match p.kind() {
      t![==] => BinaryOp::Equal,
      t![!=] => BinaryOp::NotEqual,
      _ => break,
    };
    p.bump()?;
    let rhs = cmp(p)?;
    lhs = Spanned::new(lhs.span.to(rhs.span), ExprKind::binary(lhs, op, rhs))
  }
  Ok(lhs)
}

fn cmp<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let mut lhs = add(p)?;
  while !p.end() {
    let op = match p.kind() {
      t![<] => BinaryOp::LessThan,
      t![<=] => BinaryOp::LessThanOrEqual,
      t![>] => BinaryOp::GreaterThan,
      t![>=] => BinaryOp::GreaterThanOrEqual,
      _ => break,
    };
    p.bump()?; // op
    let rhs = add(p)?;
    lhs = Spanned::new(lhs.span.to(rhs.span), ExprKind::binary(lhs, op, rhs));
  }
  Ok(lhs)
}

fn add<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let mut lhs = mul(p)?;
  while !p.end() {
    let op = match p.kind() {
      t![+] => BinaryOp::Add,
      t![-] => BinaryOp::Subtract,
      _ => break,
    };
    p.bump()?;
    let rhs = mul(p)?;
    lhs = Spanned::new(lhs.span.to(rhs.span), ExprKind::binary(lhs, op, rhs))
  }
  Ok(lhs)
}

fn mul<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  let mut lhs = primary(p)?;
  while !p.end() {
    let op = match p.kind() {
      t![*] => BinaryOp::Multiply,
      t![/] => BinaryOp::Divide,
      _ => break,
    };
    p.bump()?;
    let rhs = primary(p)?;
    lhs = Spanned::new(lhs.span.to(rhs.span), ExprKind::binary(lhs, op, rhs))
  }
  Ok(lhs)
}

fn primary<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  match p.kind() {
    t![let] => let_(p),
    t![fn] => fn_(p),
    t![if] => if_(p),
    t![bool] => bool_(p),
    t![int] => int_(p),
    t![ident] => var_(p),
    t!["("] => group_(p),
    _ => Err(Error::new(format!(
      "unexpected token `{}`",
      &p.src[p.current().span]
    ))),
  }
}

fn let_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  spanned(p, |p| {
    p.must(t![let])?;
    let name = ident(p)?;
    p.must(t![=])?;
    let value = expr(p)?;
    p.must(t![in])?;
    let in_ = expr(p)?;

    Ok(ExprKind::let_(name, value, in_))
  })
}

fn fn_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  spanned(p, |p| {
    p.must(t![fn])?;
    let param = ident(p)?;
    p.must(t![->])?;
    let body = expr(p)?;

    Ok(ExprKind::fn_(param, body))
  })
}

fn if_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  spanned(p, |p| {
    p.must(t![if])?;
    let cond = expr(p)?;
    p.must(t![then])?;
    let then = expr(p)?;
    p.must(t![else])?;
    let else_ = expr(p)?;

    Ok(ExprKind::if_(cond, then, else_))
  })
}

fn bool_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  spanned(p, |p| {
    p.must(t![bool])?;
    let v = p
      .lexeme(p.previous())
      .parse()
      .map_err(|e| Error::new(format!("failed to parse bool: {e}")))?;
    Ok(ExprKind::Bool(v))
  })
}

fn int_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  spanned(p, |p| {
    p.must(t![int])?;
    let v = p
      .lexeme(p.previous())
      .parse()
      .map_err(|e| Error::new(format!("failed to parse bool: {e}")))?;
    Ok(ExprKind::Int(v))
  })
}

fn var_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  spanned(p, |p| Ok(ExprKind::Var(ident(p)?)))
}

fn group_<'src>(p: &mut Parser<'src>) -> Result<Expr<'src>> {
  p.must(t!["("])?;
  let inner = expr(p)?;
  p.must(t![")"])?;
  Ok(inner)
}

fn ident<'src>(p: &mut Parser<'src>) -> Result<Ident<'src>> {
  p.must(t![ident])?;
  Ok(Spanned::new(p.previous().span, p.lexeme(p.previous())))
}

#[inline]
fn spanned<'src, T>(
  p: &mut Parser<'src>,
  f: impl FnOnce(&mut Parser<'src>) -> Result<T>,
) -> Result<Spanned<T>> {
  let start = p.current().span.start;
  let inner = f(p);
  let end = p.previous().span.end;
  match inner {
    Ok(inner) => Ok(Spanned::new(start..end, inner)),
    Err(e) => Err(e),
  }
}

#[derive(Debug, Clone, Copy, Logos, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(skip r"[ \t\n\r]+")]
pub enum TokenKind {
  #[token("+")]
  Plus,
  #[token("-")]
  Minus,
  #[token("*")]
  Star,
  #[token("/")]
  Slash,
  #[token("<")]
  AngleL,
  #[token(">")]
  AngleR,
  #[token("<=")]
  AngleLEq,
  #[token(">=")]
  AngleREq,
  #[token("==")]
  EqEq,
  #[token("!=")]
  Neq,

  #[token("true")]
  #[token("false")]
  Bool,
  #[token("let")]
  Let,
  #[token("=")]
  Eq,
  #[token("in")]
  In,
  #[token("fn")]
  Fn,
  #[token("->")]
  Arrow,
  #[token("if")]
  If,
  #[token("then")]
  Then,
  #[token("else")]
  Else,
  #[regex(r"[0-9][0-9]*")]
  Int,
  #[regex(r"[a-zA-Z]+")]
  Ident,
  #[token("(")]
  GroupL,
  #[token(")")]
  GroupR,
  Eof,
}

#[derive(Debug)]
pub enum ExprKind<'src> {
  App {
    callee: Box<Expr<'src>>,
    arg: Box<Expr<'src>>,
  },
  Let {
    name: Ident<'src>,
    value: Box<Expr<'src>>,
    in_: Box<Expr<'src>>,
  },
  Fn {
    param: Ident<'src>,
    body: Box<Expr<'src>>,
  },
  If {
    cond: Box<Expr<'src>>,
    then: Box<Expr<'src>>,
    else_: Box<Expr<'src>>,
  },
  Binary {
    lhs: Box<Expr<'src>>,
    op: BinaryOp,
    rhs: Box<Expr<'src>>,
  },
  Int(i64),
  Bool(bool),
  Var(Ident<'src>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOp {
  Add,
  Subtract,
  Multiply,
  Divide,
  Equal,
  NotEqual,
  LessThan,
  LessThanOrEqual,
  GreaterThan,
  GreaterThanOrEqual,
}

impl<'src> ExprKind<'src> {
  pub fn app(callee: Expr<'src>, arg: Expr<'src>) -> Self {
    Self::App {
      callee: Box::new(callee),
      arg: Box::new(arg),
    }
  }

  pub fn let_(name: Ident<'src>, value: Expr<'src>, in_: Expr<'src>) -> Self {
    Self::Let {
      name,
      value: Box::new(value),
      in_: Box::new(in_),
    }
  }

  pub fn fn_(param: Ident<'src>, body: Expr<'src>) -> Self {
    Self::Fn {
      param,
      body: Box::new(body),
    }
  }

  pub fn if_(cond: Expr<'src>, then: Expr<'src>, else_: Expr<'src>) -> Self {
    Self::If {
      cond: Box::new(cond),
      then: Box::new(then),
      else_: Box::new(else_),
    }
  }

  pub fn binary(lhs: Expr<'src>, op: BinaryOp, rhs: Expr<'src>) -> Self {
    Self::Binary {
      lhs: Box::new(lhs),
      op,
      rhs: Box::new(rhs),
    }
  }
}

pub type Expr<'src> = Spanned<ExprKind<'src>>;

pub struct Spanned<T> {
  pub span: Span,
  inner: T,
}

impl<T> Spanned<T> {
  pub fn new(span: impl Into<Span>, inner: T) -> Self {
    Self {
      span: span.into(),
      inner,
    }
  }

  pub fn into_inner(self) -> T {
    self.inner
  }
}

impl<T: Debug> Debug for Spanned<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Debug::fmt(&self.inner, f)
  }
}

impl<T: Display> Display for Spanned<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Display::fmt(&self.inner, f)
  }
}

impl<T> Deref for Spanned<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.inner
  }
}

impl<T> DerefMut for Spanned<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.inner
  }
}

pub type Ident<'src> = Spanned<&'src str>;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
  pub start: u32,
  pub end: u32,
}

impl Span {
  #[inline]
  pub fn start(self) -> usize {
    self.start as usize
  }

  #[inline]
  pub fn end(self) -> usize {
    self.end as usize
  }

  #[inline]
  pub const fn empty() -> Span {
    Span { start: 0, end: 0 }
  }

  #[inline]
  pub fn is_empty(self) -> bool {
    self.start == self.end
  }

  #[inline]
  pub fn to(self, other: Span) -> Span {
    Span {
      start: self.start,
      end: other.end,
    }
  }
}

impl From<Range<usize>> for Span {
  #[inline]
  fn from(value: Range<usize>) -> Self {
    Span {
      start: value.start as u32,
      end: value.end as u32,
    }
  }
}

impl From<Range<u32>> for Span {
  #[inline]
  fn from(value: Range<u32>) -> Self {
    Span {
      start: value.start,
      end: value.end,
    }
  }
}

impl From<Span> for Range<usize> {
  #[inline]
  fn from(value: Span) -> Self {
    value.start as usize..value.end as usize
  }
}

impl<T: Index<Range<usize>>> Index<Span> for [T] {
  type Output = <[T] as Index<Range<usize>>>::Output;

  #[inline]
  fn index(&self, index: Span) -> &Self::Output {
    self.index(Range::from(index))
  }
}

impl Index<Span> for str {
  type Output = <str as Index<Range<usize>>>::Output;

  #[inline]
  fn index(&self, index: Span) -> &Self::Output {
    self.index(Range::from(index))
  }
}

impl Display for Span {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}..{}", self.start, self.end)
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
  pub kind: TokenKind,
  pub span: Span,
}

impl Token {
  #[inline]
  pub fn is(&self, kind: impl Borrow<TokenKind>) -> bool {
    use std::mem::discriminant;

    discriminant(&self.kind) == discriminant(kind.borrow())
  }
}

impl Display for BinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(match self {
      BinaryOp::Add => "+",
      BinaryOp::Subtract => "-",
      BinaryOp::Multiply => "*",
      BinaryOp::Divide => "/",
      BinaryOp::LessThan => "<",
      BinaryOp::LessThanOrEqual => "<=",
      BinaryOp::GreaterThan => ">",
      BinaryOp::GreaterThanOrEqual => ">=",
      BinaryOp::Equal => "==",
      BinaryOp::NotEqual => "!=",
    })
  }
}
