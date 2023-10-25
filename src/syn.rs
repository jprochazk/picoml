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
    self.current.is(t![eof])
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
      Some((Ok(kind), span)) => Ok(Token {
        kind,
        span: span.into(),
      }),
      Some((Err(_), span)) => Err(Error::new(format!(
        "unexpected token `{}`",
        &self.src[span]
      ))),
      None => Ok(EOF),
    };
    self.previous = std::mem::replace(&mut self.current, token?);
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

pub fn parse(src: &str) -> Result<S<Expr>> {
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

fn expr(p: &mut Parser<'_>) -> Result<S<Expr>> {
  let left = match p.kind() {
    t![let] => let_(p)?,
    t![fn] => fn_(p)?,
    t![if] => if_(p)?,
    t![bool] => bool_(p)?,
    t![int] => int_(p)?,
    t![ident] => var_(p)?,
    t!["("] => group_(p)?,
    _ => {
      return Err(Error::new(format!(
        "unexpected token `{}`",
        &p.src[p.current().span]
      )))
    }
  };

  if p.can_begin_expr() {
    let right = expr(p)?;
    Ok(Meta::new(
      left.span.to(right.span),
      (),
      Expr::app(left, right),
    ))
  } else {
    Ok(left)
  }
}

fn let_(p: &mut Parser<'_>) -> Result<S<Expr>> {
  spanned(p, |p| {
    p.must(t![let])?;
    let name = ident(p)?;
    p.must(t![=])?;
    let value = expr(p)?;
    p.must(t![in])?;
    let in_ = expr(p)?;

    Ok(Expr::let_(name, value, in_))
  })
}

fn fn_(p: &mut Parser<'_>) -> Result<S<Expr>> {
  spanned(p, |p| {
    p.must(t![fn])?;
    let param = ident(p)?;
    p.must(t![->])?;
    let body = expr(p)?;

    Ok(Expr::fn_(param, body))
  })
}

fn if_(p: &mut Parser<'_>) -> Result<S<Expr>> {
  spanned(p, |p| {
    p.must(t![if])?;
    let cond = expr(p)?;
    p.must(t![then])?;
    let then = expr(p)?;
    p.must(t![else])?;
    let else_ = expr(p)?;

    Ok(Expr::if_(cond, then, else_))
  })
}

fn bool_(p: &mut Parser<'_>) -> Result<S<Expr>> {
  spanned(p, |p| {
    p.must(t![bool])?;
    let v = p
      .lexeme(p.previous())
      .parse()
      .map_err(|e| Error::new(format!("failed to parse bool: {e}")))?;
    Ok(Expr::bool(v))
  })
}

fn int_(p: &mut Parser<'_>) -> Result<S<Expr>> {
  spanned(p, |p| {
    p.must(t![int])?;
    let v = p
      .lexeme(p.previous())
      .parse()
      .map_err(|e| Error::new(format!("failed to parse bool: {e}")))?;
    Ok(Expr::int(v))
  })
}

fn var_(p: &mut Parser<'_>) -> Result<S<Expr>> {
  spanned(p, |p| Ok(Expr::var(ident(p)?)))
}

fn group_(p: &mut Parser<'_>) -> Result<S<Expr>> {
  p.must(t!["("])?;
  let inner = expr(p)?;
  p.must(t![")"])?;
  Ok(inner)
}

fn ident(p: &mut Parser<'_>) -> Result<Ident> {
  p.must(t![ident])?;
  Ok(Spanned::new(
    p.previous().span,
    (),
    p.lexeme(p.previous()).to_string(),
  ))
}

#[inline]
fn spanned<T>(
  p: &mut Parser<'_>,
  f: impl FnOnce(&mut Parser<'_>) -> Result<T>,
) -> Result<Spanned<T>> {
  let start = p.current().span.start;
  let inner = f(p);
  let end = p.previous().span.end;
  match inner {
    Ok(inner) => Ok(Spanned::new(start..end, (), inner)),
    Err(e) => Err(e),
  }
}

#[derive(Debug, Clone, Copy, Logos, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(skip r"[ \t\n\r]+")]
pub enum TokenKind {
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

pub struct Meta<T, Extra> {
  pub span: Span,
  pub extra: Extra,
  inner: T,
}

pub type Spanned<T> = Meta<T, ()>;
pub use Spanned as S;

impl<T, Extra> Meta<T, Extra> {
  pub fn new(span: impl Into<Span>, extra: Extra, inner: T) -> Self {
    Self {
      span: span.into(),
      extra,
      inner,
    }
  }
}

impl<T: Debug, Extra> Debug for Meta<T, Extra> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Debug::fmt(&self.inner, f)
  }
}

impl<T: Display, Extra> Display for Meta<T, Extra> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Display::fmt(&self.inner, f)
  }
}

impl<T, Extra> Deref for Meta<T, Extra> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.inner
  }
}

impl<T, Extra> DerefMut for Meta<T, Extra> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.inner
  }
}

#[derive(Debug)]
pub enum Expr {
  App {
    callee: Box<S<Expr>>,
    arg: Box<S<Expr>>,
  },
  Let {
    name: Ident,
    value: Box<S<Expr>>,
    in_: Box<S<Expr>>,
  },
  Fn {
    param: Ident,
    body: Box<S<Expr>>,
  },
  If {
    cond: Box<S<Expr>>,
    then: Box<S<Expr>>,
    else_: Box<S<Expr>>,
  },
  Bool(bool),
  Int(i64),
  Var(Ident),
}

impl Expr {
  pub fn app(callee: S<Expr>, arg: S<Expr>) -> Self {
    Self::App {
      callee: Box::new(callee),
      arg: Box::new(arg),
    }
  }

  pub fn let_(name: Ident, value: S<Expr>, in_: S<Expr>) -> Self {
    Self::Let {
      name,
      value: Box::new(value),
      in_: Box::new(in_),
    }
  }

  pub fn fn_(param: Ident, body: S<Expr>) -> Self {
    Self::Fn {
      param,
      body: Box::new(body),
    }
  }

  pub fn if_(cond: S<Expr>, then: S<Expr>, else_: S<Expr>) -> Self {
    Self::If {
      cond: Box::new(cond),
      then: Box::new(then),
      else_: Box::new(else_),
    }
  }

  pub fn bool(v: bool) -> Self {
    Self::Bool(v)
  }

  pub fn int(v: i64) -> Self {
    Self::Int(v)
  }

  pub fn var(v: Ident) -> Self {
    Self::Var(v)
  }
}

pub type Ident = Spanned<String>;

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
