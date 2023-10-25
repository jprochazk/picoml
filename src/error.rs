use std::borrow::Cow;
use std::fmt::Display;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
pub struct Error {
  msg: Cow<'static, str>,
}

impl Error {
  pub fn new(msg: impl Into<Cow<'static, str>>) -> Self {
    Error { msg: msg.into() }
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "error: {}", self.msg)
  }
}

impl std::error::Error for Error {}
