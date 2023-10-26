mod error;
mod syn;
mod ty;

use crate::error::Result;
use reedline::SearchQuery;
use reedline::ValidationResult;
use reedline::Validator;
use reedline::{Prompt, Reedline, Signal};
use std::borrow::Cow;

struct Repl;

impl Prompt for Repl {
  fn render_prompt_left(&self) -> Cow<str> {
    Cow::Borrowed("")
  }

  fn render_prompt_right(&self) -> Cow<str> {
    Cow::Borrowed("")
  }

  fn render_prompt_indicator(&self, _: reedline::PromptEditMode) -> Cow<str> {
    Cow::Borrowed("> ")
  }

  fn render_prompt_multiline_indicator(&self) -> Cow<str> {
    Cow::Borrowed("> ")
  }

  fn render_prompt_history_search_indicator(&self, _: reedline::PromptHistorySearch) -> Cow<str> {
    Cow::Borrowed("? ")
  }
}

fn line(buf: &str) -> Result<()> {
  let expr = syn::parse(buf)?;
  // println!("{expr:?}");
  let expr = ty::infer(expr)?;
  println!("type:\n  {}", expr.ty);
  Ok(())
}

struct Newline;
impl Validator for Newline {
  fn validate(&self, line: &str) -> ValidationResult {
    if line.trim() == "exit" || line.trim_end().ends_with(';') {
      ValidationResult::Complete
    } else {
      ValidationResult::Incomplete
    }
  }
}

fn main() {
  println!(
    "picoML v{}\
    \nCTRL-C or type \"exit\" to quit\
    \nCTRL-D to cancel the current input",
    env!("CARGO_PKG_VERSION")
  );
  let mut line_editor = Reedline::create().with_validator(Box::new(Newline));
  loop {
    let sig = line_editor.read_line(&Repl);
    match sig {
      Ok(Signal::CtrlC) => break,
      Ok(Signal::CtrlD) => continue,
      Ok(Signal::Success(buf)) => {
        if buf.trim() == "exit" {
          break;
        }
        let buf = buf.trim_end();
        let buf = buf.strip_suffix(';').unwrap_or(buf);
        if let Err(e) = line(buf) {
          println!("{e}");
        }
      }
      _ => {}
    }
  }
}
