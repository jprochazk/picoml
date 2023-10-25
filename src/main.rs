mod error;
mod syn;
mod ty;

use crate::error::Result;
use reedline::default_emacs_keybindings;
use reedline::EditCommand;
use reedline::Emacs;
use reedline::KeyCode;
use reedline::KeyModifiers;
use reedline::ReedlineEvent;
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
  let ty = ty::infer(expr)?;
  println!("{ty}");
  Ok(())
}

fn main() {
  let mut keybindings = default_emacs_keybindings();
  keybindings.add_binding(
    KeyModifiers::CONTROL,
    KeyCode::Char('n'),
    ReedlineEvent::Edit(vec![EditCommand::InsertNewline]),
  );
  let mut line_editor = Reedline::create().with_edit_mode(Box::new(Emacs::new(keybindings)));

  loop {
    let sig = line_editor.read_line(&Repl);
    match sig {
      Ok(Signal::Success(buf)) => {
        if let Err(e) = line(&buf) {
          println!("{e}");
        }
      }
      Ok(Signal::CtrlC | Signal::CtrlD) => break,
      _ => {}
    }
  }
}
