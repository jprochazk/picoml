macro_rules! t {
  [bool] => [$crate::syn::TokenKind::Bool];
  [let] => [$crate::syn::TokenKind::Let];
  [=] => [$crate::syn::TokenKind::Eq];
  [in] => [$crate::syn::TokenKind::In];
  [fn] => [$crate::syn::TokenKind::Fn];
  [->] => [$crate::syn::TokenKind::Arrow];
  [if] => [$crate::syn::TokenKind::If];
  [then] => [$crate::syn::TokenKind::Then];
  [else] => [$crate::syn::TokenKind::Else];
  [int] => [$crate::syn::TokenKind::Int];
  [ident] => [$crate::syn::TokenKind::Ident];
  ["("] => [$crate::syn::TokenKind::GroupL];
  [")"] => [$crate::syn::TokenKind::GroupR];
  [eof] => [$crate::syn::TokenKind::Eof];
}
