pub(crate) mod range;
mod result;
pub(crate) mod set;

/// Contains predefined parsers and combinators.
pub mod parser;

/// Utility functions to recognize char class of byte value.
pub mod char_class;

/// Variants of parser functions specialized for matching UTF-8 strings and returning chars.
/// Method and constructor names/functionality are generally the same as in base parser module.
#[cfg(feature = "utf8")]
pub mod utf8;

pub use crate::result::{Error, Result};

/// Parser type, `Parser<I, O>` is alias of `parser::Parser<'static, I, O>`.
pub type Parser<I, O> = parser::Parser<'static, I, O>;
