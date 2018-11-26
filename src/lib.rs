mod result;
pub mod range;
pub mod set;

/// Constains predefined parsers and combinators.
pub mod parser;

/// Utility functions to recognize char class of byte value.
pub mod char_class;

pub use crate::result::{Result, Error};

/// Parser type, `Parser<I, O>` is alias of `parser::Parser<'static, I, O>`.
pub type Parser<I, O> = parser::Parser<'static, I, O>;
