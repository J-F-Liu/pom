mod input;
mod result;
mod train;
pub mod range;
pub mod set;

/// Constains predefined parsers and combinators.
pub mod parser;

/// Utility functions to recognize char class of byte value.
pub mod char_class;

pub use crate::input::{Input, DataInput, TextInput};
pub use crate::result::{Result, Error};
pub use crate::train::Train;

/// Parser type, `Parser<I, O>` is alias of `parser::Parser<'static, I, O>`.
pub type Parser<I, O> = parser::Parser<'static, I, O>;
