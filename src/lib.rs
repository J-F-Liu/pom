#![feature(loop_break_value)]
#![feature(collections_range)]
#![feature(collections_bound)]

mod input;
mod result;
mod train;

/// Constains predefined parsers and combinators.
pub mod parser;

/// Utility functions to get char class of byte value.
pub mod char_class;

pub use input::{Input, DataInput, TextInput};
pub use result::{Result, Error};
pub use train::Train;

/// Parser type, `Parser<I, O>` is alias of `parser::Parser<'static, I, O>`.
pub type Parser<I, O> = parser::Parser<'static, I, O>;
