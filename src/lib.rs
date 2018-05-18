#![feature(range_contains)]

mod result;
pub mod range;
pub mod set;

/// Constains predefined parsers and combinators.
pub mod parser;
pub mod combinator;

/// Utility functions to recognize char class of byte value.
pub mod char_class;

pub use result::{Result, Error};
pub use parser::Parser;

// The following code is not accepted by today's compiler.
// pub type Parser<'a, I, O> = combinator::Combinator<impl parser::Parser<'a, I, Output=O>>;
// pub type Parser<I, O> = for<'a> parser::Parser<'a, I, O>;
