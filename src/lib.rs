#![feature(loop_break_value)]
#![feature(collections_range)]
#![allow(extra_requirement_in_impl)]

mod input;
mod result;
pub mod parser;
pub mod char_class;

pub use input::Input;
pub use result::{Result, Error};
