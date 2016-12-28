/// Parser error.
#[derive(Debug, PartialEq)]
pub enum Error {
	Incomplete,
	Mismatch { message: String, position: usize },
}

/// Parser result.
pub type Result<O> = ::std::result::Result<O, Error>;
