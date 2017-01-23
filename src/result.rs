/// Parser error.
#[derive(Debug, PartialEq)]
pub enum Error {
	Incomplete,
	Mismatch { message: String, position: usize },
	Conversion { message: String, position: usize },
}

/// Parser result, `Result<O>` ia alias of `Result<O, pom::Error>`.
pub type Result<O> = ::std::result::Result<O, Error>;
