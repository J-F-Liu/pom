/// Parser error.
#[derive(Debug, PartialEq)]
pub enum Error {
	Incomplete,
	Mismatch { message: String, position: usize },
	Conversion { message: String, position: usize },
	Custom { message: String, position: usize, inner: Option<Box<Error>> },
}

/// Parser result, `Result<O>` ia alias of `Result<O, pom::Error>`.
pub type Result<O> = ::std::result::Result<O, Error>;
