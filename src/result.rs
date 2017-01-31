use std::fmt::{self, Display};
use std::error;

/// Parser error.
#[derive(Debug, PartialEq)]
pub enum Error {
	Incomplete,
	Mismatch { message: String, position: usize },
	Conversion { message: String, position: usize },
	Custom { message: String, position: usize, inner: Option<Box<Error>> },
}

impl error::Error for Error {
	fn description(&self) -> &'static str {
		"Parse error"
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			&Error::Incomplete =>
				write!(f, "Incomplete"),
			&Error::Mismatch { ref message, ref position } =>
				write!(f, "Mismatch at {}: {}", position, message),
			&Error::Conversion { ref message, ref position } =>
				write!(f, "Conversion failed at {}: {}", position, message),
			&Error::Custom { ref message, ref position, inner: Some(ref inner) } =>
				write!(f, "Custom error at {}: {} (inner: {})", position, message, inner),
			&Error::Custom { ref message, ref position, inner: None } =>
				write!(f, "Custom error at {}: {}", position, message),
		}
	}
}

/// Parser result, `Result<O>` ia alias of `Result<O, pom::Error>`.
pub type Result<O> = ::std::result::Result<O, Error>;
