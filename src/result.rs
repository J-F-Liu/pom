use std::error;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RollbackType {
	Fail = 0,
	Stop,
}

// Change to tree to allow for readable forks
#[derive(Debug, PartialEq, Clone)]
pub struct RollbackRecord {
	pub name: String,
	pub position: usize,
	pub typ: RollbackType,
	pub inner: Option<Box<Error>>,
	pub previous_tracks: Vec<RollbackRecord>,
}

impl RollbackRecord {
	fn fmt_with_indent(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
		let idnt = "  ".repeat(indent);
		match &self.previous_tracks[..] {
			[] => {}
			[prev] => prev.fmt_with_indent(f, indent)?,
			_ => {
				writeln!(f, "{}Fork: {{", idnt)?;
				for prev in &self.previous_tracks {
					writeln!(f, "{}>><<", idnt)?;
					prev.fmt_with_indent(f, indent + 1)?;
				}
				writeln!(f, "{}}}", idnt)?;
			}
		}
		match &self.inner {
			Some(ref inner) => writeln!(
				f,
				"{}{} failed at {}, (inner: {})",
				idnt, &self.name, &self.position, inner
			)?,
			None => writeln!(f, "{}{} failed at {}", idnt, &self.name, &self.position)?,
		}
		Ok(())
	}
}

impl Display for RollbackRecord {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.fmt_with_indent(f, 0)
	}
}

/// Parser error.
#[derive(Debug, PartialEq, Clone)]
pub enum Error {
	Incomplete,
	Mismatch {
		message: String,
		position: usize,
	},
	Conversion {
		message: String,
		position: usize,
	},
	Expect {
		message: String,
		position: usize,
		inner: Box<Error>,
	},
	Custom {
		message: String,
		position: usize,
		inner: Option<Box<Error>>,
	},
	Rollback {
		rollbacks: RollbackRecord,
	},
}

impl error::Error for Error {
	fn description(&self) -> &'static str {
		"Parse error"
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Error::Incomplete => write!(f, "Incomplete"),
			Error::Mismatch {
				ref message,
				ref position,
			} => write!(f, "Mismatch at {}: {}", position, message),
			Error::Conversion {
				ref message,
				ref position,
			} => write!(f, "Conversion failed at {}: {}", position, message),
			Error::Expect {
				ref message,
				ref position,
				ref inner,
			} => write!(f, "{} at {}: {}", message, position, inner),
			Error::Custom {
				ref message,
				ref position,
				inner: Some(ref inner),
			} => write!(f, "{} at {}, (inner: {})", message, position, inner),
			Error::Custom {
				ref message,
				ref position,
				inner: None,
			} => write!(f, "{} at {}", message, position),
			Error::Rollback { ref rollbacks } => {
				writeln!(f, "Rollback log:")?;
				write!(f, "{}", rollbacks)?;
				Ok(())
			}
		}
	}
}

/// Parser result, `Result<O>` ia alias of `Result<O, pom::Error>`.
pub type Result<O> = ::std::result::Result<O, Error>;
