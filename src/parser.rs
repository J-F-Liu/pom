use super::{Result, Error};

/// Parser trait.
pub trait Parser<'a, I> {
	type Output;
	/// Parse input at start position, return output and finished position.
	fn parse(&self, input: &'a [I], start: usize) -> Result<(Self::Output, usize)>;
}

impl<'a, I: 'a, O, F> Parser<'a, I> for F where F: Fn(&'a [I], usize) -> Result<(O, usize)> {
	type Output = O;

	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self(input, start)
	}
}

// impl<'a, I: 'a, O, F> Parser<'a, I> for Box<F> where F: Fn(&'a [I], usize) -> Result<(O, usize)> {
// 	type Output = O;

// 	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
// 		self(input, start)
// 	}
// }

// impl<'a> Parser<'a, u8, &'a [u8]> for &'a str {
// 	fn parse(&self, input: &'a [u8], start: usize) -> Result<(&'a [u8], usize)> {
// 		seq(self.as_bytes()).parse(input, start)
// 	}
// }

impl<'a> Parser<'a, char> for &'a str {
	type Output = &'a str;

	fn parse(&self, input: &'a [char], start: usize) -> Result<(&'a str, usize)> {
		let mut pos = start;
		for c in self.chars() {
			if let Some(&s) = input.get(pos) {
				if c != s {
					return Err(Error::Mismatch {
						message: format!("seq {:?} expect: {:?}, found: {:?}", self, c, s),
						position: pos,
					});
				}
			} else {
				return Err(Error::Incomplete);
			}
			pos += 1;
		}
		return Ok((self, pos));
	}
}
