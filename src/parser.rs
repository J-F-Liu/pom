use super::{Result, Error};

/// Parser trait.
pub trait Parser<'a, I, O> {
	/// Parse input at start position, return output and finished position.
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)>;
}

impl<'a, I: 'a, O, F> Parser<'a, I, O> for F where F: Fn(&'a [I], usize) -> Result<(O, usize)> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self(input, start)
	}
}

// impl<'a> Parser<'a, u8, &'a [u8]> for &'a str {
// 	fn parse(&self, input: &'a [u8], start: usize) -> Result<(&'a [u8], usize)> {
// 		seq(self.as_bytes()).parse(input, start)
// 	}
// }

impl<'a> Parser<'a, char, &'a str> for &'a str {
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

// impl<'a, I: Copy + PartialEq + Debug + 'a> Parser<'a, I, &'a [I]> for &'a [I] {
// 	fn parse(&self, input: &'a [I], start: usize) -> Result<(&'a [I], usize)> {
// 		seq(self).parse(input, start)
// 	}
// }

#[cfg(test)]
mod tests {
	use ::parser::*;

	// #[test]
	// fn byte_works() {
	// 	let input = b"abcde";
	// 	let parser = (b'a', one_of(b"ab"), sym(b'C'));
	// 	let output = parser.parse(input, 0);
	// 	assert_eq!(output, Err(Error::Mismatch{message: "expect: 67, found: 99".to_string(), position: 2}));

	// 	let parser = (b'a', none_of(b"AB"), b'c', b"de");
	// 	let output = parser.parse(input, 0);
	// 	assert_eq!(output, Ok( ((b'a', b'b', b'c', &b"de"[..]), 5)) );
	// 	assert_eq!(end().parse(input, 5), Ok( ((), 5) ) );

	// 	let parser = [b'e', b'd', b'a'];
	// 	let output = parser.parse(input, 0);
	// 	assert_eq!(output, Ok((b'a', 1)));
	// }

	// #[test]
	// fn char_works() {
	// 	let input = "abcd".chars().collect::<Vec<char>>();
	// 	let parser = ("ab", map(['c', 'd'], |_| '0'));
	// 	let output = parser.parse(&input, 0);
	// 	assert_eq!(output, Ok((("ab", '0'), 3)));
	// }

	// #[test]
	// fn recursive_parser() {
	// 	#[derive(Debug, PartialEq)]
	// 	enum Expr{
	// 		Empty,
	// 		Group(Box<Expr>)
	// 	}
	// 	fn expr<'a>(input: &'a [u8], start: usize) -> Result<(Expr, usize)> {
	// 		Alt(
	// 			map((b'(', expr, b')'), |(_, e, _)|Expr::Group(Box::new(e))),
	// 			map(empty(), |_|Expr::Empty)
	// 		).parse(input, start)
	// 	}
	// 	let input = b"(())";
	// 	let parser = expr;
	// 	let output = parser.parse(input, 0);
	// 	assert_eq!(output, Ok( (Expr::Group(Box::new(Expr::Group(Box::new(Expr::Empty)))), 4)) );
	// }

// 	#[test]
// 	fn chain_parser() {
// 		let mut input = DataInput::new(b"5oooooooo");
// 		// let parser = one_of(b"0123456789").map(|c|c - b'0') >> |n| take(n as usize) + sym(b'o').repeat(0..);
// 		let parser = skip(1) * take(3) >> |v:Vec<u8>|{
// 			take(v.len()+2).map(move |u|{
// 				(u, v.clone())
// 			})
// 		};
// 		// To avoid clone v:
// 		// let parser = Parser::new(move |input| {
// 		// 	(skip(1) * take(3)).parse(input).and_then(|v:Vec<u8>| {
// 		// 			take(v.len()+2).parse(input).map(|u|{
// 		// 				(u, v)
// 		// 			})
// 		// 	})
// 		// });
// 		let output = parser.parse(&mut input);
// 		assert_eq!(output, Ok( (vec![b'o';5], vec![b'o';3]) ));
// 	}

// 	#[test]
// 	fn repeat_at_least() {
// 		let input = DataInput::new(b"xxxooo");

// 		{
// 			let parser = sym(b'x').repeat(1..2);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 1]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(1..);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 3]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(0..);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 3]))
// 		}

// 		{
// 			let parser = sym(b'y').repeat(0..);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![]))
// 		}

// 		{
// 			let parser = sym(b'y').repeat(1..);
// 			let output = parser.parse(&mut input.clone());
// 			assert!(output.is_err());
// 		}

// 		{
// 			let parser = sym(b'x').repeat(10..);
// 			let output = parser.parse(&mut input.clone());
// 			assert!(output.is_err());
// 		}
// 	}

// 	#[test]
// 	fn repeat_up_to() {
// 		let input = DataInput::new(b"xxxooo");

// 		{
// 			let parser = sym(b'x').repeat(..2);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 1]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(..4);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 3]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(..);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 3]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(..0);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(..10);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 3]))
// 		}
// 	}


// 	#[test]
// 	fn repeat_exactly() {
// 		let input = DataInput::new(b"xxxooo");

// 		{
// 			let parser = sym(b'x').repeat(0);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(1);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x';1]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(2);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 2]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(3);
// 			let output = parser.parse(&mut input.clone());
// 			assert_eq!(output, Ok(vec![b'x'; 3]))
// 		}

// 		{
// 			let parser = sym(b'x').repeat(4);
// 			let output = parser.parse(&mut input.clone());
// 			assert!(output.is_err())
// 		}
// 	}
}
