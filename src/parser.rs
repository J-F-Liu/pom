use std::fmt::{Display, Debug};
use std::ops::{Add, Sub, BitOr, Neg, Not};
use std::collections::range::RangeArgument;
use super::{Result, Error, Input};

/// Parser combinator.
pub struct Parser<I, O> {
	method: Box<Fn(&mut Input<I>) -> Result<O>>,
}

impl<I, O> Parser<I, O> {
	pub fn new<P>(parse: P) -> Parser<I, O>
		where P: Fn(&mut Input<I>) -> Result<O> + 'static
	{
		Parser { method: Box::new(parse) }
	}

	pub fn parse(&self, input: &mut Input<I>) -> Result<O> {
		(self.method)(input)
	}

	pub fn map<U, F>(self, f: F) -> Parser<I, U>
		where F: Fn(O) -> U + 'static,
			  I: 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| self.parse(input).map(&f))
	}

	pub fn opt(self) -> Parser<I, Option<O>>
		where I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			match self.parse(input) {
				Ok(out) => Ok(Some(out)),
				Err(_) => Ok(None),
			}
		})
	}

	pub fn repeat<R>(self, range: R) -> Parser<I, Vec<O>>
		where R: RangeArgument<usize> + Debug + 'static,
			  I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start_pos = input.position;
			let mut items = vec![];
			while let Ok(item) = self.parse(input) {
				items.push(item);
				if let Some(&end) = range.end() {
					if items.len() >= end { break; }
				}
			}
			if let Some(&start) = range.start() {
				if items.len() < start {
					input.position = start_pos;
					return Err(Error::Mismatch {
						message: format!("expect repeat at least {} times, found {} times", start, items.len()),
						position: input.position,
					});
				}
			}
			return Ok(items);
		})
	}
}

/// Always success, consume no input.
pub fn empty<I>() -> Parser<I, ()> {
	Parser::new(|_: &mut Input<I>| Ok(()))
}

/// Sucess when current input symbol equals t.
pub fn term<I>(t: I) -> Parser<I, I>
	where I: Copy + PartialEq + Display + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if t == s {
				input.advance();
				Ok(t)
			} else {
				Err(Error::Mismatch {
					message: format!("expect: {}, found: {}", t, s),
					position: input.position,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Sucess when sequence of symbols match current input.
pub fn seq<I>(tag: &'static [I]) -> Parser<I, &[I]>
	where I: Copy + PartialEq + Display + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		let start = input.position;
		let mut index = 0;
		let result = loop {
			if index == tag.len() {
				break Ok(tag);
			}
			if let Some(s) = input.current() {
				if tag[index] == s {
					input.advance();
				} else {
					break Err(Error::Mismatch {
						message: format!("seq expect: {}, found: {}", tag[index], s),
						position: input.position,
					});
				}
			} else {
				break Err(Error::Incomplete);
			}
			index += 1;
		};
		if result.is_err() {
			input.position = start;
		}
		result
	})
}

/// Sucess when current input symbol is one of the set.
pub fn one_of<I>(set: &'static [I]) -> Parser<I, I>
	where I: Copy + PartialEq + Display + Debug + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if set.contains(&s) {
				input.advance();
				Ok(s)
			} else {
				Err(Error::Mismatch {
					message: format!("expect one of: {:?}, found: {}", set, s),
					position: input.position,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Sucess when current input symbol is none of the set.
pub fn none_of<I>(set: &'static [I]) -> Parser<I, I>
	where I: Copy + PartialEq + Display + Debug + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if set.contains(&s) {
				Err(Error::Mismatch {
					message: format!("expect none of: {:?}, found: {}", set, s),
					position: input.position,
				})
			} else {
				input.advance();
				Ok(s)
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}


/// Sucess when predict return true on current input symbol.
pub fn is_a<I, F>(predict: F) -> Parser<I, I>
	where I: Copy + PartialEq + Display + Debug + 'static,
		  F: Fn(I) -> bool + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if predict(s) {
				input.advance();
				Ok(s)
			} else {
				Err(Error::Mismatch {
					message: format!("is_a predict failed on: {}", s),
					position: input.position,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Sucess when predict return false on current input symbol.
pub fn not_a<I, F>(predict: F) -> Parser<I, I>
	where I: Copy + PartialEq + Display + Debug + 'static,
		  F: Fn(I) -> bool + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if predict(s) {
				Err(Error::Mismatch {
					message: format!("not_a predict failed on: {}", s),
					position: input.position,
				})
			} else {
				input.advance();
				Ok(s)
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Sucess when the range contains current input symbol.
pub fn range<I, R>(set: R) -> Parser<I, I>
	where I: Copy + PartialOrd<I> + Display + Debug + 'static,
		  R: RangeArgument<I> + Debug + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if let Some(&start) = set.start() {
				if s < start {
					return Err(Error::Mismatch {
						message: format!("expect range: {:?}, found: {}", set, s),
						position: input.position,
					});
				}
			}
			if let Some(&end) = set.end() {
				if s >= end {
					return Err(Error::Mismatch {
						message: format!("expect range: {:?}, found: {}", set, s),
						position: input.position,
					});
				}
			}
			input.advance();
			return Ok(s);
		} else {
			return Err(Error::Incomplete);
		}
	})
}

/// Success when end of file is reached.
pub fn eof<I>() -> Parser<I, ()>
	where I: Copy + Display + 'static
{
	Parser::new(|input: &mut Input<I>| {
		if let Some(s) = input.current() {
			Err(Error::Mismatch{
				message: format!("expect end of file, found: {}", s),
				position: input.position,
			})
		} else {
			Ok(())
		}
	})
}

/// Sequence reserve value
impl<I, O, U> Add<Parser<I, U>> for Parser<I, O> {
	type Output = Parser<I, (O, U)>;

	fn add(self, other: Parser<I, U>) -> Self::Output
		where I: 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position;
			let result = self.parse(input).and_then(|out1| other.parse(input).map(|out2| (out1, out2)));
			if result.is_err() {
				input.position = start;
			}
			result
		})
	}
}

/// Sequence discard value
impl<I, O, U> Sub<Parser<I, U>> for Parser<I, O> {
	type Output = Parser<I, O>;

	fn sub(self, other: Parser<I, U>) -> Self::Output
		where I: 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position;
			let result = self.parse(input).and_then(|out1| other.parse(input).map(|_| out1));
			if result.is_err() {
				input.position = start;
			}
			result
		})
	}
}

/// Ordered choice
impl<I, O> BitOr for Parser<I, O> {
	type Output = Parser<I, O>;

	fn bitor(self, other: Parser<I, O>) -> Self::Output
		where I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			self.parse(input).or_else(|_| other.parse(input))
		})
	}
}

/// And predicate
impl<I, O> Neg for Parser<I, O> {
	type Output = Parser<I, bool>;

	fn neg(self) -> Self::Output
		where I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position;
			let result = self.parse(input);
			input.position = start;
			result.map(|_| true)
		})
	}
}

/// Not predicate
impl<I, O> Not for Parser<I, O> {
	type Output = Parser<I, bool>;

	fn not(self) -> Self::Output
		where I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position;
			let result = self.parse(input);
			input.position = start;
			match result {
				Ok(_) => Err(Error::Mismatch{
					message: "not predicate failed".to_string(),
					position: input.position,
				}),
				Err(_) => Ok(true),
			}
		})
	}
}

#[test]
fn byte_works() {
	let mut input = Input::new(b"abcde");
	let parser = term(b'a') + one_of(b"ab") - term(b'C');
	let output = parser.parse(&mut input);
	assert_eq!(input.position, 0);

	let parser = term(b'a') + none_of(b"AB") - term(b'c') + seq(b"de");
	let output = parser.parse(&mut input);
	assert_eq!(output, Ok( ((b'a', b'b'), &b"de"[..]) ) );

	let parser = term(b'e') | term(b'd') | empty().map(|_| b'0');
	let output = parser.parse(&mut input);
	assert_eq!(output, Ok(b'0'));
}

#[test]
fn char_works() {
	let chars = "abc".chars().collect::<Vec<char>>();
	let mut input = Input::new(chars.as_slice());
	let parser = term('a') + term('b') - term('c') |
				 term('d').map(|_| ('0', '0'));
	let output = parser.parse(&mut input);
	assert_eq!(output, Ok(('a', 'b')));
}
