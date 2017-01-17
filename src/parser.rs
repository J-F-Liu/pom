use std::fmt::{Display, Debug};
use std::ops::{Add, Sub, Mul, Shr, BitOr, Neg, Not};
use std::collections::range::RangeArgument;
use std::collections::Bound::{Excluded, Included, Unbounded};
use super::{Result, Error, Input, Train};

/// Parser combinator.
pub struct Parser<I, O> {
	method: Box<Fn(&mut Input<I>) -> Result<O>>,
}

impl<I, O> Parser<I, O> {
	/// Create new parser.
	pub fn new<P>(parse: P) -> Parser<I, O>
		where P: Fn(&mut Input<I>) -> Result<O> + 'static
	{
		Parser { method: Box::new(parse) }
	}

	/// Apply the parser to parse input.
	pub fn parse(&self, input: &mut Input<I>) -> Result<O> {
		(self.method)(input)
	}

	/// Convert parser result to desired value.
	pub fn map<U, F>(self, f: F) -> Parser<I, U>
		where F: Fn(O) -> U + 'static,
			  I: 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| self.parse(input).map(&f))
	}

	/// Collect all matched input symbols.
	pub fn collect(self) -> Parser<I, Vec<I>>
		where I: Copy + 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			self.parse(input).map(|_|{
				let end = input.position();
				input.segment(start, end)
			})
		})
	}

	/// Discard parser output.
	pub fn discard(self) -> Parser<I, ()>
		where I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			self.parse(input).map(|_|())
		})
	}

	/// Make parser optional.
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

	/// `p.repeat(0..)` repeat p zero or more times
	/// `p.repeat(1..)` repeat p one or more times
	/// `p.repeat(1..4)` match p at least 1 and at most 3 times
	pub fn repeat<R>(self, range: R) -> Parser<I, Vec<O>>
		where R: RangeArgument<usize> + Debug + 'static,
			  I: Copy + 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start_pos = input.position();
			let mut items = vec![];
			while let Ok(item) = self.parse(input) {
				items.push(item);
				match range.end() {
					Included(&end) => if items.len() >= end { break; },
					Excluded(&end) => if items.len() >= end - 1 { break; },
					Unbounded => continue,
				}
			}
			if let Included(&start) = range.start() {
				if items.len() < start {
					input.jump_to(start_pos);
					return Err(Error::Mismatch {
						message: format!("expect repeat at least {} times, found {} times", start, items.len()),
						position: start_pos,
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
pub fn sym<I>(t: I) -> Parser<I, I>
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
					position: input.position(),
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Sucess when sequence of symbols match current input.
pub fn seq<I, T>(train: &'static T) -> Parser<I, Vec<I>>
	where I: Copy + PartialEq + Display + 'static,
		  T: Train<I> + ?Sized
{
	Parser::new(move |input: &mut Input<I>| {
		let tag = train.knots();
		let start = input.position();
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
						message: format!("seq {} expect: {}, found: {}", train.to_str(), tag[index], s),
						position: input.position(),
					});
				}
			} else {
				break Err(Error::Incomplete);
			}
			index += 1;
		};
		if result.is_err() {
			input.jump_to(start);
		}
		result
	})
}

/// Parse separated list.
pub fn list<I, O, U>(parser: Parser<I, O>, separator: Parser<I, U>) -> Parser<I, Vec<O>>
	where I: Copy + 'static,
		  O: 'static,
		  U: 'static
{
	Parser::new(move |input: &mut Input<I>| {
		let start = input.position();
		let mut items = vec![];
		if let Ok(first_item) = parser.parse(input) {
			items.push(first_item);
			while let Ok(_) = separator.parse(input) {
				match parser.parse(input) {
					Ok(more_item) => items.push(more_item),
					Err(error) => {
						input.jump_to(start);
						return Err(Error::Mismatch{
							message: format!("expect item after separator, found: {:?}", error),
							position: start,
						});
					}
				}
			}
		}
		return Ok(items);
	})
}

/// Sucess when current input symbol is one of the set.
pub fn one_of<I, T>(train: &'static T) -> Parser<I, I>
	where I: Copy + PartialEq + Display + Debug + 'static,
		  T: Train<I> + ?Sized
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			let set = train.knots();
			if set.contains(&s) {
				input.advance();
				Ok(s)
			} else {
				Err(Error::Mismatch {
					message: format!("expect one of: {}, found: {}", train.to_str(), s),
					position: input.position(),
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Sucess when current input symbol is none of the set.
pub fn none_of<I, T>(train: &'static T) -> Parser<I, I>
	where I: Copy + PartialEq + Display + Debug + 'static,
		  T: Train<I> + ?Sized
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			let set = train.knots();
			if set.contains(&s) {
				Err(Error::Mismatch {
					message: format!("expect none of: {}, found: {}", train.to_str(), s),
					position: input.position(),
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
					position: input.position(),
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
					position: input.position(),
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
			let meet_start = match set.start() {
				Included(&start) => s >= start,
				Excluded(&start) => s > start,
				Unbounded => true,
			};
			if !meet_start {
				return Err(Error::Mismatch {
					message: format!("expect range: {:?}, found: {}", set, s),
					position: input.position(),
				});
			}
			let meet_end = match set.end() {
				Included(&end) => s <= end,
				Excluded(&end) => s < end,
				Unbounded => true,
			};
			if !meet_end {
				return Err(Error::Mismatch {
					message: format!("expect range: {:?}, found: {}", set, s),
					position: input.position(),
				});
			}
			input.advance();
			return Ok(s);
		} else {
			return Err(Error::Incomplete);
		}
	})
}

/// Read n symbols.
pub fn take<I>(n: usize) -> Parser<I, Vec<I>>
	where I: Copy + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		let start = input.position();
		let mut symbols = Vec::with_capacity(n);
		while let Some(symbol) = input.current() {
			input.advance();
			symbols.push(symbol);
			if symbols.len() == n {
				break;
			}
		}
		if symbols.len() < n {
			input.jump_to(start);
			Err(Error::Incomplete)
		} else {
			Ok(symbols)
		}
	})
}

/// Skip n symbols.
pub fn skip<I>(n: usize) -> Parser<I, ()>
	where I: Copy + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		let start = input.position();
		let mut count = 0;
		while let Some(_) = input.current() {
			input.advance();
			count += 1;
			if count == n {
				break;
			}
		}
		if count < n {
			input.jump_to(start);
			Err(Error::Incomplete)
		} else {
			Ok(())
		}
	})
}

/// Call a parser factory, can be used to create recursive parsers.
pub fn call<I, O, F>(parser_factory: F) -> Parser<I, O>
	where I: 'static,
		  O: 'static,
		  F: Fn() -> Parser<I, O> + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		let parser = parser_factory();
		parser.parse(input)
	})
}

/// Success when end of input is reached.
pub fn end<I>() -> Parser<I, ()>
	where I: Copy + Display + 'static
{
	Parser::new(|input: &mut Input<I>| {
		if let Some(s) = input.current() {
			Err(Error::Mismatch{
				message: format!("expect end of input, found: {}", s),
				position: input.position(),
			})
		} else {
			Ok(())
		}
	})
}

/// Sequence reserve value
impl<I: Copy, O, U> Add<Parser<I, U>> for Parser<I, O> {
	type Output = Parser<I, (O, U)>;

	fn add(self, other: Parser<I, U>) -> Self::Output
		where I: 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			let result = self.parse(input).and_then(|out1| other.parse(input).map(|out2| (out1, out2)));
			if result.is_err() {
				input.jump_to(start);
			}
			result
		})
	}
}

/// Sequence discard second value
impl<I: Copy, O, U> Sub<Parser<I, U>> for Parser<I, O> {
	type Output = Parser<I, O>;

	fn sub(self, other: Parser<I, U>) -> Self::Output
		where I: 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			let result = self.parse(input).and_then(|out1| other.parse(input).map(|_| out1));
			if result.is_err() {
				input.jump_to(start);
			}
			result
		})
	}
}

/// Sequence discard first value
impl<I: Copy, O, U> Mul<Parser<I, U>> for Parser<I, O> {
	type Output = Parser<I, U>;

	fn mul(self, other: Parser<I, U>) -> Self::Output
		where I: 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			let result = self.parse(input).and_then(|_| other.parse(input));
			if result.is_err() {
				input.jump_to(start);
			}
			result
		})
	}
}

/// Chain two passers where the second parser depends on the first's result.
impl<I: Copy, O, U, F: Fn(O) -> Parser<I, U>> Shr<F> for Parser<I, O> {
	type Output = Parser<I, U>;

	fn shr(self, other: F) -> Self::Output
		where I: 'static,
			  O: 'static,
			  U: 'static,
			  F: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			let result = self.parse(input).and_then(|out1| other(out1).parse(input));
			if result.is_err() {
				input.jump_to(start);
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
impl<I: Copy, O> Neg for Parser<I, O> {
	type Output = Parser<I, bool>;

	fn neg(self) -> Self::Output
		where I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			let result = self.parse(input);
			input.jump_to(start);
			result.map(|_| true)
		})
	}
}

/// Not predicate
impl<I: Copy, O> Not for Parser<I, O> {
	type Output = Parser<I, bool>;

	fn not(self) -> Self::Output
		where I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			let result = self.parse(input);
			input.jump_to(start);
			match result {
				Ok(_) => Err(Error::Mismatch{
					message: "not predicate failed".to_string(),
					position: start,
				}),
				Err(_) => Ok(true),
			}
		})
	}
}

#[cfg(test)]
mod tests {
	use ::parser::*;
	use ::{DataInput, TextInput};

	#[test]
	fn byte_works() {
		let mut input = DataInput::new(b"abcde");
		let parser = sym(b'a') + one_of(b"ab") - sym(b'C');
		let output = parser.parse(&mut input);
		assert_eq!(output, Err(Error::Mismatch{message: "expect: 67, found: 99".to_string(), position: 2}));
		assert_eq!(input.position(), 0);

		let parser = sym(b'a') * none_of(b"AB") - sym(b'c') + seq(b"de");
		let output = parser.parse(&mut input);
		assert_eq!(output, Ok( (b'b', vec![b'd', b'e']) ) );

		let parser = sym(b'e') | sym(b'd') | empty().map(|_| b'0');
		let output = parser.parse(&mut input);
		assert_eq!(output, Ok(b'0'));
	}

	#[test]
	fn char_works() {
		let mut input = TextInput::new("abcd");
		let parser = seq("ab") + sym('c') |
					sym('d').map(|_| (vec![], '0'));
		let output = parser.parse(&mut input);
		assert_eq!(output, Ok((vec!['a', 'b'], 'c')));
	}

	#[test]
	fn recursive_parser() {
		#[derive(Debug, PartialEq)]
		enum Expr{
			Empty,
			Group(Box<Expr>)
		}
		fn expr() -> Parser<u8, Expr> {
			(sym(b'(') + call(expr) - sym(b')')).map(|(_, e)|Expr::Group(Box::new(e)))
			| empty().map(|_|Expr::Empty)
		}
		let mut input = DataInput::new(b"(())");
		let parser = expr();
		let output = parser.parse(&mut input);
		assert_eq!(output, Ok(Expr::Group(Box::new(Expr::Group(Box::new(Expr::Empty))))));
	}

	#[test]
	fn chain_parser() {
		let mut input = DataInput::new(b"5oooooooo");
		// let parser = one_of(b"0123456789").map(|c|c - b'0') >> |n| take(n as usize) + sym(b'o').repeat(0..);
		let parser = skip(1) * take(3) >> |v:Vec<u8>|{
			take(5).map(move |u|{
				(u, v.clone())
			})
		};
		let output = parser.parse(&mut input);
		assert_eq!(output, Ok( (vec![b'o';5], vec![b'o';3]) ));
	}
}
