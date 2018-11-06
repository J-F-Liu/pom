use std::fmt::{Display, Debug};
use std::ops::{Add, Sub, Mul, Shr, BitOr, Neg, Not};
use super::{Result, Error, Input, Train};
use crate::set::Set;
use crate::range::RangeArgument;
use crate::range::Bound::*;

/// Parser combinator.
pub struct Parser<'a, I, O> {
	method: Box<Fn(&mut Input<I>) -> Result<O> + 'a>,
}

impl<'a, I, O> Parser<'a, I, O> {
	/// Create new parser.
	pub fn new<P>(parse: P) -> Parser<'a, I, O>
		where P: Fn(&mut Input<I>) -> Result<O> + 'a
	{
		Parser { method: Box::new(parse) }
	}

	/// Apply the parser to parse input.
	pub fn parse(&self, input: &mut Input<I>) -> Result<O> {
		(self.method)(input)
	}

	/// Convert parser result to desired value.
	pub fn map<U, F>(self, f: F) -> Parser<'a, I, U>
		where F: Fn(O) -> U + 'a,
			  I: 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			self.parse(input).map(&f)
		})
	}

	/// Convert parser result to desired value, fail in case of conversion error.
	pub fn convert<U, E, F>(self, f: F) -> Parser<'a, I, U>
		where F: Fn(O) -> ::std::result::Result<U, E> + 'a,
			  E: Debug,
			  I: Copy + 'static,
			  O: 'static,
			  U: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			self.parse(input).and_then(|res|{
				match f(res) {
					Ok(out) => Ok(out),
					Err(err) => {
						input.jump_to(start);
						Err(Error::Conversion{
							message: format!("Conversion error: {:?}", err),
							position: start,
						})
					}
				}
			})
		})
	}

	/// Get input position after matching parser.
	pub fn pos(self) -> Parser<'a, I, usize>
		where I: Copy + 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			self.parse(input).map(|_|input.position())
		})
	}

	/// Collect all matched input symbols.
	pub fn collect(self) -> Parser<'a, I, Vec<I>>
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
	pub fn discard(self) -> Parser<'a, I, ()>
		where I: 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			self.parse(input).map(|_|())
		})
	}

	/// Make parser optional.
	pub fn opt(self) -> Parser<'a, I, Option<O>>
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

	/// `p.repeat(5)` repeat p exactly 5 times
	/// `p.repeat(0..)` repeat p zero or more times
	/// `p.repeat(1..)` repeat p one or more times
	/// `p.repeat(1..4)` match p at least 1 and at most 3 times
	pub fn repeat<R>(self, range: R) -> Parser<'a, I, Vec<O>>
		where R: RangeArgument<usize> + Debug + 'a,
			  I: Copy + 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start_pos = input.position();
			let mut items = vec![];
			loop {
				match range.end() {
					Included(&end) => if items.len() >= end { break; },
					Excluded(&end) => if items.len() + 1 >= end { break; },
					Unbounded => (),
				}

				if let Ok(item) = self.parse(input) {
					items.push(item)
				} else {
					break;
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

	/// Give parser a name to identify parsing errors.
	pub fn name(self, name: &'a str) -> Parser<'a, I, O>
		where I: Copy + 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			match self.parse(input) {
				Ok(out) => Ok(out),
				Err(err) => match err {
					Error::Custom{..} => Err(err),
					_ => Err(Error::Custom {
						message: format!("failed to parse {}", name),
						position: start,
						inner: Some(Box::new(err)),
					})
				}
			}
		})
	}

	/// Mark parser as expected, abort early when failed in ordered choice.
	pub fn expect(self, name: &'a str) -> Parser<'a, I, O>
		where I: Copy + 'static,
			  O: 'static
	{
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			match self.parse(input) {
				Ok(out) => Ok(out),
				Err(err) => Err(Error::Expect {
					message: format!("Expect {}", name),
					position: start,
					inner: Box::new(err),
				})
			}
		})
	}
}

/// Always succeeds, consume no input.
pub fn empty<'a, I>() -> Parser<'a, I, ()> {
	Parser::new(|_: &mut Input<I>| Ok(()))
}

/// Success when current input symbol equals `t`.
pub fn sym<'a, I>(t: I) -> Parser<'a, I, I>
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

/// Success when sequence of symbols matches current input.
pub fn seq<'a, I, T>(train: &'static T) -> Parser<'a, I, Vec<I>>
	where I: Copy + PartialEq + Display + 'static,
		  T: Train<I> + ?Sized
{
	Parser::new(move |input: &mut Input<I>| {
		let tag = train.knots();
		let start = input.position();
		let mut index = 0;
		let result;

		loop {
			if index == tag.len() {
				result = Ok(tag);
				break;
			}
			if let Some(s) = input.current() {
				if tag[index] == s {
					input.advance();
				} else {
					result = Err(Error::Mismatch {
						message: format!("seq {} expect: {}, found: {}", train.to_str(), tag[index], s),
						position: input.position(),
					});
					break;
				}
			} else {
				result = Err(Error::Incomplete);
				break;
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
pub fn list<'a, I, O, U>(parser: Parser<'a, I, O>, separator: Parser<'a, I, U>) -> Parser<'a, I, Vec<O>>
	where I: Copy + 'static,
		  O: 'static,
		  U: 'static
{
	Parser::new(move |input: &mut Input<I>| {
		let mut items = vec![];
		if let Ok(first_item) = parser.parse(input) {
			items.push(first_item);
			let mut start = input.position();
			while let Ok(_) = separator.parse(input) {
				match parser.parse(input) {
					Ok(more_item) => items.push(more_item),
					Err(_) => {
						input.jump_to(start);
						break;
					}
				}
				start = input.position();
			}
		}
		return Ok(items);
	})
}

/// Success when current input symbol is one of the set.
pub fn one_of<'a, I, S>(set: &'static S) -> Parser<'a, I, I>
	where I: Copy + PartialEq + Display + Debug + 'static,
		  S: Set<I> + ?Sized
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if set.contains(&s) {
				input.advance();
				Ok(s)
			} else {
				Err(Error::Mismatch {
					message: format!("expect one of: {}, found: {}", set.to_str(), s),
					position: input.position(),
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Success when current input symbol is none of the set.
pub fn none_of<'a, I, S>(set: &'static S) -> Parser<'a, I, I>
	where I: Copy + PartialEq + Display + Debug + 'static,
		  S: Set<I> + ?Sized
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if set.contains(&s) {
				Err(Error::Mismatch {
					message: format!("expect none of: {}, found: {}", set.to_str(), s),
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


/// Success when predicate returns true on current input symbol.
pub fn is_a<'a, I, F>(predicate: F) -> Parser<'a, I, I>
	where I: Copy + PartialEq + Display + Debug + 'static,
		  F: Fn(I) -> bool + 'a
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if predicate(s) {
				input.advance();
				Ok(s)
			} else {
				Err(Error::Mismatch {
					message: format!("is_a predicate failed on: {}", s),
					position: input.position(),
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Success when predicate returns false on current input symbol.
pub fn not_a<'a, I, F>(predicate: F) -> Parser<'a, I, I>
	where I: Copy + PartialEq + Display + Debug + 'static,
		  F: Fn(I) -> bool + 'a
{
	Parser::new(move |input: &mut Input<I>| {
		if let Some(s) = input.current() {
			if predicate(s) {
				Err(Error::Mismatch {
					message: format!("not_a predicate failed on: {}", s),
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

/// Read n symbols.
pub fn take<'a, I>(n: usize) -> Parser<'a, I, Vec<I>>
	where I: Copy + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		let start = input.position();
		let mut symbols = Vec::with_capacity(n);
		if n > 0 {
			while let Some(symbol) = input.current() {
				input.advance();
				symbols.push(symbol);
				if symbols.len() == n {
					break;
				}
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
pub fn skip<'a, I>(n: usize) -> Parser<'a, I, ()>
	where I: Copy + 'static
{
	Parser::new(move |input: &mut Input<I>| {
		let start = input.position();
		let mut count = 0;
		if n > 0 {
			while let Some(_) = input.current() {
				input.advance();
				count += 1;
				if count == n {
					break;
				}
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
pub fn call<'a, I, O, F>(parser_factory: F) -> Parser<'a, I, O>
	where I: 'static,
		  O: 'static,
		  F: Fn() -> Parser<'a, I, O> + 'a
{
	Parser::new(move |input: &mut Input<I>| {
		let parser = parser_factory();
		parser.parse(input)
	})
}

/// Success when end of input is reached.
pub fn end<'a, I>() -> Parser<'a, I, ()>
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
impl<'b, 'a: 'b, I: Copy + 'static, O: 'static, U: 'static> Add<Parser<'b, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'b, I, (O, U)>;

	fn add(self, other: Parser<'b, I, U>) -> Self::Output {
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
impl<'a, 'b: 'a, I: Copy + 'static, O: 'static, U: 'static> Sub<Parser<'b, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'a, I, O>;

	fn sub(self, other: Parser<'b, I, U>) -> Self::Output {
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
impl<'b, 'a: 'b, I: Copy + 'static, O: 'static, U: 'static> Mul<Parser<'b, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'b, I, U>;

	fn mul(self, other: Parser<'b, I, U>) -> Self::Output {
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
impl<'b, 'a: 'b, I: Copy + 'static, O: 'static, U: 'static, F: Fn(O) -> Parser<'b, I, U> + 'b> Shr<F> for Parser<'a, I, O> {
	type Output = Parser<'b, I, U>;

	fn shr(self, other: F) -> Self::Output {
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
impl<'a, I: 'static, O: 'static> BitOr for Parser<'a, I, O> {
	type Output = Parser<'a, I, O>;

	fn bitor(self, other: Parser<'a, I, O>) -> Self::Output {
		Parser::new(move |input: &mut Input<I>| {
			match self.parse(input) {
				Ok(out) => Ok(out),
				Err(err) => match err {
					Error::Expect{..} => Err(err),
					_ => other.parse(input)
				}
			}
		})
	}
}

/// And predicate
impl<'a, I: Copy + 'static, O: 'static> Neg for Parser<'a, I, O> {
	type Output = Parser<'a, I, bool>;

	fn neg(self) -> Self::Output {
		Parser::new(move |input: &mut Input<I>| {
			let start = input.position();
			let result = self.parse(input);
			input.jump_to(start);
			result.map(|_| true)
		})
	}
}

/// Not predicate
impl<'a, I: Copy + 'static, O: 'static> Not for Parser<'a, I, O> {
	type Output = Parser<'a, I, bool>;

	fn not(self) -> Self::Output {
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
	use crate::parser::*;
	use crate::{DataInput, TextInput};
	use crate::{Error, Input};

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
		assert_eq!(empty().pos().parse(&mut input), Ok( 5 ) );

		let parser = sym(b'e') | sym(b'd').expect("d") | empty().map(|_| b'0');
		let output = parser.parse(&mut input);
		assert_eq!(output, Err(Error::Expect { message: "Expect d".to_owned(), position: 5, inner: Box::new(Error::Incomplete) }));
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
		fn expr() -> Parser<'static, u8, Expr> {
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
			take(v.len()+2).map(move |u|{
				(u, v.clone())
			})
		};
		// To avoid clone v:
		// let parser = Parser::new(move |input| {
		// 	(skip(1) * take(3)).parse(input).and_then(|v:Vec<u8>| {
		// 			take(v.len()+2).parse(input).map(|u|{
		// 				(u, v)
		// 			})
		// 	})
		// });
		let output = parser.parse(&mut input);
		assert_eq!(output, Ok( (vec![b'o';5], vec![b'o';3]) ));
	}

	#[test]
	fn repeat_at_least() {
		let input = DataInput::new(b"xxxooo");

		{
			let parser = sym(b'x').repeat(1..2);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 1]))
		}

		{
			let parser = sym(b'x').repeat(1..);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'x').repeat(0..);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'y').repeat(0..);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![]))
		}

		{
			let parser = sym(b'y').repeat(1..);
			let output = parser.parse(&mut input.clone());
			assert!(output.is_err());
		}

		{
			let parser = sym(b'x').repeat(10..);
			let output = parser.parse(&mut input.clone());
			assert!(output.is_err());
		}
	}

	#[test]
	fn repeat_up_to() {
		let input = DataInput::new(b"xxxooo");

		{
			let parser = sym(b'x').repeat(..2);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 1]))
		}

		{
			let parser = sym(b'x').repeat(..4);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'x').repeat(..);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'x').repeat(..0);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![]))
		}

		{
			let parser = sym(b'x').repeat(..10);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}
	}


	#[test]
	fn repeat_exactly() {
		let input = DataInput::new(b"xxxooo");

		{
			let parser = sym(b'x').repeat(0);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![]))
		}

		{
			let parser = sym(b'x').repeat(1);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x';1]))
		}

		{
			let parser = sym(b'x').repeat(2);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 2]))
		}

		{
			let parser = sym(b'x').repeat(3);
			let output = parser.parse(&mut input.clone());
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'x').repeat(4);
			let output = parser.parse(&mut input.clone());
			assert!(output.is_err())
		}
	}
}
