use super::{Error, Result};
use crate::range::Bound::*;
use crate::range::RangeArgument;
use crate::set::Set;
use std::fmt::{Debug, Display};
use std::ops::{Add, BitOr, Mul, Neg, Not, Shr, Sub};

type Parse<'a, I, O> = dyn Fn(&'a [I], usize) -> Result<(O, usize)> + 'a;

/// Parser combinator.
pub struct Parser<'a, I, O> {
	pub method: Box<Parse<'a, I, O>>,
}

impl<'a, I, O> Parser<'a, I, O> {
	/// Create new parser.
	pub fn new<P>(parse: P) -> Parser<'a, I, O>
	where
		P: Fn(&'a [I], usize) -> Result<(O, usize)> + 'a,
	{
		Parser {
			method: Box::new(parse),
		}
	}

	/// Apply the parser to parse input.
	pub fn parse(&self, input: &'a [I]) -> Result<O> {
		(self.method)(input, 0).map(|(out, _)| out)
	}

	/// Parse input at specified position.
	pub fn parse_at(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		(self.method)(input, start)
	}

	/// Convert parser result to desired value.
	pub fn map<U, F>(self, f: F) -> Parser<'a, I, U>
	where
		F: Fn(O) -> U + 'a,
		I: 'a,
		O: 'a,
		U: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start).map(|(out, pos)| (f(out), pos))
		})
	}

	/// Convert parser result to desired value, fail in case of conversion error.
	pub fn convert<U, E, F>(self, f: F) -> Parser<'a, I, U>
	where
		F: Fn(O) -> ::std::result::Result<U, E> + 'a,
		E: Debug,
		O: 'a,
		U: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start).and_then(|(res, pos)| match f(res) {
				Ok(out) => Ok((out, pos)),
				Err(err) => Err(Error::Conversion {
					message: format!("Conversion error: {:?}", err),
					position: start,
				}),
			})
		})
	}

	/// Cache parser output result to speed up backtracking.
	pub fn cache(self) -> Parser<'a, I, O>
	where
		O: Clone + 'a,
	{
		use std::cell::RefCell;
		use std::collections::HashMap;
		let results = RefCell::new(HashMap::new());
		Parser::new(move |input: &'a [I], start: usize| {
			let key = (start, format!("{:p}", &self.method));
			results
				.borrow_mut()
				.entry(key)
				.or_insert_with(|| (self.method)(input, start))
				.clone()
		})
	}

	/// Get input position after matching parser.
	pub fn pos(self) -> Parser<'a, I, usize>
	where
		O: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start).map(|(_, pos)| (pos, pos))
		})
	}

	/// Collect all matched input symbols.
	pub fn collect(self) -> Parser<'a, I, &'a [I]>
	where
		O: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start).map(|(_, end)| (&input[start..end], end))
		})
	}

	/// Discard parser output.
	pub fn discard(self) -> Parser<'a, I, ()>
	where
		O: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start).map(|(_, end)| ((), end))
		})
	}

	/// Make parser optional.
	pub fn opt(self) -> Parser<'a, I, Option<O>>
	where
		O: 'a,
	{
		Parser::new(
			move |input: &'a [I], start: usize| match (self.method)(input, start) {
				Ok((out, pos)) => Ok((Some(out), pos)),
				Err(_) => Ok((None, start)),
			},
		)
	}

	/// `p.repeat(5)` repeat p exactly 5 times
	/// `p.repeat(0..)` repeat p zero or more times
	/// `p.repeat(1..)` repeat p one or more times
	/// `p.repeat(1..4)` match p at least 1 and at most 3 times
	pub fn repeat<R>(self, range: R) -> Parser<'a, I, Vec<O>>
	where
		R: RangeArgument<usize> + Debug + 'a,
		O: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			let mut items = vec![];
			let mut pos = start;
			loop {
				match range.end() {
					Included(&max_count) => {
						if items.len() >= max_count {
							break;
						}
					}
					Excluded(&max_count) => {
						if items.len() + 1 >= max_count {
							break;
						}
					}
					Unbounded => (),
				}

				if let Ok((item, item_pos)) = (self.method)(input, pos) {
					items.push(item);
					pos = item_pos;
				} else {
					break;
				}
			}
			if let Included(&min_count) = range.start() {
				if items.len() < min_count {
					return Err(Error::Mismatch {
						message: format!(
							"expect repeat at least {} times, found {} times",
							min_count,
							items.len()
						),
						position: start,
					});
				}
			}
			Ok((items, pos))
		})
	}

	/// Give parser a name to identify parsing errors.
	pub fn name(self, name: &'a str) -> Parser<'a, I, O>
	where
		O: 'a,
	{
		Parser::new(
			move |input: &'a [I], start: usize| match (self.method)(input, start) {
				res @ Ok(_) => res,
				Err(err) => match err {
					Error::Custom { .. } => Err(err),
					_ => Err(Error::Custom {
						message: format!("failed to parse {}", name),
						position: start,
						inner: Some(Box::new(err)),
					}),
				},
			},
		)
	}

	/// Mark parser as expected, abort early when failed in ordered choice.
	pub fn expect(self, name: &'a str) -> Parser<'a, I, O>
	where
		O: 'a,
	{
		Parser::new(
			move |input: &'a [I], start: usize| match (self.method)(input, start) {
				res @ Ok(_) => res,
				Err(err) => Err(Error::Expect {
					message: format!("Expect {}", name),
					position: start,
					inner: Box::new(err),
				}),
			},
		)
	}
}

/// Always succeeds, consume no input.
pub fn empty<'a, I>() -> Parser<'a, I, ()> {
	Parser::new(|_: &[I], start: usize| Ok(((), start)))
}

/// Match any symbol.
pub fn any<'a, I>() -> Parser<'a, I, I>
where
	I: Clone,
{
	Parser::new(|input: &[I], start: usize| {
		if let Some(s) = input.get(start) {
			Ok((s.clone(), start + 1))
		} else {
			Err(Error::Mismatch {
				message: "end of input reached".to_owned(),
				position: start,
			})
		}
	})
}

/// Success when current input symbol equals `t`.
pub fn sym<'a, I>(t: I) -> Parser<'a, I, I>
where
	I: Clone + PartialEq + Display,
{
	Parser::new(move |input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			if t == *s {
				Ok((s.clone(), start + 1))
			} else {
				Err(Error::Mismatch {
					message: format!("expect: {}, found: {}", t, s),
					position: start,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Success when sequence of symbols matches current input.
pub fn seq<'a, 'b: 'a, I>(tag: &'b [I]) -> Parser<'a, I, &'a [I]>
where
	I: PartialEq + Debug,
{
	Parser::new(move |input: &'a [I], start: usize| {
		let mut index = 0;
		loop {
			let pos = start + index;
			if index == tag.len() {
				return Ok((tag, pos));
			}
			if let Some(s) = input.get(pos) {
				if tag[index] != *s {
					return Err(Error::Mismatch {
						message: format!("seq {:?} expect: {:?}, found: {:?}", tag, tag[index], s),
						position: pos,
					});
				}
			} else {
				return Err(Error::Incomplete);
			}
			index += 1;
		}
	})
}

/// Success when tag matches current input.
pub fn tag<'a, 'b: 'a>(tag: &'b str) -> Parser<'a, char, &'a str> {
	Parser::new(move |input: &'a [char], start: usize| {
		let mut pos = start;
		for c in tag.chars() {
			if let Some(&s) = input.get(pos) {
				if c != s {
					return Err(Error::Mismatch {
						message: format!("tag {:?} expect: {:?}, found: {}", tag, c, s),
						position: pos,
					});
				}
			} else {
				return Err(Error::Incomplete);
			}
			pos += 1;
		}
		Ok((tag, pos))
	})
}

/// Parse separated list.
pub fn list<'a, I, O, U>(
	parser: Parser<'a, I, O>,
	separator: Parser<'a, I, U>,
) -> Parser<'a, I, Vec<O>>
where
	O: 'a,
	U: 'a,
{
	Parser::new(move |input: &'a [I], start: usize| {
		let mut items = vec![];
		let mut pos = start;
		if let Ok((first_item, first_pos)) = (parser.method)(input, pos) {
			items.push(first_item);
			pos = first_pos;
			while let Ok((_, sep_pos)) = (separator.method)(input, pos) {
				match (parser.method)(input, sep_pos) {
					Ok((more_item, more_pos)) => {
						items.push(more_item);
						pos = more_pos;
					}
					Err(_) => break,
				}
			}
		}
		Ok((items, pos))
	})
}

/// Success when current input symbol is one of the set.
pub fn one_of<'a, I, S>(set: &'a S) -> Parser<'a, I, I>
where
	I: Clone + PartialEq + Display + Debug,
	S: Set<I> + ?Sized,
{
	Parser::new(move |input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			if set.contains(s) {
				Ok((s.clone(), start + 1))
			} else {
				Err(Error::Mismatch {
					message: format!("expect one of: {}, found: {}", set.to_str(), s),
					position: start,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Success when current input symbol is none of the set.
pub fn none_of<'a, I, S>(set: &'static S) -> Parser<'a, I, I>
where
	I: Clone + PartialEq + Display + Debug,
	S: Set<I> + ?Sized,
{
	Parser::new(move |input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			if set.contains(s) {
				Err(Error::Mismatch {
					message: format!("expect none of: {}, found: {}", set.to_str(), s),
					position: start,
				})
			} else {
				Ok((s.clone(), start + 1))
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Success when predicate returns true on current input symbol.
pub fn is_a<'a, I, F>(predicate: F) -> Parser<'a, I, I>
where
	I: Clone + PartialEq + Display + Debug,
	F: Fn(I) -> bool + 'a,
{
	Parser::new(move |input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			if predicate(s.clone()) {
				Ok((s.clone(), start + 1))
			} else {
				Err(Error::Mismatch {
					message: format!("is_a predicate failed on: {}", s),
					position: start,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Success when predicate returns false on current input symbol.
pub fn not_a<'a, I, F>(predicate: F) -> Parser<'a, I, I>
where
	I: Clone + PartialEq + Display + Debug,
	F: Fn(I) -> bool + 'a,
{
	Parser::new(move |input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			if predicate(s.clone()) {
				Err(Error::Mismatch {
					message: format!("not_a predicate failed on: {}", s),
					position: start,
				})
			} else {
				Ok((s.clone(), start + 1))
			}
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Read n symbols.
pub fn take<'a, I>(n: usize) -> Parser<'a, I, &'a [I]> {
	Parser::new(move |input: &'a [I], start: usize| {
		let pos = start + n;
		if input.len() >= pos {
			Ok((&input[start..pos], pos))
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Skip n symbols.
pub fn skip<'a, I>(n: usize) -> Parser<'a, I, ()> {
	Parser::new(move |input: &'a [I], start: usize| {
		let pos = start + n;
		if input.len() >= pos {
			Ok(((), pos))
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Call a parser factory, can be used to create recursive parsers.
pub fn call<'a, I, O, F>(parser_factory: F) -> Parser<'a, I, O>
where
	O: 'a,
	F: Fn() -> Parser<'a, I, O> + 'a,
{
	Parser::new(move |input: &'a [I], start: usize| {
		let parser = parser_factory();
		(parser.method)(input, start)
	})
}

/// Success when end of input is reached.
pub fn end<'a, I>() -> Parser<'a, I, ()>
where
	I: Display,
{
	Parser::new(|input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			Err(Error::Mismatch {
				message: format!("expect end of input, found: {}", s),
				position: start,
			})
		} else {
			Ok(((), start))
		}
	})
}

/// Sequence reserve value
impl<'a, I, O: 'a, U: 'a> Add<Parser<'a, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'a, I, (O, U)>;

	fn add(self, other: Parser<'a, I, U>) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start).and_then(|(out1, pos1)| {
				(other.method)(input, pos1).map(|(out2, pos2)| ((out1, out2), pos2))
			})
		})
	}
}

/// Sequence discard second value
impl<'a, I, O: 'a, U: 'a> Sub<Parser<'a, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'a, I, O>;

	fn sub(self, other: Parser<'a, I, U>) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start)
				.and_then(|(out1, pos1)| (other.method)(input, pos1).map(|(_, pos2)| (out1, pos2)))
		})
	}
}

/// Sequence discard first value
impl<'a, I: 'a, O: 'a, U: 'a> Mul<Parser<'a, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'a, I, U>;

	fn mul(self, other: Parser<'a, I, U>) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start)
				.and_then(|(_, pos1)| (other.method)(input, pos1).map(|(out2, pos2)| (out2, pos2)))
		})
	}
}

/// Chain two parsers where the second parser depends on the first's result.
impl<'a, I, O: 'a, U: 'a, F: Fn(O) -> Parser<'a, I, U> + 'a> Shr<F> for Parser<'a, I, O> {
	type Output = Parser<'a, I, U>;

	fn shr(self, other: F) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start).and_then(|(out, pos)| (other(out).method)(input, pos))
		})
	}
}

/// Ordered choice
impl<'a, I, O: 'a> BitOr for Parser<'a, I, O> {
	type Output = Parser<'a, I, O>;

	fn bitor(self, other: Parser<'a, I, O>) -> Self::Output {
		Parser::new(
			move |input: &'a [I], start: usize| match (self.method)(input, start) {
				Ok(out) => Ok(out),
				Err(err) => match err {
					Error::Expect { .. } => Err(err),
					_ => (other.method)(input, start),
				},
			},
		)
	}
}

/// And predicate
impl<'a, I, O: 'a> Neg for Parser<'a, I, O> {
	type Output = Parser<'a, I, bool>;

	fn neg(self) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			(self.method)(input, start).map(|_| (true, start))
		})
	}
}

/// Not predicate
impl<'a, I, O: 'a> Not for Parser<'a, I, O> {
	type Output = Parser<'a, I, bool>;

	fn not(self) -> Self::Output {
		Parser::new(
			move |input: &'a [I], start: usize| match (self.method)(input, start) {
				Ok(_) => Err(Error::Mismatch {
					message: "not predicate failed".to_string(),
					position: start,
				}),
				Err(_) => Ok((true, start)),
			},
		)
	}
}

#[cfg(test)]
mod tests {
	use crate::parser::*;
	use crate::Error;

	#[test]
	fn byte_works() {
		let input = b"abcde";
		let parser = sym(b'a') + one_of(b"ab") - sym(b'C');
		let output = parser.parse(input);
		assert_eq!(
			output,
			Err(Error::Mismatch {
				message: "expect: 67, found: 99".to_string(),
				position: 2
			})
		);

		let parser = sym(b'a') * none_of(b"AB") - sym(b'c') + seq(b"de");
		let output = parser.parse(input);
		assert_eq!(output, Ok((b'b', &b"de"[..])));
		assert_eq!(parser.pos().parse(input), Ok(5));

		let parser = sym(b'e') | sym(b'd').expect("d") | empty().map(|_| b'0');
		let output = parser.parse(input);
		assert_eq!(
			output,
			Err(Error::Expect {
				message: "Expect d".to_owned(),
				position: 0,
				inner: Box::new(Error::Mismatch {
					message: "expect: 100, found: 97".to_string(),
					position: 0
				})
			})
		);
	}

	#[test]
	fn char_works() {
		let input = "abcd".chars().collect::<Vec<char>>();
		let parser = tag("ab") + sym('c') | sym('d').map(|_| ("", '0'));
		let output = parser.parse(&input);
		assert_eq!(output, Ok(("ab", 'c')));
	}

	#[test]
	fn recursive_parser() {
		#[derive(Debug, PartialEq)]
		enum Expr {
			Empty,
			Group(Box<Expr>),
		}
		fn expr() -> Parser<'static, u8, Expr> {
			(sym(b'(') + call(expr) - sym(b')')).map(|(_, e)| Expr::Group(Box::new(e)))
				| empty().map(|_| Expr::Empty)
		}
		let input = b"(())";
		let parser = expr();
		let output = parser.parse(input);
		assert_eq!(
			output,
			Ok(Expr::Group(Box::new(Expr::Group(Box::new(Expr::Empty)))))
		);
	}

	#[test]
	fn chain_parser() {
		let input = b"5oooooooo";
		{
			let parser = one_of(b"0123456789").map(|c| c - b'0')
				>> |n| take(n as usize) + sym(b'o').repeat(0..);
			assert_eq!(parser.parse(input), Ok((&b"ooooo"[..], vec![b'o'; 3])));
		}
		{
			let parser =
				skip(1) * take(3) >> |v: &'static [u8]| take(v.len() + 2).map(move |u| (u, v));
			assert_eq!(parser.parse(input), Ok((&b"ooooo"[..], &b"ooo"[..])));
		}
		{
			let parser = Parser::new(move |input, start| {
				(skip(1) * take(3))
					.parse_at(input, start)
					.and_then(|(v, pos)| {
						take(v.len() + 2)
							.parse_at(input, pos)
							.map(|(u, end)| ((u, v), end))
					})
			});
			assert_eq!(parser.parse(input), Ok((&b"ooooo"[..], &b"ooo"[..])));
		}
	}

	#[test]
	fn repeat_at_least() {
		let input = b"xxxooo";

		{
			let parser = sym(b'x').repeat(1..2);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 1]))
		}

		{
			let parser = sym(b'x').repeat(1..);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'x').repeat(0..);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'y').repeat(0..);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![]))
		}

		{
			let parser = sym(b'y').repeat(1..);
			let output = parser.parse(input);
			assert!(output.is_err());
		}

		{
			let parser = sym(b'x').repeat(10..);
			let output = parser.parse(input);
			assert!(output.is_err());
		}
	}

	#[test]
	fn repeat_up_to() {
		let input = b"xxxooo";

		{
			let parser = sym(b'x').repeat(..2);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 1]))
		}

		{
			let parser = sym(b'x').repeat(..4);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'x').repeat(..);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'x').repeat(..0);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![]))
		}

		{
			let parser = sym(b'x').repeat(..10);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}
	}

	#[test]
	fn repeat_exactly() {
		let input = b"xxxooo";

		{
			let parser = sym(b'x').repeat(0);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![]))
		}

		{
			let parser = sym(b'x').repeat(1);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 1]))
		}

		{
			let parser = sym(b'x').repeat(2);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 2]))
		}

		{
			let parser = sym(b'x').repeat(3);
			let output = parser.parse(input);
			assert_eq!(output, Ok(vec![b'x'; 3]))
		}

		{
			let parser = sym(b'x').repeat(4);
			let output = parser.parse(input);
			assert!(output.is_err())
		}
	}
}
