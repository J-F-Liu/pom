use std::fmt::{Display, Debug};
use std::ops::{Range, RangeFrom, RangeTo, RangeFull};
use super::{Result, Error};

pub enum Bound<'a, T: 'a> {
	Excluded(&'a T),
	Included(&'a T),
	Unbounded,
}
use self::Bound::*;

pub trait RangeArgument<T> {
	fn start(&self) -> Bound<T>;
	fn end(&self) -> Bound<T>;
}

impl<T> RangeArgument<T> for Range<T> {
	fn start(&self) -> Bound<T> { Included(&self.start) }
	fn end(&self) -> Bound<T> { Excluded(&self.end) }
}

impl<T> RangeArgument<T> for RangeFrom<T> {
	fn start(&self) -> Bound<T> { Included(&self.start) }
	fn end(&self) -> Bound<T> { Unbounded }
}

impl<T> RangeArgument<T> for RangeTo<T> {
	fn start(&self) -> Bound<T> { Unbounded }
	fn end(&self) -> Bound<T> { Excluded(&self.end) }
}

impl<T> RangeArgument<T> for RangeFull {
	fn start(&self) -> Bound<T> { Unbounded }
	fn end(&self) -> Bound<T> { Unbounded }
}

impl RangeArgument<usize> for usize {
	fn start(&self) -> Bound<usize> { Included(self) }
	fn end(&self) -> Bound<usize> { Included(self) }
}

/// Parser trait.
pub trait Parser<'a, I, O> {
	/// Parse input at start position, return output and finished position.
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)>;
}

	/// Convert parser result to desired value.
	pub fn map<'a, I: 'a, O, U, P: Parser<'a, I, O>, F: Fn(O) -> U>(parser: P, f: F) -> impl Parser<'a, I, U>
	{
		move |input: &'a [I], start: usize| {
			parser.parse(input, start).map(|(out, pos)|(f(out), pos))
		}
	}

	/// Convert parser result to desired value, fail in case of conversion error.
	pub fn convert<'a, I: 'a, O, P: Parser<'a, I, O>, U, E, F>(parser: P, f: F) -> impl Parser<'a, I, U>
		where F: Fn(O) -> ::std::result::Result<U, E>,
			  E: Debug
	{
		move |input: &'a [I], start: usize| {
			parser.parse(input, start).and_then(|(res, pos)|{
				match f(res) {
					Ok(out) => Ok((out, pos)),
					Err(err) => {
						Err(Error::Conversion{
							message: format!("Conversion error: {:?}", err),
							position: start,
						})
					}
				}
			})
		}
	}

	// /// Get input position after matching parser.
	// pub fn pos(self) -> Parser<'a, I, usize>
	// 	where I: Copy + 'static,
	// 		  O: 'static
	// {
	// 	Parser::new(move |input: &mut Input<I>| {
	// 		self.parse(input).map(|_|input.position())
	// 	})
	// }

	/// Collect all matched input symbols.
	pub fn collect<'a, I: 'a, O, P: Parser<'a, I, O>>(parser: P) -> impl Parser<'a, I, &'a [I]>
	{
		move |input: &'a [I], start: usize| {
			parser.parse(input, start).map(|(_, end)|(&input[start..end], end))
		}
	}

	/// Discard parser output.
	pub fn discard<'a, I: 'a, O, P: Parser<'a, I, O>>(parser: P) -> impl Parser<'a, I, ()>
	{
		move |input: &'a [I], start: usize| {
			parser.parse(input, start).map(|(_, end)|((), end))
		}
	}

	/// Make parser optional.
	pub fn opt<'a, I: 'a, O, P: Parser<'a, I, O>>(parser: P) -> impl Parser<'a, I, Option<O>>
	{
		move |input: &'a [I], start: usize| {
			match parser.parse(input, start) {
				Ok((out, pos)) => Ok((Some(out), pos)),
				Err(_) => Ok((None, start)),
			}
		}
	}

	/// `p.repeat(5)` repeat p exactly 5 times
	/// `p.repeat(0..)` repeat p zero or more times
	/// `p.repeat(1..)` repeat p one or more times
	/// `p.repeat(1..4)` match p at least 1 and at most 3 times
	pub fn repeat<'a, I: 'a, O, P: Parser<'a, I, O>, R>(parser: P, range: R) -> impl Parser<'a, I, Vec<O>>
		where R: RangeArgument<usize> + Debug + 'a
	{
		move |input: &'a [I], start_pos: usize| {
			let mut items = vec![];
			let mut pos = start_pos;
			loop {
				match range.end() {
					Included(&end) => if items.len() >= end { break; },
					Excluded(&end) => if items.len() + 1 >= end { break; },
					Unbounded => (),
				}

				if let Ok((item, item_pos)) = parser.parse(input, pos) {
					items.push(item);
					pos = item_pos;
				} else {
					break;
				}
			}
			if let Included(&start) = range.start() {
				if items.len() < start {
					return Err(Error::Mismatch {
						message: format!("expect repeat at least {} times, found {} times", start, items.len()),
						position: start_pos,
					});
				}
			}
			Ok((items, pos))
		}
	}

	/// Give parser a name to identify parsing errors.
	pub fn name<'a, I: 'a, O, P: Parser<'a, I, O>>(parser: P, name: &'a str) -> impl Parser<'a, I, O>
	{
		move |input: &'a [I], start: usize| {
			match parser.parse(input, start) {
				res @ Ok(_) => res,
				Err(err) => match err {
					Error::Custom{..} => Err(err),
					_ => Err(Error::Custom {
						message: format!("failed to parse {}", name),
						position: start,
						inner: Some(Box::new(err)),
					})
				}
			}
		}
	}

impl<'a, I: 'a, O, F> Parser<'a, I, O> for F where F: Fn(&'a [I], usize) -> Result<(O, usize)> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self(input, start)
	}
}

// impl<'a, I: 'a, O, P: Parser<'a, I, O>> Parser<'a, I, O> for Box<P> {
// 	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
// 		self.parse(input, start)
// 	}
// }

/// Always succeeds, consume no input.
pub fn empty<'a, I: 'a>() -> impl Parser<'a, I, ()> {
	|_: &[I], start: usize| Ok(((), start))
}

/// Success when current input symbol equals `t`.
pub fn sym<'a, I: 'a>(t: I) -> impl Parser<'a, I, I>
	where I: Copy + PartialEq + Display
{
	move |input: &'a [I], start: usize| {
		if let Some(&s) = input.get(start) {
			if t == s {
				Ok((t, start + 1))
			} else {
				Err(Error::Mismatch {
					message: format!("expect: {}, found: {}", t, s),
					position: start,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	}
}

/// Success when sequence of symbols matches current input.
pub fn seq<'a, I: 'a>(tag: &'a [I]) -> impl Parser<'a, I, &'a [I]>
	where I: Copy + PartialEq + Debug
{
	move |input: &'a [I], start: usize| {
		let mut index = 0;
		loop {
			let pos = start + index;
			if index == tag.len() {
				return Ok((tag, pos));
			}
			if let Some(&s) = input.get(pos) {
				if tag[index] != s {
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
	}
}

/// Parse separated list.
pub fn list<'a, I: 'a, O, U, P: Parser<'a, I, O>, S: Parser<'a, I, U>>(parser: P, separator: S) -> impl Parser<'a, I, Vec<O>>
	where I: Copy
{
	move |input: &'a [I], start: usize| {
		let mut items = vec![];
		let mut pos = start;
		if let Ok((first_item, first_pos)) = parser.parse(input, pos) {
			items.push(first_item);
			pos = first_pos;
			while let Ok((_, sep_pos)) = separator.parse(input, pos) {
				match parser.parse(input, sep_pos) {
					Ok((more_item, more_pos)) => {
						items.push(more_item);
						pos = more_pos;
					},
					Err(_) => break,
				}
			}
		}
		return Ok((items, pos));
	}
}

/// Success when current input symbol is one of the set.
pub fn one_of<'a, I: 'a>(set: &'a [I]) -> impl Parser<'a, I, I>
	where I: Copy + PartialEq + Debug
{
	move |input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			if set.contains(s) {
				Ok((*s, start + 1))
			} else {
				Err(Error::Mismatch {
					message: format!("expect one of: {:?}, found: {:?}", set, s),
					position: start,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	}
}

/// Success when current input symbol is none of the set.
pub fn none_of<'a, I: 'a>(set: &'a [I]) -> impl Parser<'a, I, I>
	where I: Copy + PartialEq + Debug
{
	move |input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			if set.contains(&s) {
				Err(Error::Mismatch {
					message: format!("expect none of: {:?}, found: {:?}", set, s),
					position: start,
				})
			} else {
				Ok((*s, start + 1))
			}
		} else {
			Err(Error::Incomplete)
		}
	}
}

/// Success when predicate returns true on current input symbol.
pub fn is_a<'a, I: 'a, F>(predicate: F) -> impl Parser<'a, I, I>
	where I: Copy + PartialEq + Debug,
		  F: Fn(I) -> bool
{
	move |input: &'a [I], start: usize| {
		if let Some(&s) = input.get(start) {
			if predicate(s) {
				Ok((s, start + 1))
			} else {
				Err(Error::Mismatch {
					message: format!("is_a predicate failed on: {:?}", s),
					position: start,
				})
			}
		} else {
			Err(Error::Incomplete)
		}
	}
}

/// Success when predicate returns false on current input symbol.
pub fn not_a<'a, I: 'a, F>(predicate: F) -> impl Parser<'a, I, I>
	where I: Copy + PartialEq + Debug,
		  F: Fn(I) -> bool
{
	move |input: &'a [I], start: usize| {
		if let Some(&s) = input.get(start) {
			if predicate(s) {
				Err(Error::Mismatch {
					message: format!("not_a predicate failed on: {:?}", s),
					position: start,
				})
			} else {
				Ok((s, start + 1))
			}
		} else {
			Err(Error::Incomplete)
		}
	}
}

/// Success when the range contains current input symbol.
pub fn range<'a, I: 'a, R>(set: R) -> impl Parser<'a, I, I>
	where I: Copy + PartialOrd<I> + Display + Debug,
		  R: RangeArgument<I> + Debug
{
	move |input: &'a [I], pos: usize| {
		if let Some(&s) = input.get(pos) {
			let meet_start = match set.start() {
				Included(&start) => s >= start,
				Excluded(&start) => s > start,
				Unbounded => true,
			};
			if !meet_start {
				return Err(Error::Mismatch {
					message: format!("expect range: {:?}, found: {}", set, s),
					position: pos,
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
					position: pos,
				});
			}
			return Ok((s, pos + 1));
		} else {
			return Err(Error::Incomplete);
		}
	}
}

/// Read n symbols.
pub fn take<'a, I: 'a>(n: usize) -> impl Parser<'a, I, &'a [I]>
{
	move |input: &'a [I], start: usize| {
		if input.len() >= n {
			let pos = start + n;
			Ok((&input[start..pos], pos))
		} else {
			Err(Error::Incomplete)
		}
	}
}

/// Skip n symbols.
pub fn skip<'a, I: 'a>(n: usize) -> impl Parser<'a, I, ()>
	where I: Copy + 'static
{
	move |input: &'a [I], start: usize| {
		if input.len() >= n {
			Ok(((), start + n))
		} else {
			Err(Error::Incomplete)
		}
	}
}

/// Call a parser factory, can be used to create recursive parsers.
pub fn call<'a, I: 'a, O, F, P>(parser_factory: F) -> impl Parser<'a, I, O>
	where F: Fn() -> P, P: Parser<'a, I, O>
{
	move |input: &'a [I], start: usize| {
		let parser = parser_factory();
		parser.parse(input, start)
	}
}

/// Success when end of input is reached.
pub fn end<'a, I: Debug + 'a>() -> impl Parser<'a, I, ()> {
	|input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			Err(Error::Mismatch{
				message: format!("expect end of input, found: {:?}", s),
				position: start,
			})
		} else {
			Ok(((), start))
		}
	}
}

impl<'a> Parser<'a, u8, u8> for u8 {
	fn parse(&self, input: &'a [u8], start: usize) -> Result<(u8, usize)> {
		sym(*self).parse(input, start)
	}
}

impl<'a> Parser<'a, u8, &'a [u8]> for &'a str {
	fn parse(&self, input: &'a [u8], start: usize) -> Result<(&'a [u8], usize)> {
		seq(self.as_bytes()).parse(input, start)
	}
}

impl<'a> Parser<'a, char, char> for char {
	fn parse(&self, input: &'a [char], start: usize) -> Result<(char, usize)> {
		sym(*self).parse(input, start)
	}
}

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

impl<'a, I, O1, O2, P1: Parser<'a, I, O1>, P2: Parser<'a, I, O2>> Parser<'a, I, (O1, O2)> for (P1, P2) {
	fn parse(&self, input: &'a [I], start: usize) -> Result<((O1, O2), usize)> {
		self.0.parse(input, start).and_then(|(out1, pos1)|
			self.1.parse(input, pos1).map(|(out2, pos2)|
				((out1, out2), pos2)
			)
		)
	}
}

// pub trait First<T> {
// 	fn first(&self) -> T;
// }
// impl<'a, I, O1, O2, P1: Parser<'a, I, O1>, P2: Parser<'a, I, O2>, P3: Parser<'a, I, O1>> First<P3> for (P1, P2) {
// 	fn first(&self) -> P3 {
// 		move |input: &'a [I], start: usize| {
// 			self.parse(input, start).map(|((v, _), pos)|(v, pos))
// 		}
// 	}
// }

impl<'a, I, O1, O2, O3, P1: Parser<'a, I, O1>, P2: Parser<'a, I, O2>, P3: Parser<'a, I, O3>> Parser<'a, I, (O1, O2, O3)> for (P1, P2, P3) {
	fn parse(&self, input: &'a [I], start: usize) -> Result<((O1, O2, O3), usize)> {
		self.0.parse(input, start).and_then(|(out1, pos1)|
			self.1.parse(input, pos1).and_then(|(out2, pos2)|
				self.2.parse(input, pos2).map(|(out3, pos3)|
					((out1, out2, out3), pos3)
				)
			)
		)
	}
}

impl<'a, I, O1, O2, O3, O4, P1: Parser<'a, I, O1>, P2: Parser<'a, I, O2>, P3: Parser<'a, I, O3>, P4: Parser<'a, I, O4>> Parser<'a, I, (O1, O2, O3, O4)> for (P1, P2, P3, P4) {
	fn parse(&self, input: &'a [I], start: usize) -> Result<((O1, O2, O3, O4), usize)> {
		self.0.parse(input, start).and_then(|(out1, pos1)|
			self.1.parse(input, pos1).and_then(|(out2, pos2)|
				self.2.parse(input, pos2).and_then(|(out3, pos3)|
					self.3.parse(input, pos3).map(|(out4, pos4)|
						((out1, out2, out3, out4), pos4)
					)
				)
			)
		)
	}
}

// pub struct Chain<'a, I: 'a, O, U, P: Parser<'a, I, O>, F: Fn(O) -> impl Parser<'a, I, U>>(P, F, PhantomData<&'a I>, PhantomData<O>);

// /// Chain two passers where the second parser depends on the first's result.
// impl<'a, I: Copy + 'a, O, U, F: Fn(O) -> Parser<'a, I, U> + 'b> Shr<F> for Parser<'a, I, O> {
// 	type Output = Parser<'b, I, U>;

// 	fn shr(self, other: F) -> Self::Output {
// 		Parser::new(move |input: &mut Input<I>| {
// 			let start = input.position();
// 			let result = self.parse(input).and_then(|out1| other(out1).parse(input));
// 			if result.is_err() {
// 				input.jump_to(start);
// 			}
// 			result
// 		})
// 	}
// }

/// Choice from two heterogeneous alternatives.
pub struct Alt<P1, P2>(pub P1, pub P2);
impl<'a, I, O, P1: Parser<'a, I, O>, P2: Parser<'a, I, O>> Parser<'a, I, O> for Alt<P1, P2> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self.0.parse(input, start).or_else(
			|_| self.1.parse(input, start)
		)
	}
}

pub struct Alt3<P1, P2, P3>(pub P1, pub P2, pub P3);
impl<'a, I, O, P1: Parser<'a, I, O>, P2: Parser<'a, I, O>, P3: Parser<'a, I, O>> Parser<'a, I, O> for Alt3<P1, P2, P3> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self.0.parse(input, start).or_else(
			|_| self.1.parse(input, start).or_else(
				|_| self.2.parse(input, start)
			)
		)
	}
}

pub struct Alt4<P1, P2, P3, P4>(pub P1, pub P2, pub P3, pub P4);
impl<'a, I, O, P1: Parser<'a, I, O>, P2: Parser<'a, I, O>, P3: Parser<'a, I, O>, P4: Parser<'a, I, O>> Parser<'a, I, O> for Alt4<P1, P2, P3, P4> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self.0.parse(input, start).or_else(
			|_| self.1.parse(input, start).or_else(
				|_| self.2.parse(input, start).or_else(
					|_| self.3.parse(input, start)
				)
			)
		)
	}
}

/// Ordered choice
impl<'a, I, O, P: Parser<'a, I, O>> Parser<'a, I, O> for [P; 2] {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self[0].parse(input, start).or_else(
			|_| self[1].parse(input, start)
		)
	}
}

macro_rules! impl_ordered_choice {
	($n:expr) => {
		impl<'a, I, O, P: Parser<'a, I, O>> Parser<'a, I, O> for [P; $n] {
			fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
				for i in 0..$n {
					if let res @ Ok(_) = self[i].parse(input, start) {
						return res;
					}
				}
				return Err(Error::Mismatch{
					message: format!("Parsing failed for all {} alternatives", $n),
					position: start,
				});
			}
		}
	};
}

impl<'a, I: Copy + PartialEq + Debug + 'a> Parser<'a, I, &'a [I]> for &'a [I] {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(&'a [I], usize)> {
		seq(self).parse(input, start)
	}
}

macro_rules! impl_parser_for_array
{
	($n:expr) => {
		impl<'a> Parser<'a, u8, &'a [u8]> for &'a [u8; $n] {
			fn parse(&self, input: &'a [u8], start: usize) -> Result<(&'a [u8], usize)> {
				seq(&self[..]).parse(input, start)
			}
		}
	};
}

impl_parser_for_array!(0);
impl_parser_for_array!(1);
impl_parser_for_array!(2);
impl_parser_for_array!(3);
impl_parser_for_array!(4);
impl_parser_for_array!(5);
impl_parser_for_array!(6);
impl_parser_for_array!(7);
impl_parser_for_array!(8);
impl_parser_for_array!(9);

impl_ordered_choice!(3);
impl_ordered_choice!(4);
impl_ordered_choice!(5);
impl_ordered_choice!(6);
impl_ordered_choice!(7);
impl_ordered_choice!(8);
impl_ordered_choice!(9);

// /// And predicate
// impl<'a, I: Copy + 'static, O: 'static> Neg for Parser<'a, I, O> {
// 	type Output = Parser<'a, I, bool>;

// 	fn neg(self) -> Self::Output {
// 		Parser::new(move |input: &mut Input<I>| {
// 			let start = input.position();
// 			let result = self.parse(input);
// 			input.jump_to(start);
// 			result.map(|_| true)
// 		})
// 	}
// }

// /// Not predicate
// impl<'a, I: Copy + 'static, O: 'static> Not for Parser<'a, I, O> {
// 	type Output = Parser<'a, I, bool>;

// 	fn not(self) -> Self::Output {
// 		Parser::new(move |input: &mut Input<I>| {
// 			let start = input.position();
// 			let result = self.parse(input);
// 			input.jump_to(start);
// 			match result {
// 				Ok(_) => Err(Error::Mismatch{
// 					message: "not predicate failed".to_string(),
// 					position: start,
// 				}),
// 				Err(_) => Ok(true),
// 			}
// 		})
// 	}
// }

#[cfg(test)]
mod tests {
	use ::parser::*;

	#[test]
	fn byte_works() {
		let input = b"abcde";
		let parser = (b'a', one_of(b"ab"), sym(b'C'));
		let output = parser.parse(input, 0);
		assert_eq!(output, Err(Error::Mismatch{message: "expect: 67, found: 99".to_string(), position: 2}));

		let parser = (b'a', none_of(b"AB"), b'c', b"de");
		let output = parser.parse(input, 0);
		assert_eq!(output, Ok( ((b'a', b'b', b'c', &b"de"[..]), 5)) );
		assert_eq!(end().parse(input, 5), Ok( ((), 5) ) );

		let parser = [b'e', b'd', b'a'];
		let output = parser.parse(input, 0);
		assert_eq!(output, Ok((b'a', 1)));
	}

	#[test]
	fn char_works() {
		let input = "abcd".chars().collect::<Vec<char>>();
		let parser = ("ab", map(['c', 'd'], |_| '0'));
		let output = parser.parse(&input, 0);
		assert_eq!(output, Ok((("ab", '0'), 3)));
	}

	#[test]
	fn recursive_parser() {
		#[derive(Debug, PartialEq)]
		enum Expr{
			Empty,
			Group(Box<Expr>)
		}
		fn expr<'a>(input: &'a [u8], start: usize) -> Result<(Expr, usize)> {
			Alt(
				map((b'(', expr, b')'), |(_, e, _)|Expr::Group(Box::new(e))),
				map(empty(), |_|Expr::Empty)
			).parse(input, start)
		}
		let input = b"(())";
		let parser = expr;
		let output = parser.parse(input, 0);
		assert_eq!(output, Ok( (Expr::Group(Box::new(Expr::Group(Box::new(Expr::Empty)))), 4)) );
	}

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
