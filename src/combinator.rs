use std::fmt::{Display, Debug};
use std::marker::PhantomData;
use std::ops::{Add, Sub, Mul, Shr, BitOr, Neg, Not};
use super::{Result, Error};
use parser::Parser;
use range::RangeArgument;
use range::Bound::*;

pub struct Combinator<Parser>(Parser);

impl<P> Combinator<P> {
	/// Parse input with wrapped parser.
	pub fn parse<'a, I: 'a, O>(&self, input: &'a [I]) -> Result<O>
		where P: Parser<'a, I, O>
	{
		self.0.parse(input, 0).map(|(out, _)|out)
	}

	/// Convert parser result to desired value.
	pub fn map<'a, I: 'a, O, U, F: Fn(O) -> U>(self, f: F) -> Combinator<impl Parser<'a, I, U>>
		where P: Parser<'a, I, O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).map(|(out, pos)|(f(out), pos))
		})
	}

	/// Convert parser result to desired value, fail in case of conversion error.
	pub fn convert<'a, I: 'a, O, U, E, F>(self, f: F) -> Combinator<impl Parser<'a, I, U>>
		where P: Parser<'a, I, O>,
			  F: Fn(O) -> ::std::result::Result<U, E>,
			  E: Debug
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).and_then(|(res, pos)|{
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
		})
	}

	/// Get input position after matching parser.
	pub fn pos<'a, I: 'a, O>(self) -> Combinator<impl Parser<'a, I, usize>>
		where P: Parser<'a, I, O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).map(|(_, pos)|(pos, pos))
		})
	}

	/// Collect all matched input symbols.
	pub fn collect<'a, I: 'a, O>(self) -> Combinator<impl Parser<'a, I, &'a [I]>>
		where P: Parser<'a, I, O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).map(|(_, end)|(&input[start..end], end))
		})
	}

	/// Discard parser output.
	pub fn discard<'a, I: 'a, O>(self) -> Combinator<impl Parser<'a, I, ()>>
		where P: Parser<'a, I, O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).map(|(_, end)|((), end))
		})
	}

	/// Make parser optional.
	pub fn opt<'a, I: 'a, O>(self) -> Combinator<impl Parser<'a, I, Option<O>>>
		where P: Parser<'a, I, O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			match self.0.parse(input, start) {
				Ok((out, pos)) => Ok((Some(out), pos)),
				Err(_) => Ok((None, start)),
			}
		})
	}

	/// `p.repeat(5)` repeat p exactly 5 times
	/// `p.repeat(0..)` repeat p zero or more times
	/// `p.repeat(1..)` repeat p one or more times
	/// `p.repeat(1..4)` match p at least 1 and at most 3 times
	pub fn repeat<'a, I: 'a, O, R>(self, range: R) -> Combinator<impl Parser<'a, I, Vec<O>>>
		where P: Parser<'a, I, O>,
			  R: RangeArgument<usize> + Debug + 'a
	{
		Combinator(move |input: &'a [I], start_pos: usize| {
			let mut items = vec![];
			let mut pos = start_pos;
			loop {
				match range.end() {
					Included(&end) => if items.len() >= end { break; },
					Excluded(&end) => if items.len() + 1 >= end { break; },
					Unbounded => (),
				}

				if let Ok((item, item_pos)) = self.0.parse(input, pos) {
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
		})
	}

	/// Give parser a name to identify parsing errors.
	pub fn name<'a, I: 'a, O>(self, name: &'a str) -> Combinator<impl Parser<'a, I, O>>
		where P: Parser<'a, I, O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			match self.0.parse(input, start) {
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
		})
	}
}

/// Always succeeds, consume no input.
pub fn empty<'a, I: 'a>() -> Combinator<impl Parser<'a, I, ()>> {
	Combinator(|_: &[I], start: usize| Ok(((), start)))
}

/// Success when current input symbol equals `t`.
pub fn sym<'a, I: 'a>(t: I) -> Combinator<impl Parser<'a, I, I>>
	where I: Copy + PartialEq + Display
{
	Combinator(move |input: &'a [I], start: usize| {
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
	})
}

/// Success when sequence of symbols matches current input.
pub fn seq<'a, I: 'a>(tag: &'a [I]) -> Combinator<impl Parser<'a, I, &'a [I]>>
	where I: Copy + PartialEq + Debug
{
	Combinator(move |input: &'a [I], start: usize| {
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
	})
}

/// Parse separated list.
pub fn list<'a, I: 'a, O, U, P, S>(combinator: Combinator<P>, separator: Combinator<S>) -> Combinator<impl Parser<'a, I, Vec<O>>>
	where I: Copy,
		  P: Parser<'a, I, O>,
		  S: Parser<'a, I, U>
{
	Combinator(move |input: &'a [I], start: usize| {
		let mut items = vec![];
		let mut pos = start;
		if let Ok((first_item, first_pos)) = combinator.0.parse(input, pos) {
			items.push(first_item);
			pos = first_pos;
			while let Ok((_, sep_pos)) = separator.0.parse(input, pos) {
				match combinator.0.parse(input, sep_pos) {
					Ok((more_item, more_pos)) => {
						items.push(more_item);
						pos = more_pos;
					},
					Err(_) => break,
				}
			}
		}
		return Ok((items, pos));
	})
}

/// Success when current input symbol is one of the set.
pub fn one_of<'a, I: 'a>(set: &'a [I]) -> Combinator<impl Parser<'a, I, I>>
	where I: Copy + PartialEq + Debug
{
	Combinator(move |input: &'a [I], start: usize| {
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
	})
}

/// Success when current input symbol is none of the set.
pub fn none_of<'a, I: 'a>(set: &'a [I]) -> Combinator<impl Parser<'a, I, I>>
	where I: Copy + PartialEq + Debug
{
	Combinator(move |input: &'a [I], start: usize| {
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
	})
}

/// Success when predicate returns true on current input symbol.
pub fn is_a<'a, I: 'a, F>(predicate: F) -> Combinator<impl Parser<'a, I, I>>
	where I: Copy + PartialEq + Debug,
		  F: Fn(I) -> bool
{
	Combinator(move |input: &'a [I], start: usize| {
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
	})
}

/// Success when predicate returns false on current input symbol.
pub fn not_a<'a, I: 'a, F>(predicate: F) -> Combinator<impl Parser<'a, I, I>>
	where I: Copy + PartialEq + Debug,
		  F: Fn(I) -> bool
{
	Combinator(move |input: &'a [I], start: usize| {
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
	})
}

/// Success when the range contains current input symbol.
pub fn range<'a, I: 'a, R>(set: R) -> Combinator<impl Parser<'a, I, I>>
	where I: Copy + PartialOrd<I> + Display + Debug,
		  R: RangeArgument<I> + Debug
{
	Combinator(move |input: &'a [I], pos: usize| {
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
	})
}

/// Read n symbols.
pub fn take<'a, I: 'a>(n: usize) -> Combinator<impl Parser<'a, I, &'a [I]>> {
	Combinator(move |input: &'a [I], start: usize| {
		if input.len() >= n {
			let pos = start + n;
			Ok((&input[start..pos], pos))
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Skip n symbols.
pub fn skip<'a, I: 'a>(n: usize) -> Combinator<impl Parser<'a, I, ()>>
	where I: Copy + 'static
{
	Combinator(move |input: &'a [I], start: usize| {
		if input.len() >= n {
			Ok(((), start + n))
		} else {
			Err(Error::Incomplete)
		}
	})
}

/// Call a combinator factory, can be used to create recursive parsers.
pub fn call<'a, I: 'a, O, F, P>(factory: F) -> Combinator<impl Parser<'a, I, O>>
	where F: Fn() -> Combinator<P>, P: Parser<'a, I, O>
{
	Combinator(move |input: &'a [I], start: usize| {
		let combinator = factory();
		combinator.0.parse(input, start)
	})
}

/// Success when end of input is reached.
pub fn end<'a, I: Debug + 'a>() -> Combinator<impl Parser<'a, I, ()>> {
	Combinator(|input: &'a [I], start: usize| {
		if let Some(s) = input.get(start) {
			Err(Error::Mismatch{
				message: format!("expect end of input, found: {:?}", s),
				position: start,
			})
		} else {
			Ok(((), start))
		}
	})
}

/// Wrap parser as a combinator.
pub fn comb<P>(parser: P) -> Combinator<P> {
	Combinator(parser)
}

pub struct Both<P1, P2>(P1, P2);
impl<'a, I, O1, O2, P1: Parser<'a, I, O1>, P2: Parser<'a, I, O2>> Parser<'a, I, (O1, O2)> for Both<P1, P2> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<((O1, O2), usize)> {
		self.0.parse(input, start).and_then(|(out1, pos1)|
			self.1.parse(input, pos1).map(|(out2, pos2)|
				((out1, out2), pos2)
			)
		)
	}
}

pub struct Left<P1, P2, O2>(P1, P2, PhantomData<O2>);
impl<'a, I, O1, O2, P1: Parser<'a, I, O1>, P2: Parser<'a, I, O2>> Parser<'a, I, O1> for Left<P1, P2, O2> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O1, usize)> {
		self.0.parse(input, start).and_then(|(out1, pos1)|
			self.1.parse(input, pos1).map(|(_, pos2)|
				(out1, pos2)
			)
		)
	}
}

pub struct Right<P1, P2, O1>(P1, P2, PhantomData<O1>);
impl<'a, I, O1, O2, P1: Parser<'a, I, O1>, P2: Parser<'a, I, O2>> Parser<'a, I, O2> for Right<P1, P2, O1> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O2, usize)> {
		self.0.parse(input, start).and_then(|(_, pos1)|
			self.1.parse(input, pos1)
		)
	}
}

pub struct Chain<P, F, O>(P, F, PhantomData<O>);
impl<'a, I, O, P: Parser<'a, I, O>, U, Q: Parser<'a, I, U>, F: Fn(O) -> Combinator<Q>> Parser<'a, I, U> for Chain<P, F, O> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(U, usize)> {
		self.0.parse(input, start).and_then(|(out, pos)|
			self.1(out).0.parse(input, pos)
		)
	}
}

pub struct Alt<P1, P2>(P1, P2);
impl<'a, I, O, P1: Parser<'a, I, O>, P2: Parser<'a, I, O>> Parser<'a, I, O> for Alt<P1, P2> {
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self.0.parse(input, start).or_else(
			|_| self.1.parse(input, start)
		)
	}
}

/// Sequence reserve value
impl<P, Q> Add<Combinator<Q>> for Combinator<P> {
	type Output = Combinator<Both<P, Q>>;

	fn add(self, other: Combinator<Q>) -> Self::Output {
		Combinator(Both(self.0, other.0))
	}
}

/// Sequence discard second value
// impl<P, Q, O> Sub<Combinator<Q>> for Combinator<P>
// {
// 	type Output = Combinator<Left<P,Q,O>>;

// 	fn sub(self, other: Combinator<Q>) -> Self::Output {
// 		Combinator(Left(self.0, other.0, PhantomData))
// 	}
// }

// /// Chain two passers where the second parser depends on the first's result.
// impl<P, F, O> Shr<F> for Combinator<P> {
// 	type Output = Combinator<Chain<P, F, O>>;

// 	fn shr(self, other: F) -> Self::Output {
// 		Combinator(Chain(self.0, other, PhantomData))
// 	}
// }

/// Ordered choice
impl<P, Q> BitOr<Combinator<Q>> for Combinator<P> {
	type Output = Combinator<Alt<P,Q>>;

	fn bitor(self, other: Combinator<Q>) -> Self::Output {
		Combinator(Alt(self.0, other.0))
	}
}

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
