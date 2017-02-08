use std::fmt::{Display, Debug};
use std::ops::{Add, Sub, Mul, Shr, BitOr, Neg};
use super::{Result, Error};
use parser::Parser;
use range::RangeArgument;
use range::Bound::*;

pub struct Combinator<Parser>(pub Parser);

impl<P> Combinator<P> {
	/// Parse input with wrapped parser.
	pub fn parse<'a, I: 'a, O>(&self, input: &'a [I]) -> Result<O>
		where P: Parser<'a, I, Output=O>
	{
		self.0.parse(input, 0).map(|(out, _)|out)
	}

	/// Convert parser result to desired value.
	pub fn map<'a, I: 'a, O, U, F: Fn(O) -> U>(self, f: F) -> Combinator<impl Parser<'a, I, Output=U>>
		where P: Parser<'a, I, Output=O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).map(|(out, pos)|(f(out), pos))
		})
	}

	/// Convert parser result to desired value, fail in case of conversion error.
	pub fn convert<'a, I: 'a, O, U, E, F>(self, f: F) -> Combinator<impl Parser<'a, I, Output=U>>
		where P: Parser<'a, I, Output=O>,
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
	pub fn pos<'a, I: 'a, O>(self) -> Combinator<impl Parser<'a, I, Output=usize>>
		where P: Parser<'a, I, Output=O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).map(|(_, pos)|(pos, pos))
		})
	}

	/// Collect all matched input symbols.
	pub fn collect<'a, I: 'a, O>(self) -> Combinator<impl Parser<'a, I, Output=&'a [I]>>
		where P: Parser<'a, I, Output=O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).map(|(_, end)|(&input[start..end], end))
		})
	}

	/// Discard parser output.
	pub fn discard<'a, I: 'a, O>(self) -> Combinator<impl Parser<'a, I, Output=()>>
		where P: Parser<'a, I, Output=O>
	{
		Combinator(move |input: &'a [I], start: usize| {
			self.0.parse(input, start).map(|(_, end)|((), end))
		})
	}

	/// Make parser optional.
	pub fn opt<'a, I: 'a, O>(self) -> Combinator<impl Parser<'a, I, Output=Option<O>>>
		where P: Parser<'a, I, Output=O>
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
	pub fn repeat<'a, I: 'a, O, R>(self, range: R) -> Combinator<impl Parser<'a, I, Output=Vec<O>>>
		where P: Parser<'a, I, Output=O>,
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
	pub fn name<'a, I: 'a, O>(self, name: &'a str) -> Combinator<impl Parser<'a, I, Output=O>>
		where P: Parser<'a, I, Output=O>
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
pub fn empty<'a, I: 'a>() -> Combinator<impl Parser<'a, I, Output=()>> {
	Combinator(|_: &[I], start: usize| Ok(((), start)))
}

/// Success when current input symbol equals `t`.
pub fn sym<'a, I: 'a>(t: I) -> Combinator<impl Parser<'a, I, Output=I>>
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
pub fn seq<'a, I: 'a>(tag: &'a [I]) -> Combinator<impl Parser<'a, I, Output=&'a [I]>>
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
pub fn list<'a, I: 'a, O, U, P, S>(combinator: Combinator<P>, separator: Combinator<S>) -> Combinator<impl Parser<'a, I, Output=Vec<O>>>
	where I: Copy,
		  P: Parser<'a, I, Output=O>,
		  S: Parser<'a, I, Output=U>
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
pub fn one_of<'a, I: 'a>(set: &'a [I]) -> Combinator<impl Parser<'a, I, Output=I>>
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
pub fn none_of<'a, I: 'a>(set: &'a [I]) -> Combinator<impl Parser<'a, I, Output=I>>
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
pub fn is_a<'a, I: 'a, F>(predicate: F) -> Combinator<impl Parser<'a, I, Output=I>>
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
pub fn not_a<'a, I: 'a, F>(predicate: F) -> Combinator<impl Parser<'a, I, Output=I>>
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
pub fn range<'a, I: 'a, R>(set: R) -> Combinator<impl Parser<'a, I, Output=I>>
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
pub fn take<'a, I: 'a>(n: usize) -> Combinator<impl Parser<'a, I, Output=&'a [I]>> {
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
pub fn skip<'a, I: 'a>(n: usize) -> Combinator<impl Parser<'a, I, Output=()>>
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
pub fn call<'a, I: 'a, O, F, P>(factory: F) -> Combinator<impl Parser<'a, I, Output=O>>
	where F: Fn() -> Combinator<P>, P: Parser<'a, I, Output=O>
{
	Combinator(move |input: &'a [I], start: usize| {
		let combinator = factory();
		combinator.0.parse(input, start)
	})
}

/// Success when end of input is reached.
pub fn end<'a, I: Debug + 'a>() -> Combinator<impl Parser<'a, I, Output=()>> {
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

pub struct Pair<P1, P2>(P1, P2);
impl<'a, I, O1, O2, P1: Parser<'a, I, Output=O1>, P2: Parser<'a, I, Output=O2>> Parser<'a, I> for Pair<P1, P2> {
	type Output = (O1, O2);
	fn parse(&self, input: &'a [I], start: usize) -> Result<((O1, O2), usize)> {
		self.0.parse(input, start).and_then(|(out1, pos1)|
			self.1.parse(input, pos1).map(|(out2, pos2)|
				((out1, out2), pos2)
			)
		)
	}
}

/// Sequence return value pair.
impl<P, Q> Add<Combinator<Q>> for Combinator<P> {
	type Output = Combinator<Pair<P, Q>>;

	fn add(self, other: Combinator<Q>) -> Self::Output {
		Combinator(Pair(self.0, other.0))
	}
}

pub struct Left<P1, P2>(P1, P2);
impl<'a, I, O1, O2, P1: Parser<'a, I, Output=O1>, P2: Parser<'a, I, Output=O2>> Parser<'a, I> for Left<P1, P2> {
	type Output = O1;
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O1, usize)> {
		self.0.parse(input, start).and_then(|(out1, pos1)|
			self.1.parse(input, pos1).map(|(_, pos2)|
				(out1, pos2)
			)
		)
	}
}

/// Sequence return first value.
impl<P, Q> Sub<Combinator<Q>> for Combinator<P> {
	type Output = Combinator<Left<P,Q>>;

	fn sub(self, other: Combinator<Q>) -> Self::Output {
		Combinator(Left(self.0, other.0))
	}
}

pub struct Right<P1, P2>(P1, P2);
impl<'a, I, O1, O2, P1: Parser<'a, I, Output=O1>, P2: Parser<'a, I, Output=O2>> Parser<'a, I> for Right<P1, P2> {
	type Output = O2;
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O2, usize)> {
		self.0.parse(input, start).and_then(|(_, pos1)|
			self.1.parse(input, pos1)
		)
	}
}

/// Sequence return second value.
impl<P, Q> Mul<Combinator<Q>> for Combinator<P> {
	type Output = Combinator<Right<P,Q>>;

	fn mul(self, other: Combinator<Q>) -> Self::Output {
		Combinator(Right(self.0, other.0))
	}
}

pub struct Chain<P, F>(P, F);
impl<'a, I, O, P: Parser<'a, I, Output=O>, U, Q: Parser<'a, I, Output=U>, F: Fn(O) -> Combinator<Q>> Parser<'a, I> for Chain<P, F> {
	type Output = U;
	fn parse(&self, input: &'a [I], start: usize) -> Result<(U, usize)> {
		self.0.parse(input, start).and_then(|(out, pos)|
			self.1(out).0.parse(input, pos)
		)
	}
}

/// Chain two passers where the second parser depends on the first's result.
impl<P, F> Shr<F> for Combinator<P> {
	type Output = Combinator<Chain<P, F>>;

	fn shr(self, other: F) -> Self::Output {
		Combinator(Chain(self.0, other))
	}
}

pub struct Alt<P1, P2>(P1, P2);
impl<'a, I, O, P1: Parser<'a, I, Output=O>, P2: Parser<'a, I, Output=O>> Parser<'a, I> for Alt<P1, P2> {
	type Output = O;
	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
		self.0.parse(input, start).or_else(
			|_| self.1.parse(input, start)
		)
	}
}

/// Ordered choice
impl<P, Q> BitOr<Combinator<Q>> for Combinator<P> {
	type Output = Combinator<Alt<P,Q>>;

	fn bitor(self, other: Combinator<Q>) -> Self::Output {
		Combinator(Alt(self.0, other.0))
	}
}

pub struct And<P>(P);
impl<'a, I, O, P: Parser<'a, I, Output=O>> Parser<'a, I> for And<P> {
	type Output = bool;
	fn parse(&self, input: &'a [I], start: usize) -> Result<(bool, usize)> {
		self.0.parse(input, start).map(|_|(true, start))
	}
}

/// And predicate
impl<P> Neg for Combinator<P> {
	type Output = Combinator<And<Self>>;

	fn neg(self) -> Self::Output {
		Combinator(And(self))
	}
}

pub struct Not<P>(P);
impl<'a, I, O, P: Parser<'a, I, Output=O>> Parser<'a, I> for Not<P> {
	type Output = bool;
	fn parse(&self, input: &'a [I], start: usize) -> Result<(bool, usize)> {
		match self.0.parse(input, start) {
			Ok(_) => Err(Error::Mismatch{
				message: "not predicate failed".to_string(),
				position: start,
			}),
			Err(_) => Ok((true, start)),
		}
	}
}

/// Not predicate
impl<P> ::std::ops::Not for Combinator<P> {
	type Output = Combinator<Not<Self>>;

	fn not(self) -> Self::Output {
		Combinator(Not(self))
	}
}

// pub struct Call<'a, P>(Box<Fn() -> Combinator<P> + 'a>);
// impl<'a, I, O, P: Parser<'a, I, Output=O>> Parser<'a, I> for Call<'a, P> {
// 	type Output = O;
// 	fn parse(&self, input: &'a [I], start: usize) -> Result<(O, usize)> {
// 		let combinator = self.0();
// 		combinator.0.parse(input, start)
// 	}
// }

// /// Call a combinator factory, can be used to create recursive parsers.
// pub fn call<'a, I: 'a, O, F: 'a, P>(factory: F) -> Combinator<impl Parser<'a, I, Output=O>>
// 	where F: Fn() -> Combinator<P>, P: Parser<'a, I, Output=O>
// {
// 	Combinator(Call(Box::new(factory)))
// }

#[cfg(test)]
mod tests {
	use ::combinator::*;

	#[test]
	fn byte_works() {
		let input = b"abcde";
		let parser = sym(b'a') + one_of(b"ab") - sym(b'C');
		let output = parser.parse(input);
		assert_eq!(output, Err(Error::Mismatch{message: "expect: 67, found: 99".to_string(), position: 2}));

		let parser = sym(b'a') * none_of(b"AB") - sym(b'c') + seq(b"de");
		let output = parser.parse(input);
		assert_eq!(output, Ok( (b'b', &b"de"[..]) ));
		assert_eq!(parser.pos().parse(input), Ok( 5 ) );

		let parser = sym(b'e') | sym(b'd') | empty().map(|_| b'0');
		let output = parser.parse(input);
		assert_eq!(output, Ok(b'0'));
	}

	#[test]
	fn char_works() {
		let input = "abcd".chars().collect::<Vec<char>>();
		let parser = comb("ab") + sym('c')
				   | sym('d').map(|_| ("", '0'));
		let output = parser.parse(&input);
		assert_eq!(output, Ok(("ab", 'c')));
	}

	#[test]
	fn recursive_parser() {
		#[derive(Debug, PartialEq)]
		enum Expr{
			Empty,
			Group(Box<Expr>)
		}
		fn expr<'a>(input: &'a [u8], start: usize) -> Result<(Expr, usize)> {
			(sym(b'(') * comb(expr).map(|e|Expr::Group(Box::new(e))) - sym(b')')
			 | empty().map(|_|Expr::Empty)
			).0.parse(input, start)
		}
		let input = b"(())";
		let parser = comb(expr);
		let output = parser.parse(input);
		assert_eq!(output, Ok( Expr::Group(Box::new(Expr::Group(Box::new(Expr::Empty)))) ));
	}

	#[test]
	fn chain_parser() {
		let input = b"5oooooooo";
		{
			let parser = one_of(b"0123456789").map(|c|c - b'0') >> |n| take(n as usize) + sym(b'o').repeat(0..);
			assert_eq!(parser.parse(input), Ok( (&[b'o'; 5][..], vec![b'o'; 3]) ));
		}
		{
			let parser = skip(1) * take(3) >> |v:&[u8]| {
				take(v.len()+2).map(move |u|{
					(u.to_vec(), v.to_vec())
				})
			};
			assert_eq!(parser.parse(input), Ok( (vec![b'o'; 5], vec![b'o'; 3]) ));
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
			assert_eq!(output, Ok(vec![b'x';1]))
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
