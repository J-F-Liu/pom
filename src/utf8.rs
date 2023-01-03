// Variants of parser functions specialized for matching UTF-8 strings and returning chars

use super::{Error, Result};
use super::parser;
use crate::range::RangeArgument;
use crate::set::Set;
use std::str;
use std::fmt::Debug;
use bstr::decode_utf8;
use std::ops::{Add, BitOr, Mul, Neg, Not, Shr, Sub};

// / Parser combinator.
//type Parse<'a, O> = dyn Fn(&'a [u8], usize) -> Result<(O, usize)> + 'a;

/// Being wrapped in this struct guarantees that the parser within will only match valid UTF-8 strings.
pub struct Parser<'a, O> (parser::Parser<'a, u8, O>);

impl<'a, O> Parser<'a, O> {
	/// Create new parser.
	pub fn new<P>(parse: P) -> Parser<'a, O>
	where
		P: Fn(&'a [u8], usize) -> Result<(O, usize)> + 'a,
	{
		Parser( parser::Parser::new(parse) )
	}

	/// Collect all matched input symbols.
	// This method is the primary reason utf8::Parser exists at all.
	pub fn collect(self) -> Parser<'a, &'a str>
	where
		O: 'a,
	{
		Parser( self.0.collect().map(
			// UNSAFE: Because we only could have constructed this object from other utf8::Parser objects, the match space must be valid UTF-8
			|s| unsafe { str::from_utf8_unchecked(s) }
		) )
	}

	// Remaining methods in impl only delegate to base parser::Parser

	/// Apply the parser to parse input.
	pub fn parse(&self, input: &'a [u8]) -> Result<O> {
		self.0.parse(input)
	}

	/// Parse input at specified byte position.
	pub fn parse_at(&self, input: &'a [u8], start: usize) -> Result<(O, usize)> {
		self.0.parse_at(input, start)
	}

	/// Apply the parser to parse input.
	pub fn parse_str(&self, input: &'a str) -> Result<O> {
		self.0.parse(input.as_bytes())
	}

	/// Convert parser result to desired value.
	pub fn map<U, F>(self, f: F) -> Parser<'a, U>
	where
		F: Fn(O) -> U + 'a,
		O: 'a,
		U: 'a,
	{
		Parser( self.0.map(f) )
	}

	/// Convert parser result to desired value, fail in case of conversion error.
	pub fn convert<U, E, F>(self, f: F) -> Parser<'a, U>
	where
		F: Fn(O) -> ::std::result::Result<U, E> + 'a,
		E: Debug,
		O: 'a,
		U: 'a,
	{
		Parser( self.0.convert(f) )
	}

	/// Cache parser output result to speed up backtracking.
	pub fn cache(self) -> Parser<'a, O>
	where
		O: Clone + 'a,
	{
		Parser( self.0.cache() )
	}

	/// Get input position after matching parser.
	pub fn pos(self) -> Parser<'a, usize>
	where
		O: 'a,
	{
		Parser( self.0.pos() )
	}

	/// Discard parser output.
	pub fn discard(self) -> Parser<'a, ()>
	where
		O: 'a,
	{
		Parser( self.0.discard() )
	}

	/// Make parser optional.
	pub fn opt(self) -> Parser<'a, Option<O>>
	where
		O: 'a,
	{
		Parser( self.0.opt() )
	}

	/// `p.repeat(5)` repeat p exactly 5 times
	/// `p.repeat(0..)` repeat p zero or more times
	/// `p.repeat(1..)` repeat p one or more times
	/// `p.repeat(1..4)` match p at least 1 and at most 3 times
	pub fn repeat<R>(self, range: R) -> Parser<'a, Vec<O>>
	where
		R: RangeArgument<usize> + Debug + 'a,
		O: 'a,
	{
		Parser( self.0.repeat(range) )
	}

	/// Give parser a name to identify parsing errors.
	pub fn name(self, name: &'a str) -> Parser<'a, O>
	where
		O: 'a,
	{
		Parser( self.0.name(name) )
	}

	/// Mark parser as expected, abort early when failed in ordered choice.
	pub fn expect(self, name: &'a str) -> Parser<'a, O>
	where
		O: 'a,
	{
		Parser( self.0.expect(name) )
	}
}

impl<'a, O> From<Parser<'a, O>> for parser::Parser<'a, u8, O> {
    fn from(parser: Parser<'a, O>) -> Self {
        parser.0 // Simply unwrap
    }
}

// Helper for functions that decode_utf8 and fail
fn no_utf8<T>(start: usize, size: usize) -> Result<T> {
	if size == 0 {
		Err(Error::Mismatch {
			message: "end of input reached".to_owned(),
			position: start,
		})
	} else {
		Err(Error::Mismatch {
			message: "not UTF-8".to_owned(),
			position: start,
		})
	}
}

/// Match any UTF-8 character.
pub fn any<'a>() -> Parser<'a, char>
{
	Parser::new(|input: &[u8], start: usize| {
		let (ch, size) = decode_utf8(&input[start..]);

		if let Some(ch) = ch {
			let pos = start+size;

			Ok((ch, pos))
		} else {
			no_utf8(start, size)
		}
	})
}

/// Match specific UTF-8 character.
pub fn sym<'a>(tag: char) -> Parser<'a, char>
{
	Parser::new(move |input: &[u8], start: usize| {
		let (ch, size) = decode_utf8(&input[start..]);

		if let Some(ch) = ch {
			if ch == tag {
				let pos = start+size;

				Ok((ch, pos))
			} else {
				Err(Error::Mismatch {
					message: format!("expect: {}, found: {}", tag, ch),
					position: start,
				})
			}
		} else {
			no_utf8(start, size)
		}
	})
}

/// Success when sequence of chars matches current input.
pub fn seq<'a, 'b: 'a>(tag_str: &'b str) -> Parser<'a, &'a str>
{
	let tag = tag_str.as_bytes();
	Parser::new(move |input: &'a [u8], start: usize| {
		let mut index = 0;
		loop {
			let pos = start + index;
			if index == tag.len() {
				let result = &input[start..pos];
				// UNSAFE: Because slice is byte-identical to a str, it is known valid UTF-8
				let result_str = unsafe { str::from_utf8_unchecked(result) };
				return Ok((result_str, pos));
			}
			if let Some(s) = input.get(pos) {
				if tag[index] != *s {
					return Err(Error::Mismatch {
						message: format!("seq {:?} at byte index: {}", tag, pos),
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

/// Success when current input symbol is one of the set.
pub fn one_of<'a, S>(set: &'a S) -> Parser<'a, char>
where
	S: Set<char> + ?Sized,
{
	Parser::new(move |input: &'a [u8], start: usize| {
		let (ch, size) = decode_utf8(&input[start..]);

		if let Some(ch) = ch {
			if set.contains(&ch) {
				let pos = start+size;

				Ok((ch, pos))
			} else {
				Err(Error::Mismatch {
					message: format!("expect one of: {}, found: {}", set.to_str(), ch),
					position: start,
				})
			}
		} else {
			no_utf8(start, size)
		}
	})
}

/// Success when current input symbol is none of the set.
pub fn none_of<'a, S>(set: &'a S) -> Parser<'a, char>
where
	S: Set<char> + ?Sized,
{
	Parser::new(move |input: &'a [u8], start: usize| {
		let (ch, size) = decode_utf8(&input[start..]);

		if let Some(ch) = ch {
			if !set.contains(&ch) {
				let pos = start+size;

				Ok((ch, pos))
			} else {
				Err(Error::Mismatch {
					message: format!("expect one of: {}, found: {}", set.to_str(), ch),
					position: start,
				})
			}
		} else {
			no_utf8(start, size)
		}
	})
}

/// Success when predicate returns true on current input symbol.
pub fn is_a<'a, F>(predicate: F) -> Parser<'a, char>
where
	F: Fn(char) -> bool + 'a,
{
	Parser::new(move |input: &'a [u8], start: usize| {
		let (ch, size) = decode_utf8(&input[start..]);

		if let Some(ch) = ch {
			if predicate(ch) {
				let pos = start+size;

				Ok((ch, pos))
			} else {
				Err(Error::Mismatch {
					message: format!("is_a predicate failed on: {}", ch),
					position: start,
				})
			}
		} else {
			no_utf8(start, size)
		}
	})
}

/// Success when predicate returns false on current input symbol.
pub fn not_a<'a, F>(predicate: F) -> Parser<'a, char>
where
	F: Fn(char) -> bool + 'a,
{
	Parser::new(move |input: &'a [u8], start: usize| {
		let (ch, size) = decode_utf8(&input[start..]);

		if let Some(ch) = ch {
			if !predicate(ch) {
				let pos = start+size;

				Ok((ch, pos))
			} else {
				Err(Error::Mismatch {
					message: format!("is_a predicate failed on: {}", ch),
					position: start,
				})
			}
		} else {
			no_utf8(start, size)
		}
	})
}

/// Read n chars.
pub fn take<'a>(n: usize) -> Parser<'a, &'a str> {
	Parser::new(move |input: &'a [u8], start: usize| {
		let mut byte_pos = start;
		for _ in 0..n {
			let (ch, size) = decode_utf8(&input[start..]);
			if ch.is_none() {
				return no_utf8(byte_pos, size)
			}
			byte_pos += size;
		}
		let result = &input[start..byte_pos];
		// UNSAFE: Because every char has been checked by decode_utf8, this string is known utf8
		let result_str = unsafe { str::from_utf8_unchecked(result) };
		Ok((result_str, byte_pos))
	})
}

/// Skip n symbols.
pub fn skip<'a, I>(n: usize) -> Parser<'a, ()> {
	Parser::new(move |input: &'a [u8], start: usize| {
		let mut byte_pos = start;
		for _ in 0..n {
			let (ch, size) = decode_utf8(&input[start..]);
			if ch.is_none() {
				return no_utf8(byte_pos, size)
			}
			byte_pos += size;
		}
		Ok(((), byte_pos))
	})
}

/// Read n bytes exactly.
pub fn take_bytes<'a>(n: usize) -> Parser<'a, &'a str> {
	Parser::new(move |input: &'a [u8], start: usize| {
		// FIXME: This runs in linear time because it checks each character.
		// If we could remember which inputs were passed in from parse_str() instead of parse(),
		// we could assume the characters are valid utf8 and run this in constant time by only checking
		// the final character using bstr::decode_last_utf8.
		let mut byte_pos = start;
		loop {
			let (ch, size) = decode_utf8(&input[start..]);
			if ch.is_none() {
				return no_utf8(byte_pos, size)
			}
			byte_pos += size;
			if byte_pos > n {
				return Err(Error::Mismatch {
					message: "range splits a UTF-8 character".to_owned(),
					position: start,
				})
			}
			if byte_pos == n {
				let result = &input[start..byte_pos];
				// UNSAFE: Because every char has been checked by decode_utf8, this string is known utf8
				let result_str = unsafe { str::from_utf8_unchecked(result) };
				return Ok((result_str, byte_pos))
			}
		}		
	})
}

/// Skip n bytes exactly.
pub fn skip_bytes<'a>(n: usize) -> Parser<'a, ()> {
	Parser::new(move |input: &'a [u8], start: usize| {
		// FIXME: See note on take_bytes.
		let mut byte_pos = start;
		loop {
			let (ch, size) = decode_utf8(&input[start..]);
			if ch.is_none() {
				return no_utf8(byte_pos, size)
			}
			byte_pos += size;
			if byte_pos > n {
				return Err(Error::Mismatch {
					message: "range splits a UTF-8 character".to_owned(),
					position: start,
				})
			}
			if byte_pos == n {
				return Ok(((), byte_pos))
			}
		}
	})
}

/// Chain two parsers where the second parser depends on the first's result.
impl<'a, O: 'a, U: 'a, F: Fn(O) -> Parser<'a, U> + 'a> Shr<F> for Parser<'a, O> {
	type Output = Parser<'a, U>;

	fn shr(self, other: F) -> Self::Output {
		Parser::new(move |input: &'a [u8], start: usize| {
			(self.0.method)(input, start).and_then(|(out, pos)| (other(out).0.method)(input, pos))
		})
	}
}

// Note: There are no "degrade to parser::Parser" implementations for >>
// because Rust cannot tell the difference between an FN(O)->U and an FN(O)->V.

// Remaining functions in file only delegate to base parser::Parser

/// Always succeeds, consume no input.
pub fn empty<'a>() -> Parser<'a, ()> {
	Parser( parser::empty() )
}

/// Parse separated list.
pub fn list<'a, I, O, U>(
	item: Parser<'a, O>,
	separator: Parser<'a, U>,
) -> Parser<'a, Vec<O>>
where
	O: 'a,
	U: 'a,
{
	Parser( parser::list(item.0, separator.0) )
}

/// Call a parser factory, can be used to create recursive parsers.
pub fn call<'a, O, F>(parser_factory: F) -> Parser<'a, O>
where
	O: 'a,
	F: Fn() -> Parser<'a, O> + 'a,
{
	Parser( parser::call(move || parser_factory().0) )
}

/// Success when end of input is reached.
pub fn end<'a, I>() -> Parser<'a, ()>
{
	Parser( parser::end() )
}

// And, Sub and Mul are similar enough we can implement them with macros

macro_rules! utf_op {
    ( $impl_name:ident, $fn_name:ident, $op:tt, $return_type:ty, $doc:expr ) => {
    	#[doc=$doc]
		impl<'a, Left: 'a, Right: 'a> $impl_name<Parser<'a, Right>> for Parser<'a, Left> {
			type Output = Parser<'a, $return_type>;

			fn $fn_name (self, other: Parser<'a, Right>) -> Self::Output {
				Parser(self.0 $op other.0)
			}
		}
    };
}

macro_rules! utf_u8_op {
    ( $impl_name:ident, $fn_name:ident, $op:tt, $return_type:ty, $doc:expr ) => {
    	#[doc=concat!($doc, " (but degrade to non-utf8 parser)")]
		impl<'a, Left: 'a, Right: 'a> $impl_name<parser::Parser<'a, u8, Right>> for Parser<'a, Left> {
			type Output = parser::Parser<'a, u8, $return_type>;

			fn $fn_name (self, other: parser::Parser<'a, u8, Right>) -> Self::Output {
				self.0 $op other
			}
		}
    };
}

macro_rules! u8_utf_op {
    ( $impl_name:ident, $fn_name:ident, $op:tt, $return_type:ty, $doc:expr ) => {
    	#[doc=concat!($doc, " (but degrade to non-utf8 parser)")]
		impl<'a, Left: 'a, Right: 'a> $impl_name<Parser<'a, Right>> for parser::Parser<'a, u8, Left> {
			type Output = parser::Parser<'a, u8, $return_type>;

			fn $fn_name (self, other: Parser<'a, Right>) -> Self::Output {
				self $op other.0
			}
		}
    };
}

macro_rules! all_op {
    ( $impl_name:ident, $fn_name:ident, $op:tt, $return_type:ty, $doc:expr ) => {
    	utf_op!($impl_name, $fn_name, $op, $return_type, $doc);
    	utf_u8_op!($impl_name, $fn_name, $op, $return_type, $doc);
    	u8_utf_op!($impl_name, $fn_name, $op, $return_type, $doc);
    }
}

all_op!(Add, add, +, (Left, Right), "Sequence reserve value");

all_op!(Sub, sub, -, Left, "Sequence discard second value");

all_op!(Mul, mul, *, Right, "Sequence discard first value");

/// Ordered choice
impl<'a, O: 'a> BitOr for Parser<'a, O> {
	type Output = Parser<'a, O>;

	fn bitor(self, other: Parser<'a, O>) -> Self::Output {
		Parser(self.0 | other.0)
	}
}

/// Ordered choice (but degrade to non-utf8 parser)
impl<'a, O: 'a> BitOr<parser::Parser<'a, u8, O>> for Parser<'a, O> {
	type Output = parser::Parser<'a, u8, O>;

	fn bitor(self, other: parser::Parser<'a, u8, O>) -> Self::Output {
		self.0 | other
	}
}

/// Ordered choice (but degrade to non-utf8 parser)
impl<'a, O: 'a> BitOr<Parser<'a, O>> for parser::Parser<'a, u8, O> {
	type Output = parser::Parser<'a, u8, O>;

	fn bitor(self, other: Parser<'a, O>) -> Self::Output {
		self | other.0
	}
}

/// And predicate
impl<'a, O: 'a> Neg for Parser<'a, O> {
	type Output = Parser<'a, bool>;

	fn neg(self) -> Self::Output {
		Parser( -self.0 )
	}
}

/// Not predicate
impl<'a, O: 'a> Not for Parser<'a, O> {
	type Output = Parser<'a, bool>;

	fn not(self) -> Self::Output {
		Parser( !self.0 )
	}
}
