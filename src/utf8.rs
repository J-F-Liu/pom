// Variants of parser functions specialized for matching UTF-8 strings and returning chars

use super::{Error, Result};
use super::parser;
use crate::range::RangeArgument;
use crate::set::Set;
use std::str;
use std::fmt::Debug;
use bstr::decode_utf8;
use std::ops::{BitOr, Mul};

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

// Functions delegating normal parser::Parser
// TODO: Fill out
// TODO: Create mixed-type versions that degrade to parser::Parser

/// Sequence discard first value
impl<'a, O: 'a, U: 'a> Mul<Parser<'a, U>> for Parser<'a, O> {
	type Output = Parser<'a, U>;

	fn mul(self, other: Parser<'a, U>) -> Self::Output {
		Parser(self.0 * other.0)
	}
}

/// Ordered choice
impl<'a, O: 'a> BitOr for Parser<'a, O> {
	type Output = Parser<'a, O>;

	fn bitor(self, other: Parser<'a, O>) -> Self::Output {
		Parser(self.0 | other.0)
	}
}
