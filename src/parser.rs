use super::{Error, Result, RollbackRecord, RollbackType};
use crate::range::Bound::*;
use crate::range::RangeArgument;
use crate::set::Set;
use std::fmt::{Debug, Display};
use std::ops::{Add, BitOr, Mul, Neg, Not, Shr, Sub};

/// The third item, `Option<Error>`, if some, explains why the consumption of the next terminal has failed.
/// This answers the question "Why didn't the parser continue?". For all fixed-length parsers, like `sym` and `seq`, it should always be none.
type ParseOutput<O> = (O, usize, Option<Error>);

type Parse<'a, I, O> = dyn Fn(&'a [I], usize) -> Result<ParseOutput<O>> + 'a;

fn merge_errors(name: String, pos: usize, err1: Error, err2: Error) -> Error {
	match (err1, err2) {
		(
			Error::Rollback {
				rollbacks: rollbacks1,
			},
			Error::Rollback {
				rollbacks: rollbacks2,
			},
		) => Error::Rollback {
			rollbacks: RollbackRecord {
				name,
				position: pos,
				typ: RollbackType::Fail,
				inner: None,
				previous_tracks: vec![rollbacks1, rollbacks2],
			},
		},
		// TODO: remove this case
		/*(err1 @ Error::Rollback { .. }, err2) => {
			println!(
				"merge_errors at {} losing rollback data: {:?} <<>> {:?}",
				name, err1, err2
			);
			err2
		}*/
		(_, err2) => err2,
	}
}

fn merge_continuation_errors(
	name: String,
	pos: usize,
	cont_err1: Option<Error>,
	cont_err2: Option<Error>,
) -> Option<Error> {
	match (cont_err1, cont_err2) {
		(
			Some(Error::Rollback {
				rollbacks: rollbacks1,
			}),
			Some(Error::Rollback {
				rollbacks: rollbacks2,
			}),
		) => Some(Error::Rollback {
			rollbacks: RollbackRecord {
				name,
				position: pos,
				typ: RollbackType::Stop,
				inner: None,
				previous_tracks: vec![rollbacks1, rollbacks2],
			},
		}),
		(
			Some(Error::Rollback {
				rollbacks: rollbacks1,
			}),
			None,
		) => Some(Error::Rollback {
			rollbacks: RollbackRecord {
				name,
				position: pos,
				typ: RollbackType::Stop,
				inner: None,
				previous_tracks: vec![rollbacks1],
			},
		}),
		// TODO: remove this case
		/*(cont_err1 @ Some(Error::Rollback { .. }), cont_err2) => {
			println!(
				"merge_continuation_errors at {} losing rollback data: {:?} <<>> {:?}",
				name, cont_err1, cont_err2
			);
			cont_err2
		}*/
		(_, cont_err2) => cont_err2,
	}
}

/// Parser combinator.
pub struct Parser<'a, I, O> {
	method: Box<Parse<'a, I, O>>,
}

impl<'a, I, O> Parser<'a, I, O> {
	/// Create new parser.
	pub fn new<P>(parse: P) -> Parser<'a, I, O>
	where
		P: Fn(&'a [I], usize) -> Result<ParseOutput<O>> + 'a,
	{
		Parser {
			method: Box::new(parse),
		}
	}

	/// Apply the parser to parse input.
	pub fn parse(&self, input: &'a [I]) -> Result<O> {
		self.parse_at(input, 0).map(|(out, _, _)| out)
	}

	/// Parse input at specified position.
	#[inline]
	pub fn parse_at(&self, input: &'a [I], start: usize) -> Result<ParseOutput<O>> {
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
			self.parse_at(input, start)
				.map(|(out, pos, cont_err)| (f(out), pos, cont_err))
		})
	}

	/// Convert parser result to desired value, fail in case of conversion error.
	pub fn convert<U, E, F>(self, f: F) -> Parser<'a, I, U>
	where
		F: Fn(O, &Option<Error>) -> ::std::result::Result<U, E> + 'a,
		E: Debug,
		O: 'a,
		U: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			self.parse_at(input, start)
				.and_then(|(res, pos, cont_err)| match f(res, &cont_err) {
					Ok(out) => Ok((out, pos, cont_err)),
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
				.or_insert_with(|| self.parse_at(input, start))
				.clone()
		})
	}

	pub fn debug_ok<S>(self, id: S) -> Parser<'a, I, O>
	where
		O: std::fmt::Debug + 'a,
		S: AsRef<str> + 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			let result = self.parse_at(input, start);
			if result.is_ok() {
				println!("[debug] {} at {}: {:?}", id.as_ref(), start, result);
			}
			result
		})
	}

	/// Get input position after matching parser.
	pub fn pos(self) -> Parser<'a, I, usize>
	where
		O: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			self.parse_at(input, start)
				.map(|(_, pos, cont_err)| (pos, pos, cont_err))
		})
	}

	/// Collect all matched input symbols.
	pub fn collect(self) -> Parser<'a, I, &'a [I]>
	where
		O: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			self.parse_at(input, start)
				.map(|(_, end, cont_err)| (&input[start..end], end, cont_err))
		})
	}

	/// Discard parser output.
	pub fn discard(self) -> Parser<'a, I, ()>
	where
		O: 'a,
	{
		Parser::new(move |input: &'a [I], start: usize| {
			self.parse_at(input, start)
				.map(|(_, end, cont_err)| ((), end, cont_err))
		})
	}

	/// Make parser optional.
	pub fn opt(self) -> Parser<'a, I, Option<O>>
	where
		O: 'a,
	{
		Parser::new(
			move |input: &'a [I], start: usize| match self.parse_at(input, start) {
				Ok((out, pos, cont_err)) => Ok((Some(out), pos, cont_err)),
				Err(err) => Ok((None, start, Some(err))),
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
			let mut cont_err = None;
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

				match self.parse_at(input, pos) {
					Ok((item, item_pos, item_cont_err)) => {
						items.push(item);
						pos = item_pos;
						cont_err = item_cont_err;
					}
					Err(err) => {
						cont_err = merge_continuation_errors(
							"<Parser::repeat>".into(),
							pos,
							cont_err,
							Some(err),
						);
						break;
					}
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
						continuation_error: cont_err.map(Box::new),
					});
				}
			}
			Ok((items, pos, cont_err))
		})
	}

	/// Give parser a name to identify parsing errors, and stops rollback tracing propagation.
	pub fn name(self, name: &'a str) -> Parser<'a, I, O>
	where
		O: 'a,
	{
		Parser::new(
			move |input: &'a [I], start: usize| match self.parse_at(input, start) {
				res @ Ok(_) => res,
				Err(err) => match err {
					// TODO: how to handle rollbacks?
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

	/// Trace parser with an identifier for rollback tracing.
	pub fn trace<S>(self, id: S) -> Parser<'a, I, O>
	where
		O: 'a,
		S: AsRef<str> + 'a,
	{
		Parser::new(
			move |input: &'a [I], start: usize| match self.parse_at(input, start) {
				res @ Ok((_, _, None)) => res,
				Ok((out, pos, Some(Error::Rollback { rollbacks }))) => Ok((
					out,
					pos,
					Some(Error::Rollback {
						rollbacks: RollbackRecord {
							name: id.as_ref().into(),
							position: pos,
							typ: RollbackType::Stop,
							inner: None,
							previous_tracks: vec![rollbacks],
						},
					}),
				)),
				Ok((out, pos, Some(cont_err))) => Ok((
					out,
					pos,
					Some(Error::Rollback {
						rollbacks: RollbackRecord {
							name: id.as_ref().into(),
							position: pos,
							typ: RollbackType::Stop,
							inner: Some(Box::new(cont_err)),
							previous_tracks: vec![],
						},
					}),
				)),
				Err(err) => match err {
					Error::Rollback { rollbacks } => Err(Error::Rollback {
						rollbacks: RollbackRecord {
							name: id.as_ref().into(),
							position: start,
							typ: RollbackType::Fail,
							inner: None,
							previous_tracks: vec![rollbacks],
						},
					}),
					_ => Err(Error::Rollback {
						rollbacks: RollbackRecord {
							name: id.as_ref().into(),
							position: start,
							typ: RollbackType::Fail,
							inner: Some(Box::new(err)),
							previous_tracks: vec![],
						},
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
			move |input: &'a [I], start: usize| match self.parse_at(input, start) {
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
	Parser::new(|_: &[I], start: usize| Ok(((), start, None)))
}

/// Match any symbol.
pub fn any<'a, I>() -> Parser<'a, I, I>
where
	I: Clone,
{
	Parser::new(|input: &[I], start: usize| {
		if let Some(s) = input.get(start) {
			Ok((s.clone(), start + 1, None))
		} else {
			Err(Error::Mismatch {
				message: "end of input reached".to_owned(),
				position: start,
				continuation_error: None,
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
				Ok((s.clone(), start + 1, None))
			} else {
				Err(Error::Mismatch {
					message: format!("expect: {}, found: {}", t, s),
					position: start,
					continuation_error: None,
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
				return Ok((tag, pos, None));
			}
			if let Some(s) = input.get(pos) {
				if tag[index] != *s {
					return Err(Error::Mismatch {
						message: format!("seq {:?} expect: {:?}, found: {:?}", tag, tag[index], s),
						position: pos,
						continuation_error: None,
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
						continuation_error: None,
					});
				}
			} else {
				return Err(Error::Incomplete);
			}
			pos += 1;
		}
		Ok((tag, pos, None))
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
		let mut cont_err = None;
		match parser.parse_at(input, pos) {
			Ok((first_item, first_pos, _)) => {
				items.push(first_item);
				pos = first_pos;
				loop {
					match separator.parse_at(input, pos) {
						Ok((_, sep_pos, _)) => match parser.parse_at(input, sep_pos) {
							Ok((more_item, more_pos, more_cont_err)) => {
								items.push(more_item);
								pos = more_pos;
								cont_err = more_cont_err;
							}
							Err(err) => {
								cont_err = merge_continuation_errors(
									"<list>".into(),
									pos,
									cont_err,
									Some(err),
								);
								break;
							}
						},
						Err(err) => {
							cont_err = merge_continuation_errors(
								"<list>".into(),
								pos,
								cont_err,
								Some(err),
							);
							break;
						}
					}
				}
			}
			Err(err) => {
				cont_err = Some(err);
			}
		}
		Ok((items, pos, cont_err))
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
				Ok((s.clone(), start + 1, None))
			} else {
				Err(Error::Mismatch {
					message: format!("expect one of: {}, found: {}", set.to_str(), s),
					position: start,
					continuation_error: None,
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
					continuation_error: None,
				})
			} else {
				Ok((s.clone(), start + 1, None))
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
				Ok((s.clone(), start + 1, None))
			} else {
				Err(Error::Mismatch {
					message: format!("is_a predicate failed on: {}", s),
					position: start,
					continuation_error: None,
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
					continuation_error: None,
				})
			} else {
				Ok((s.clone(), start + 1, None))
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
			Ok((&input[start..pos], pos, None))
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
			Ok(((), pos, None))
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
		parser.parse_at(input, start)
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
				continuation_error: None,
			})
		} else {
			Ok(((), start, None))
		}
	})
}

/// Sequence reserve value
impl<'a, I, O: 'a, U: 'a> Add<Parser<'a, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'a, I, (O, U)>;

	fn add(self, other: Parser<'a, I, U>) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			self.parse_at(input, start)
				.and_then(
					|(out1, pos1, cont_err1)| match other.parse_at(input, pos1) {
						Ok((out2, pos2, cont_err2)) => {
							let cont_err = if pos2 == pos1 {
								merge_continuation_errors(
									"<Add>".into(),
									pos2,
									cont_err1,
									cont_err2,
								)
							} else {
								cont_err2
							};
							Ok(((out1, out2), pos2, cont_err))
						}
						Err(err2) => Err(cont_err1.map_or(err2.clone(), |err1| {
							merge_errors("<Add>".into(), pos1, err1, err2)
						})),
					},
				)
		})
	}
}

/// Sequence discard second value
impl<'a, I, O: 'a, U: 'a> Sub<Parser<'a, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'a, I, O>;

	fn sub(self, other: Parser<'a, I, U>) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			self.parse_at(input, start)
				.and_then(
					|(out1, pos1, cont_err1)| match other.parse_at(input, pos1) {
						Ok((_, pos2, cont_err2)) => {
							let cont_err = if pos2 == pos1 {
								merge_continuation_errors(
									"<Sub>".into(),
									pos2,
									cont_err1,
									cont_err2,
								)
							} else {
								cont_err2
							};
							Ok((out1, pos2, cont_err))
						}
						Err(err2) => Err(cont_err1.map_or(err2.clone(), |err1| {
							merge_errors("<Sub>".into(), pos1, err1, err2)
						})),
					},
				)
		})
	}
}

/// Sequence discard first value
impl<'a, I: 'a, O: 'a, U: 'a> Mul<Parser<'a, I, U>> for Parser<'a, I, O> {
	type Output = Parser<'a, I, U>;

	fn mul(self, other: Parser<'a, I, U>) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			self.parse_at(input, start)
				.and_then(|(_, pos1, cont_err1)| match other.parse_at(input, pos1) {
					Ok((out2, pos2, cont_err2)) => {
						let cont_err = if pos2 == pos1 {
							merge_continuation_errors("<Mul>".into(), pos2, cont_err1, cont_err2)
						} else {
							cont_err2
						};
						Ok((out2, pos2, cont_err))
					}
					Err(err2) => Err(cont_err1.map_or(err2.clone(), |err1| {
						merge_errors("<Mul>".into(), pos1, err1, err2)
					})),
				})
		})
	}
}

/// Chain two parsers where the second parser depends on the first's result.
impl<'a, I, O: 'a, U: 'a, F: Fn(O) -> Parser<'a, I, U> + 'a> Shr<F> for Parser<'a, I, O> {
	type Output = Parser<'a, I, U>;

	fn shr(self, other: F) -> Self::Output {
		Parser::new(move |input: &'a [I], start: usize| {
			self.parse_at(input, start)
				.and_then(|(out1, pos1, cont_err1)| {
					other(out1)
						.parse_at(input, pos1)
						.map(|(out2, pos2, cont_err2)| {
							let cont_err = if pos2 == pos1 {
								merge_continuation_errors(
									"<Shr>".into(),
									pos2,
									cont_err1,
									cont_err2,
								)
							} else {
								cont_err2
							};
							(out2, pos2, cont_err)
						})
				})
		})
	}
}

/// Ordered choice
impl<'a, I, O: 'a> BitOr for Parser<'a, I, O> {
	type Output = Parser<'a, I, O>;

	fn bitor(self, other: Parser<'a, I, O>) -> Self::Output {
		Parser::new(
			move |input: &'a [I], start: usize| match self.parse_at(input, start) {
				Ok(out) => Ok(out),
				Err(err) => match err {
					Error::Expect { .. } => Err(err),
					_ => match other.parse_at(input, start) {
						Ok((out, pos, cont_err2)) => {
							let cont_err = if pos == start {
								merge_continuation_errors(
									"<Bitor>".into(),
									pos,
									Some(err),
									cont_err2,
								)
							} else {
								cont_err2
							};
							Ok((out, pos, cont_err))
						}
						Err(err2) => Err(merge_errors("<Bitor>".into(), start, err, err2)),
					},
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
			self.parse_at(input, start).map(|_| (true, start, None))
		})
	}
}

/// Not predicate
impl<'a, I, O: 'a> Not for Parser<'a, I, O> {
	type Output = Parser<'a, I, bool>;

	fn not(self) -> Self::Output {
		Parser::new(
			move |input: &'a [I], start: usize| match self.parse_at(input, start) {
				Ok(_) => Err(Error::Mismatch {
					message: "not predicate failed".to_string(),
					position: start,
					continuation_error: None,
				}),
				Err(_) => Ok((true, start, None)),
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
				position: 2,
				continuation_error: None,
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
					position: 0,
					continuation_error: None,
				})
			})
		);
	}

	#[test]
	fn char_works() {
		let input = "abcd".chars().collect::<Vec<char>>();
		let parser = (tag("ab") + sym('c')) | sym('d').map(|_| ("", '0'));
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
				(skip(1) * take(3)) >> |v: &'static [u8]| take(v.len() + 2).map(move |u| (u, v));
			assert_eq!(parser.parse(input), Ok((&b"ooooo"[..], &b"ooo"[..])));
		}
		{
			let parser = Parser::new(move |input, start| {
				(skip(1) * take(3))
					.parse_at(input, start)
					.and_then(|(v, pos, _)| {
						take(v.len() + 2)
							.parse_at(input, pos)
							.map(|(u, end, _)| ((u, v), end, None))
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
