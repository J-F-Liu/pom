use std::{
	cmp::{PartialEq, PartialOrd},
	ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
	str,
};

/// Set relationship.
pub trait Set<T> {
	/// Whether a set contains an element or not.
	fn contains(&self, elem: &T) -> bool;

	/// Convert to text for display.
	fn to_str(&self) -> &str {
		"<set>"
	}
}

impl<T: PartialEq> Set<T> for [T] {
	fn contains(&self, elem: &T) -> bool {
		(self as &[T]).contains(elem)
	}
}

impl Set<char> for str {
	fn contains(&self, elem: &char) -> bool {
		(self as &str).contains(*elem)
	}

	fn to_str(&self) -> &str {
		self
	}
}

impl<T: PartialOrd + Copy> Set<T> for Range<T> {
	fn contains(&self, elem: &T) -> bool {
		self.start <= *elem && self.end > *elem
	}
}

impl<T: PartialOrd + Copy> Set<T> for RangeFrom<T> {
	fn contains(&self, elem: &T) -> bool {
		self.start <= *elem
	}
}

impl<T: PartialOrd + Copy> Set<T> for RangeInclusive<T> {
	fn contains(&self, elem: &T) -> bool {
		self.start() <= elem && self.end() >= elem
	}
}

impl<T: PartialOrd + Copy> Set<T> for RangeTo<T> {
	fn contains(&self, elem: &T) -> bool {
		self.end > *elem
	}
}

impl<T: PartialOrd + Copy> Set<T> for RangeToInclusive<T> {
	fn contains(&self, elem: &T) -> bool {
		self.end >= *elem
	}
}

impl<T> Set<T> for RangeFull {
	fn contains(&self, _: &T) -> bool {
		true
	}

	fn to_str(&self) -> &str {
		".."
	}
}

impl<const N: usize> Set<u8> for [u8; N] {
	fn contains(&self, elem: &u8) -> bool {
		(self as &[u8]).contains(elem)
	}

	fn to_str(&self) -> &str {
		str::from_utf8(self).unwrap_or("<byte array>")
	}
}

#[cfg(test)]
mod test {
	use crate::parser::*;

	#[test]
	fn one_of_using_set() {
		assert!(one_of(b"az").parse(b"a").is_ok());
		assert!(one_of(b"az").parse(b"1").is_err());
	}

	#[test]
	fn one_of_using_range() {
		assert!(one_of(&(b'a'..b'z')).parse(b"a").is_ok());
		assert!(one_of(&(b'a'..b'z')).parse(b"z").is_err());
		assert!(one_of(&(b'a'..b'z')).parse(b"1").is_err());
	}

	#[test]
	fn one_of_using_range_to() {
		assert!(one_of(&(..b'z')).parse(b"a").is_ok());
		assert!(one_of(&(..b'z')).parse(b"z").is_err());
		assert!(one_of(&(..b'z')).parse(b"1").is_ok());
	}

	#[test]
	fn one_of_using_range_inclusive() {
		assert!(one_of(&(b'a'..=b'z')).parse(b"a").is_ok());
		assert!(one_of(&(b'a'..=b'z')).parse(b"z").is_ok());
		assert!(one_of(&(b'a'..=b'z')).parse(b"1").is_err());
	}

	#[test]
	fn one_of_using_range_to_inclusive() {
		assert!(one_of(&(..=b'z')).parse(b"a").is_ok());
		assert!(one_of(&(..=b'z')).parse(b"z").is_ok());
		assert!(one_of(&(..=b'z')).parse(b"1").is_ok());
	}

	#[test]
	fn one_of_using_full_range() {
		assert!(one_of(&(..)).parse(b"a").is_ok());
		assert!(one_of(&(..)).parse(b"z").is_ok());
		assert!(one_of(&(..)).parse(b"1").is_ok());
	}

}