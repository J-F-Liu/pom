use std::cmp::{PartialEq, PartialOrd};
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::str;

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

impl<T: PartialOrd + Copy> Set<T> for RangeTo<T> {
	fn contains(&self, elem: &T) -> bool {
		self.end > *elem
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
