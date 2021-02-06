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

macro_rules! impl_set_for_array {
	( $($n:expr),+ ) => {
		$(
			impl Set<u8> for [u8; $n] {
				fn contains(&self, elem: &u8) -> bool {
					(self as &[u8]).contains(elem)
				}

				fn to_str(&self) -> &str {
					str::from_utf8(self).unwrap_or("<byte array>")
				}
			}
		)+
	};
}

impl_set_for_array!(
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
	26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
	50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64
);
