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
	($n:expr) => {
		impl Set<u8> for [u8; $n] {
			fn contains(&self, elem: &u8) -> bool {
				(self as &[u8]).contains(elem)
			}

			fn to_str(&self) -> &str {
				str::from_utf8(self).unwrap_or("<byte array>")
			}
		}
	};
}

impl_set_for_array!(0);
impl_set_for_array!(1);
impl_set_for_array!(2);
impl_set_for_array!(3);
impl_set_for_array!(4);
impl_set_for_array!(5);
impl_set_for_array!(6);
impl_set_for_array!(7);
impl_set_for_array!(8);
impl_set_for_array!(9);
impl_set_for_array!(10);
impl_set_for_array!(11);
impl_set_for_array!(12);
impl_set_for_array!(13);
impl_set_for_array!(14);
impl_set_for_array!(15);
impl_set_for_array!(16);
impl_set_for_array!(17);
impl_set_for_array!(18);
impl_set_for_array!(19);
impl_set_for_array!(20);
impl_set_for_array!(21);
impl_set_for_array!(22);
impl_set_for_array!(23);
impl_set_for_array!(24);
impl_set_for_array!(25);
impl_set_for_array!(26);
impl_set_for_array!(27);
impl_set_for_array!(28);
impl_set_for_array!(29);
impl_set_for_array!(30);
impl_set_for_array!(31);
impl_set_for_array!(32);
