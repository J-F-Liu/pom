use std::cmp::{PartialEq, PartialOrd};
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::str;

/// Set relationship.
pub trait Set<T,I> {
	/// Whether a set contains an element or not.
	fn contains(&self, elem: &I) -> bool;

	/// Convert to text for display.
	fn to_str(&self) -> &str {
		"<set>"
	}
}

fn contains<T, I: PartialEq<T>, Iter: IntoIterator<Item=T>>(iter: Iter, elem: &I) -> bool {
	let mut ret = false;
	for v in iter {
		ret |= elem == &v;
	}
	ret
}

impl<T,I: PartialEq<T>> Set<T,I> for [T] {
	fn contains(&self, elem: &I) -> bool {
		contains(self, &elem)
	}
}

impl<I: PartialEq<char>> Set<char,I> for str {
	fn contains(&self, elem: &I) -> bool {
		let mut ret = false;
		for ref v in self.chars() {
			ret |= elem == v;
		}
		ret
	}

	fn to_str(&self) -> &str {
		self
	}
}

impl<T, I: PartialOrd<T>> Set<T,I> for Range<T> {
	fn contains(&self, elem: &I) -> bool {
		*elem >= self.start && *elem < self.end
	}
}

impl<T, I: PartialOrd<T>> Set<T,I> for RangeFrom<T> {
	fn contains(&self, elem: &I) -> bool {
		*elem >= self.start
	}
}

impl<T, I: PartialOrd<T>> Set<T,I> for RangeTo<T> {
	fn contains(&self, elem: &I) -> bool {
		*elem < self.end
	}
}

impl<T, I> Set<T, I> for RangeFull {
	fn contains(&self, _: &I) -> bool {
		true
	}

	fn to_str(&self) -> &str {
		".."
	}
}

macro_rules! impl_set_for_array {
	($n:expr) => {
		impl<I: PartialEq<u8>> Set<u8, I> for [u8; $n] {
			fn contains(&self, elem: &I) -> bool {
				let mut ret = false;
				for v in self {
					ret |= elem == v;
				}
				ret
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
