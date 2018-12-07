use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

pub enum Bound<'a, T: 'a> {
	Excluded(&'a T),
	Included(&'a T),
	Unbounded,
}
use self::Bound::*;

pub trait RangeArgument<T> {
	fn start(&self) -> Bound<T>;
	fn end(&self) -> Bound<T>;
}

impl<T> RangeArgument<T> for Range<T> {
	fn start(&self) -> Bound<T> {
		Included(&self.start)
	}
	fn end(&self) -> Bound<T> {
		Excluded(&self.end)
	}
}

impl<T> RangeArgument<T> for RangeFrom<T> {
	fn start(&self) -> Bound<T> {
		Included(&self.start)
	}
	fn end(&self) -> Bound<T> {
		Unbounded
	}
}

impl<T> RangeArgument<T> for RangeTo<T> {
	fn start(&self) -> Bound<T> {
		Unbounded
	}
	fn end(&self) -> Bound<T> {
		Excluded(&self.end)
	}
}

impl<T> RangeArgument<T> for RangeFull {
	fn start(&self) -> Bound<T> {
		Unbounded
	}
	fn end(&self) -> Bound<T> {
		Unbounded
	}
}

impl RangeArgument<usize> for usize {
	fn start(&self) -> Bound<usize> {
		Included(self)
	}
	fn end(&self) -> Bound<usize> {
		Included(self)
	}
}
