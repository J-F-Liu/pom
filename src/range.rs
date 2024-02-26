use std::ops::{Range, RangeFrom, RangeFull, RangeTo, RangeToInclusive};

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

impl<T> RangeArgument<T> for RangeFull {
	fn start(&self) -> Bound<T> {
		Unbounded
	}
	fn end(&self) -> Bound<T> {
		Unbounded
	}
}

// impl<T: Clone> RangeArgument<T> for RangeInclusive<T> {
//     fn start(&self) -> Bound<T> {
// 		let (start, _) = self.clone().into_inner();
//         Included(&start.to_owned())
//     }
//     fn end(&self) -> Bound<T> {
// 		let (_, end) = self.clone().into_inner();
//         Included(&end.to_owned())
//     }
// }

impl<T> RangeArgument<T> for RangeTo<T> {
	fn start(&self) -> Bound<T> {
		Unbounded
	}
	fn end(&self) -> Bound<T> {
		Excluded(&self.end)
	}
}

impl<T> RangeArgument<T> for RangeToInclusive<T> {
	fn start(&self) -> Bound<T> {
		Unbounded
	}
	fn end(&self) -> Bound<T> {
		Included(&self.end)
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
