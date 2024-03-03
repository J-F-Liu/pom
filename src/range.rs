use std::ops::{Bound, RangeBounds, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};

pub trait RangeArgument<T> {
	fn start(&self) -> Bound<&usize>;
	fn end(&self) -> Bound<&usize>;
}

impl<T> RangeArgument<T> for Range<usize> {
	fn start(&self) -> Bound<&usize> {
        self.start_bound()
	}
	fn end(&self) -> Bound<&usize> {
        self.end_bound()
	}
}

impl<T> RangeArgument<T> for RangeFrom<usize> {
	fn start(&self) -> Bound<&usize> {
        self.start_bound()
	}
	fn end(&self) -> Bound<&usize> {
        self.end_bound()
	}
}

impl<T> RangeArgument<T> for RangeFull {
	fn start(&self) -> Bound<&usize> {
        self.start_bound()
	}
	fn end(&self) -> Bound<&usize> {
        self.end_bound()
	}
}

impl<T> RangeArgument<T> for RangeInclusive<usize> {
    fn start(&self) -> Bound<&usize> {
        self.start_bound()
    }
    fn end(&self) -> Bound<&usize> {
        self.end_bound()
    }
}

impl<T> RangeArgument<T> for RangeTo<usize> {
	fn start(&self) -> Bound<&usize> {
        self.start_bound()
	}
	fn end(&self) -> Bound<&usize> {
        self.end_bound()
	}
}

impl<T> RangeArgument<T> for RangeToInclusive<usize> {
	fn start(&self) -> Bound<&usize> {
        self.start_bound()
	}
	fn end(&self) -> Bound<&usize> {
        self.end_bound()
	}
}

impl RangeArgument<usize> for usize { 
    fn start(&self) -> Bound<&usize> {
        Bound::Included(self)
    }
    fn end(&self) -> Bound<&usize> {
        Bound::Included(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn accept<T, R>(ra: R, expected: impl std::ops::RangeBounds<usize>)
    where
        R: RangeArgument<T>,
        T: std::fmt::Debug + std::cmp::PartialEq {
        assert_eq!(ra.start(), expected.start_bound());
        assert_eq!(ra.end(), expected.end_bound());
    }

    #[test]
    fn unbounded() {
        accept::<usize, _>(.., ..)
    }

    #[test]
    fn up_to_inclusive() {
        accept::<usize, _>(..=2, ..=2)
    }

    #[test]
    fn up_to_exclusive() {
        accept::<usize, _>(..2, ..2)
    }

    #[test]
    fn from() {
        accept::<usize, _>(1.., 1..)
    }

    #[test]
    fn from_to_inclusive() {
        accept::<usize, _>(1..=2, 1..=2)
    }

    #[test]
    fn from_to_exclusive() {
        accept::<usize, _>(1..3, 1..3)
    }

    #[test]
    fn exactly() {
        accept::<usize, _>(42, 42..=42)
    }
}
