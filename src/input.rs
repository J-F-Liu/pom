/// Parser input is generic over terminal type, which is usually u8 or char.
pub trait Input<T> where T: Copy {
	/// Get current position.
	fn position(&self) -> usize;

	/// Peek current symbol.
	fn current(&self) -> Option<T>;

	/// Advance to next symbol.
	fn advance(&mut self);

	/// Backward to specified position.
	fn backward(&mut self, position: usize);

	/// Get a segment from the input.
	fn segment(&self, start: usize, end: usize) -> Vec<T>;
}

pub struct DataInput<'a, T: 'a> {
	pub data: &'a [T],
	pub position: usize,
}

impl<'a, T: Copy> DataInput<'a, T> {
	pub fn new(input: &'a [T]) -> DataInput<T> {
		DataInput {
			data: input,
			position: 0,
		}
	}
}

impl<'a, T: Copy> Input<T> for DataInput<'a, T> {
	fn position(&self) -> usize {
		self.position
	}

	fn current(&self) -> Option<T>
	{
		if self.position < self.data.len() {
			Some(self.data[self.position])
		} else {
			None
		}
	}

	fn advance(&mut self) {
		self.position += 1;
	}

	fn backward(&mut self, position: usize) {
		self.position = position;
	}

	fn segment(&self, start: usize, end: usize) -> Vec<T> {
		self.data[start..end].to_vec()
	}
}
