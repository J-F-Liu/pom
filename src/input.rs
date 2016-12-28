/// Parser input.
pub struct Input<'a, I: 'a> {
	pub data: &'a [I],
	pub position: usize,
}

impl<'a, I: 'a> Input<'a, I> {
	pub fn new(input: &'a [I]) -> Input<I> {
		Input {
			data: input,
			position: 0,
		}
	}

	pub fn current(&self) -> Option<I>
		where I: Copy + Clone + 'static
	{
		if self.position < self.data.len() {
			Some(self.data[self.position])
		} else {
			None
		}
	}

	pub fn advance(&mut self) {
		self.position += 1;
	}
}
