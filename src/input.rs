/// Parser input is generic over terminal type, which is usually u8 or char.
pub trait Input<T> where T: Copy {
	/// Get current position.
	fn position(&self) -> usize;

	/// Peek current symbol.
	fn current(&self) -> Option<T>;

	/// Advance to next symbol.
	fn advance(&mut self);

	/// Jump to specified position.
	fn jump_to(&mut self, position: usize);

	/// Get a segment from the input.
	fn segment(&self, start: usize, end: usize) -> Vec<T>;
}

/// Wrap &[u8] or &[char] as input to parser.
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

	fn current(&self) -> Option<T> {
		self.data.get(self.position).cloned()
	}

	fn advance(&mut self) {
		self.position += 1;
	}

	fn jump_to(&mut self, position: usize) {
		self.position = position;
	}

	fn segment(&self, start: usize, end: usize) -> Vec<T> {
		self.data[start..end].to_vec()
	}
}

/// Wrap &str as input to parser.
pub struct TextInput<'a> {
	pub text: &'a str,
	pub position: usize,
}

impl<'a> TextInput<'a> {
	pub fn new(input: &'a str) -> TextInput<'a> {
		TextInput {
			text: input,
			position: 0,
		}
	}
}

impl<'a> Input<char> for TextInput<'a> {
	fn position(&self) -> usize {
		self.position
	}

	fn current(&self) -> Option<char>
	{
		self.text[self.position..].chars().next()
	}

	fn advance(&mut self) {
		if let Some(c) = self.text[self.position..].chars().next() {
			self.position += c.len_utf8();
		}
	}

	fn jump_to(&mut self, position: usize) {
		self.position = position;
	}

	fn segment(&self, start: usize, end: usize) -> Vec<char> {
		self.text[start..end].chars().collect()
	}
}
