/// A train is a series of knots, each knot can be a terminal symbol.
/// Train is used as argument of seq(), one_of() and none_of(),
/// so that both b'[u8] literal' and "string literal" can be accepted.
pub trait Train<K> {
	fn knots(&self) -> Vec<K>;
}

impl Train<char> for str {
	fn knots(&self) -> Vec<char> {
		self.chars().collect()
	}
}

impl Train<u8> for [u8] {
	fn knots(&self) -> Vec<u8> {
		self.to_vec()
	}
}

macro_rules! impl_train_for_array
{
	($n:expr) => {
		impl Train<u8> for [u8; $n] {
			fn knots(&self) -> Vec<u8> {
				self.to_vec()
			}
		}
	};
}

impl_train_for_array!(0);
impl_train_for_array!(1);
impl_train_for_array!(2);
impl_train_for_array!(3);
impl_train_for_array!(4);
impl_train_for_array!(5);
impl_train_for_array!(6);
impl_train_for_array!(7);
impl_train_for_array!(8);
impl_train_for_array!(9);
impl_train_for_array!(10);
impl_train_for_array!(11);
impl_train_for_array!(12);
impl_train_for_array!(13);
impl_train_for_array!(14);
impl_train_for_array!(15);
impl_train_for_array!(16);
impl_train_for_array!(17);
impl_train_for_array!(18);
impl_train_for_array!(19);
impl_train_for_array!(20);
impl_train_for_array!(21);
impl_train_for_array!(22);
impl_train_for_array!(23);
impl_train_for_array!(24);
impl_train_for_array!(25);
impl_train_for_array!(26);
impl_train_for_array!(27);
impl_train_for_array!(28);
impl_train_for_array!(29);
impl_train_for_array!(30);
impl_train_for_array!(31);
impl_train_for_array!(32);
