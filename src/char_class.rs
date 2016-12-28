
#[inline]
pub fn alpha(term: u8) -> bool {
	(term >= 0x41 && term <= 0x5A) || (term >= 0x61 && term <= 0x7A)
}

#[inline]
pub fn digit(term: u8) -> bool {
	term >= 0x30 && term <= 0x39
}

#[inline]
pub fn alphanum(term: u8) -> bool {
	alpha(term) || digit(term)
}

#[inline]
pub fn hex_digit(term: u8) -> bool {
	(term >= 0x30 && term <= 0x39) ||
	(term >= 0x41 && term <= 0x46) ||
	(term >= 0x61 && term <= 0x66)
}

#[inline]
pub fn oct_digit(term: u8) -> bool {
	term >= 0x30 && term <= 0x37
}
