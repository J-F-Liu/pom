/// Recognises an alphabetic character, `a-zA-Z`.
#[inline]
pub fn alpha(term: u8) -> bool {
	(term >= 0x41 && term <= 0x5A) || (term >= 0x61 && term <= 0x7A)
}

/// Recognises a decimal digit, `0-9`.
#[inline]
pub fn digit(term: u8) -> bool {
	term >= 0x30 && term <= 0x39
}

/// Recognises an alphanumeric character, `a-zA-Z0-9`.
#[inline]
pub fn alphanum(term: u8) -> bool {
	alpha(term) || digit(term)
}

/// Recognises a hexadecimal digit, `0-9a-fA-F`.
#[inline]
pub fn hex_digit(term: u8) -> bool {
	(term >= 0x30 && term <= 0x39)
		|| (term >= 0x41 && term <= 0x46)
		|| (term >= 0x61 && term <= 0x66)
}

/// Recognises an octal digit, `0-7`.
#[inline]
pub fn oct_digit(term: u8) -> bool {
	term >= 0x30 && term <= 0x37
}

/// Recognises a space or tab.
#[inline]
pub fn space(term: u8) -> bool {
	term == b' ' || term == b'\t'
}

/// Recognises a space, tab, line feed, or carriage return.
#[inline]
pub fn multispace(term: u8) -> bool {
	space(term) || term == b'\n' || term == b'\r'
}
