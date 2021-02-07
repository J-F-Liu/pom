/// Recognises an alphabetic character, `a-zA-Z`.
#[inline]
pub fn alpha(term: u8) -> bool {
	term.is_ascii_alphabetic()
}

/// Recognises a decimal digit, `0-9`.
#[inline]
pub fn digit(term: u8) -> bool {
	term.is_ascii_digit()
}

/// Recognises an alphanumeric character, `a-zA-Z0-9`.
#[inline]
pub fn alphanum(term: u8) -> bool {
	term.is_ascii_alphanumeric()
}

/// Recognises a hexadecimal digit, `0-9a-fA-F`.
#[inline]
pub fn hex_digit(term: u8) -> bool {
	(0x30..=0x39).contains(&term) || (0x41..=0x46).contains(&term) || (0x61..=0x66).contains(&term)
}

/// Recognises an octal digit, `0-7`.
#[inline]
pub fn oct_digit(term: u8) -> bool {
	(0x30..=0x37).contains(&term)
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
