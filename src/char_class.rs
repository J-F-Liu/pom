/// Recognises an alphabetic character, `a-zA-Z`.
#[inline]
pub fn alpha(term: u8) -> bool {
	term.is_ascii_alphabetic()
}

/// Recognises an alphabetic character, `A-Z`.
#[inline]
pub fn alpha_uppercase(term: u8) -> bool {
	term.is_ascii_uppercase()
}

/// Recognises an alphabetic character, `a-z`.
#[inline]
pub fn alpha_lowercase(term: u8) -> bool {
	term.is_ascii_lowercase()
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
	matches!(term, 0x30..=0x39 | 0x41..=0x46 | 0x61..=0x66)
}

/// Recognises an octal digit, `0-7`.
#[inline]
pub fn oct_digit(term: u8) -> bool {
	matches!(term, 0x30..=0x37)
}

/// Recognises a space or tab.
#[inline]
pub fn space(term: u8) -> bool {
	matches!(term, b' ' | b'\t')
}

/// Recognises a space, tab, line feed, or carriage return.
#[inline]
pub fn multispace(term: u8) -> bool {
	space(term) || matches!(term, b'\n' | b'\r')
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn is_an_alpha() {
		assert!(alpha(b'A'));
		assert!(alpha(b'Z'));
		assert!(alpha(b'a'));
		assert!(alpha(b'z'));
	}

	#[test]
	fn is_an_alpha_uppercase() {
		assert!(alpha_uppercase(b'A'));
		assert!(alpha_uppercase(b'Z'));
		assert!(!alpha_uppercase(b'a'));
		assert!(!alpha_uppercase(b'z'));
	}

	#[test]
	fn is_an_alpha_lowercase() {
		assert!(!alpha_lowercase(b'A'));
		assert!(!alpha_lowercase(b'Z'));
		assert!(alpha_lowercase(b'a'));
		assert!(alpha_lowercase(b'z'));
	}

	#[test]
	fn is_a_digit() {
		assert!(digit(b'0'));
		assert!(digit(b'9'));
		assert!(!digit(b'A'));
	}

	#[test]
	fn is_an_alphanum() {
		assert!(alphanum(b'A'));
		assert!(alphanum(b'Z'));
		assert!(alphanum(b'a'));
		assert!(alphanum(b'z'));
		assert!(alphanum(b'0'));
		assert!(alphanum(b'9'));
		assert!(!alphanum(b'#'));
	}

	#[test]
	fn is_a_hex_digit() {
		assert!(hex_digit(b'0'));
		assert!(hex_digit(b'9'));
		assert!(hex_digit(b'A'));
		assert!(hex_digit(b'F'));
		assert!(hex_digit(b'a'));
		assert!(hex_digit(b'f'));
		assert!(!hex_digit(b'G'));
	}

	#[test]
	fn is_a_oct_digit() {
		assert!(oct_digit(b'0'));
		assert!(oct_digit(b'7'));
		assert!(!oct_digit(b'8'));
		assert!(!oct_digit(b'9'));
	}

	#[test]
	fn is_space() {
		assert!(space(b' '));
		assert!(space(b'\t'));
		assert!(!space(b'\n'));
		assert!(!space(b'A'));
	}

	#[test]
	fn is_multispace() {
		assert!(multispace(b' '));
		assert!(multispace(b'\t'));
		assert!(multispace(b'\n'));
		assert!(!multispace(b'A'));
	}
}