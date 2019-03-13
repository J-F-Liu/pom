use pom::char_class::hex_digit;
use pom::parser::*;

use std::char::{decode_utf16, REPLACEMENT_CHARACTER};
use std::collections::HashMap;
use std::str::{self, FromStr};

#[derive(Debug, PartialEq)]
pub enum JsonValue {
	Null,
	Bool(bool),
	Str(String),
	Num(f64),
	Array(Vec<JsonValue>),
	Object(HashMap<String, JsonValue>),
}

fn space<'a>() -> Parser<'a, u8, ()> {
	one_of(b" \t\r\n").repeat(0..).discard()
}

fn number<'a>() -> Parser<'a, u8, f64> {
	let integer = one_of(b"123456789") - one_of(b"0123456789").repeat(0..) | sym(b'0');
	let frac = sym(b'.') + one_of(b"0123456789").repeat(1..);
	let exp = one_of(b"eE") + one_of(b"+-").opt() + one_of(b"0123456789").repeat(1..);
	let number = sym(b'-').opt() + integer + frac.opt() + exp.opt();
	number.collect().convert(str::from_utf8).convert(f64::from_str)
}

fn string<'a>() -> Parser<'a, u8, String> {
	let special_char = sym(b'\\') | sym(b'/') | sym(b'"')
		| sym(b'b').map(|_|b'\x08') | sym(b'f').map(|_|b'\x0C')
		| sym(b'n').map(|_|b'\n') | sym(b'r').map(|_|b'\r') | sym(b't').map(|_|b'\t');
	let escape_sequence = sym(b'\\') * special_char;
	let char_string = (none_of(b"\\\"") | escape_sequence).repeat(1..).convert(String::from_utf8);
	let utf16_char = seq(b"\\u") * is_a(hex_digit).repeat(4).convert(String::from_utf8).convert(|digits|u16::from_str_radix(&digits, 16));
	let utf16_string = utf16_char.repeat(1..).map(|chars|decode_utf16(chars).map(|r| r.unwrap_or(REPLACEMENT_CHARACTER)).collect::<String>());
	let string = sym(b'"') * (char_string | utf16_string).repeat(0..) - sym(b'"');
	string.map(|strings| strings.concat())
}

fn array<'a>() -> Parser<'a, u8, Vec<JsonValue>> {
	let elems = list(call(value), sym(b',') * space());
	sym(b'[') * space() * elems - sym(b']')
}

fn object<'a>() -> Parser<'a, u8, HashMap<String, JsonValue>> {
	let member = string() - space() - sym(b':') - space() + call(value);
	let members = list(member, sym(b',') * space());
	let obj = sym(b'{') * space() * members - sym(b'}');
	obj.map(|members| members.into_iter().collect::<HashMap<_, _>>())
}

fn value<'a>() -> Parser<'a, u8, JsonValue> {
	( seq(b"null").map(|_|JsonValue::Null)
	| seq(b"true").map(|_|JsonValue::Bool(true))
	| seq(b"false").map(|_|JsonValue::Bool(false))
	| number().map(|num|JsonValue::Num(num))
	| string().map(|text|JsonValue::Str(text))
	| array().map(|arr|JsonValue::Array(arr))
	| object().map(|obj|JsonValue::Object(obj))
	) - space()
}

pub fn json<'a>() -> Parser<'a, u8, JsonValue> {
	space() * value() - end()
}

#[allow(dead_code)]
fn main() {
	let input = br#"
	{
        "Image": {
            "Width":  800,
            "Height": 600,
            "Title":  "View from 15th Floor",
            "Thumbnail": {
                "Url":    "http://www.example.com/image/481989943",
                "Height": 125,
                "Width":  100
            },
            "Animated" : false,
            "IDs": [116, 943, 234, 38793]
        },
        "escaped characters": "\u2192\uD83D\uDE00\"\t\uD834\uDD1E"
    }"#;

	println!("{:?}", json().parse(input));
}
