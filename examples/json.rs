extern crate pom;
use pom::{Input};
use pom::parser::*;

use std::str::FromStr;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum JsonValue {
	Null,
	Bool(bool),
	Str(String),
	Num(f64),
	Array(Vec<JsonValue>),
	Object(HashMap<String,JsonValue>)
}

fn space() -> Parser<u8, ()> {
	one_of(b" \t\r\n").repeat(0..).discard()
}

fn number() -> Parser<u8, f64> {
	let integer = one_of(b"123456789") - one_of(b"0123456789").repeat(0..) | term(b'0');
	let frac = term(b'.') + one_of(b"0123456789").repeat(1..);
	let exp = one_of(b"eE") + one_of(b"+-").opt() + one_of(b"0123456789").repeat(1..);
	let number = term(b'-').opt() + integer + frac.opt() + exp.opt();
	number.collect().map(|v|String::from_utf8(v).unwrap()).map(|s|f64::from_str(&s).unwrap())
}

fn string() -> Parser<u8, String> {
	let special_char = term(b'\\') | term(b'/') | term(b'"')
		| term(b'b').map(|_|b'\x08') | term(b'f').map(|_|b'\x0C')
		| term(b'n').map(|_|b'\n') | term(b'r').map(|_|b'\r') | term(b't').map(|_|b'\t');
	let escape_sequence = term(b'\\') * special_char;
	let string = term(b'"') * (none_of(b"\\\"") | escape_sequence).repeat(0..) - term(b'"');
	string.map(|v|String::from_utf8(v).unwrap())
}

fn array() -> Parser<u8, Vec<JsonValue>> {
	let elems = list(call(value), term(b',') * space());
	let arr = term(b'[') * space() * elems.opt() - term(b']');
	arr.map(|elems|elems.unwrap_or(vec![]))
}

fn object() -> Parser<u8, HashMap<String, JsonValue>> {
	let member = string() - space() - term(b':') - space() + call(value);
	let members = list(member, term(b',') * space());
	let obj = term(b'{') * space() * members.opt() - term(b'}');
	obj.map(|members|members.unwrap_or(vec![]).into_iter().collect::<HashMap<String,JsonValue>>())
}

fn value() -> Parser<u8, JsonValue> {
	( seq(b"null").map(|_|JsonValue::Null)
	| seq(b"true").map(|_|JsonValue::Bool(true))
	| seq(b"false").map(|_|JsonValue::Bool(false))
	| number().map(|num|JsonValue::Num(num))
	| string().map(|text|JsonValue::Str(text))
	| array().map(|arr|JsonValue::Array(arr))
	| object().map(|obj|JsonValue::Object(obj))
	) - space()
}

pub fn json() -> Parser<u8, JsonValue> {
	space() * value() - eof()
}

fn main() {
	let test = br#"
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
        }
    }"#;

	let mut input = Input::new(test);
	println!("{:?}", json().parse(&mut input));
}
