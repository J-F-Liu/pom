extern crate pom;
use pom::TextInput;
use pom::parser::*;

use std::str::FromStr;
use std::char::{decode_utf16, REPLACEMENT_CHARACTER};
use std::iter::FromIterator;
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

fn space() -> Parser<char, ()> {
	one_of(" \t\r\n").repeat(0..).discard()
}

fn number() -> Parser<char, f64> {
	let integer = one_of("123456789") - one_of("0123456789").repeat(0..) | term('0');
	let frac = term('.') + one_of("0123456789").repeat(1..);
	let exp = one_of("eE") + one_of("+-").opt() + one_of("0123456789").repeat(1..);
	let number = term('-').opt() + integer + frac.opt() + exp.opt();
	number.collect().map(|v|String::from_iter(v)).map(|s|f64::from_str(&s).unwrap())
}

fn string() -> Parser<char, String> {
	let special_char = term('\\') | term('/') | term('"')
		| term('b').map(|_|'\x08') | term('f').map(|_|'\x0C')
		| term('n').map(|_|'\n') | term('r').map(|_|'\r') | term('t').map(|_|'\t');
	let escape_sequence = term('\\') * special_char;
	let char_string = (none_of("\\\"") | escape_sequence).repeat(1..).map(|chars|String::from_iter(chars));
	let utf16_char = term('\\') * term('u') * is_a(|c:char|c.is_digit(16)).repeat(4..5).map(|digits|u16::from_str_radix(&String::from_iter(digits), 16).unwrap());
	let utf16_string = utf16_char.repeat(1..).map(|chars|decode_utf16(chars).map(|r| r.unwrap_or(REPLACEMENT_CHARACTER)).collect::<String>());
	let string = term('"') * (char_string | utf16_string).repeat(0..) - term('"');
	string.map(|strings|strings.concat())
}

fn array() -> Parser<char, Vec<JsonValue>> {
	let elems = list(call(value), term(',') * space());
	let arr = term('[') * space() * elems.opt() - term(']');
	arr.map(|elems|elems.unwrap_or(vec![]))
}

fn object() -> Parser<char, HashMap<String, JsonValue>> {
	let member = string() - space() - term(':') - space() + call(value);
	let members = list(member, term(',') * space());
	let obj = term('{') * space() * members.opt() - term('}');
	obj.map(|members|members.unwrap_or(vec![]).into_iter().collect::<HashMap<_,_>>())
}

fn value() -> Parser<char, JsonValue> {
	( seq("null").map(|_|JsonValue::Null)
	| seq("true").map(|_|JsonValue::Bool(true))
	| seq("false").map(|_|JsonValue::Bool(false))
	| number().map(|num|JsonValue::Num(num))
	| string().map(|text|JsonValue::Str(text))
	| array().map(|arr|JsonValue::Array(arr))
	| object().map(|obj|JsonValue::Object(obj))
	) - space()
}

pub fn json() -> Parser<char, JsonValue> {
	space() * value() - end()
}

#[allow(dead_code)]
fn main() {
	let test = r#"
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

	let mut input = TextInput::new(test);
	println!("{:?}", json().parse(&mut input));
}
