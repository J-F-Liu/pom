use pom::parser::*;
use pom::Parser;

use std::str::{self, FromStr};

#[derive(Debug, PartialEq)]
struct Duration {
	years: Option<f32>,
	months: Option<f32>,
	weeks: Option<f32>,
	days: Option<f32>,
	hours: Option<f32>,
	minutes: Option<f32>,
	seconds: Option<f32>,
}

fn number_separator() -> Parser<u8, ()> {
	// either '.' or ',' can be used as a separator between the whole and decimal part of a number
	one_of(b".,").discard()
}

fn number() -> Parser<u8, f32> {
	let integer = one_of(b"0123456789").repeat(0..);
	let frac = number_separator() + one_of(b"0123456789").repeat(1..);
	let number = integer + frac.opt();
	number
		.collect()
		.convert(str::from_utf8)
		.convert(f32::from_str)
}

fn date_part() -> Parser<u8, (Option<f32>, Option<f32>, Option<f32>, Option<f32>)> {
	((number() - sym(b'Y')).opt()
		+ (number() - sym(b'M')).opt()
		+ (number() - sym(b'W')).opt()
		+ (number() - sym(b'D')).opt())
	.map(|(((years, months), weeks), days)| (years, months, weeks, days))
}

fn time_part() -> Parser<u8, (Option<f32>, Option<f32>, Option<f32>)> {
	sym(b'T')
		* ((number() - sym(b'H')).opt()
			+ (number() - sym(b'M')).opt()
			+ (number() - sym(b'S')).opt())
		.map(|((hours, minutes), seconds)| (hours, minutes, seconds))
}

fn parser() -> Parser<u8, Duration> {
	sym(b'P')
		* (time_part().map(|(hours, minutes, seconds)| Duration {
			years: None,
			months: None,
			weeks: None,
			days: None,
			hours,
			minutes,
			seconds,
		}) | (date_part() + time_part()).map(|(date_elements, time_elements)| {
			let (years, months, weeks, days) = date_elements;
			let (hours, minutes, seconds) = time_elements;
			Duration {
				years,
				months,
				weeks,
				days,
				hours,
				minutes,
				seconds,
			}
		}))
}

/// Parses the ISO 8601 Duration standard
/// https://en.wikipedia.org/wiki/ISO_8601#Durations
fn main() {
	let input = "P3Y6M4DT12H30M5S";
	let result = parser().parse(input.as_bytes());

	assert_eq!(
		Duration {
			years: Some(3f32),
			months: Some(6f32),
			weeks: None,
			days: Some(4f32),
			hours: Some(12f32),
			minutes: Some(30f32),
			seconds: Some(5f32)
		},
		result.unwrap()
	);
}
