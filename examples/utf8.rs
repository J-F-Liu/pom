// Example shows basic UTF-8 combinators

use pom::utf8::*;

fn main() {
	// Informal, Spanish-language movie database format
	let input = "\
Título: Abre los ojos
Año: 1997
Director: Alejandro Amenábar

Título: Amores Perros
Director: Alejandro González Iñárritu
Año: 2000

Título: La montaña sagrada
Año: 1973
Director: Alejandro Jodorowsky
";

	enum DataLine<'a> {
		Title(&'a str),
		Director(&'a str),
		Year(i32),
	}

	fn positive<'a>() -> Parser<'a, i32> {
//		let integer = (one_of("123456789") - one_of("0123456789").repeat(0..)) | sym(b'0'); // TODO
		let digit = sym('0') | sym('1') | sym('2') | sym('3') | sym('4') | sym('5') | sym('6') | sym('7') | sym('8') | sym('9');
		let integer = digit.discard().repeat(1..);
		integer.collect().convert(|x|x.parse::<i32>())
	}

	fn rest_str<'a>() -> Parser<'a, &'a str> {
		any().repeat(1..).collect()
	}

	fn separator<'a>() ->Parser<'a, ()> {
		seq(": ").discard()
	}

	let parser =
		  (seq("Título")   * separator() * rest_str().map(|s| DataLine::Title(s)))
		| (seq("Director") * separator() * rest_str().map(|s| DataLine::Director(s)))
		| (seq("Año")      * separator() * positive().map(|i| DataLine::Year(i)));

	{
		let mut title_opt:Option<&str> = None;
		let mut year_opt:Option<i32> = None;
		let mut director_opt:Option<&str> = None;

		for line in input.lines()  {
			if !line.is_empty() { // Skip blank lines without parsing
				// Parse line
				match parser.parse(line.as_bytes()).unwrap() {
					DataLine::Title(s) =>    title_opt = Some(s),
					DataLine::Director(s) => director_opt = Some(s),
					DataLine::Year(s) =>     year_opt = Some(s),
				}
				// When all three line types have been collected, print them
				if let (Some(title), Some(year), Some(director)) = (title_opt,year_opt,director_opt) {
					println!("Title: {}\nDirector: {}\nYear: {}\n", title, director, year);
					(title_opt, year_opt, director_opt) = (None,None,None);
				}
			}
		}
	}
}
