use pom::parser::*;

#[derive(Clone, Debug, PartialEq)]
struct Container {
	containers: Vec<Container>,
	contents: Vec<String>,
}

enum TmpContainerOrContent {
	Container(Container),
	Content(String),
}

fn whitespace<'a>() -> Parser<'a, u8, ()> {
	one_of(b" \t\r\n").repeat(0..).discard()
}

fn linebreak<'a>() -> Parser<'a, u8, ()> {
	sym(b'\r').opt() * sym(b'\n').discard()
}

fn indented<'a>() -> Parser<'a, u8, Vec<u8>> {
	sym(b'\t') * none_of(b"\n\r").repeat(1..) - linebreak()
}

fn empty<'a>() -> Parser<'a, u8, ()> {
	one_of(b" \t").repeat(0..).discard() - linebreak()
}

fn content<'a>() -> Parser<'a, u8, String> {
	none_of(b" \t\r\n").repeat(1..).convert(String::from_utf8) - linebreak()
}

fn subcontainer<'a>() -> Parser<'a, u8, (Vec<Container>, Vec<String>)> {
	(
		call(container).map(|ctr| TmpContainerOrContent::Container(ctr)) |
		content().map(|ctn| TmpContainerOrContent::Content(ctn))
	).repeat(1..).map(
		|tmp| {
			tmp.into_iter().fold(
				(vec![], vec![]),
				|acc, x| match x {
					TmpContainerOrContent::Container(ct) => (
						acc.0.into_iter().chain(vec![ct].into_iter()).collect(),
						acc.1,
					),
					TmpContainerOrContent::Content(cn) => (
						acc.0,
						acc.1.into_iter().chain(vec![cn].into_iter()).collect(),
					),
				}
			)
		}
	)
}

fn container<'a>() -> Parser<'a, u8, Container> {
	seq(b"Container\n") *
	(
		indented() |
		empty().map(|()| vec![])
	).repeat(1..).map(
		|lines| lines.into_iter().filter(
			|line| line.len() > 0
		).fold(
			vec![],
			|accum, line| accum.into_iter().chain(
				line.into_iter().chain(vec![b'\n'].into_iter())
			).collect()
		)
	).map(|deden| {
		subcontainer().parse(&deden).expect("subcont")
	}).map(|(containers, contents)| Container { containers, contents })
}

fn mylang<'a>() -> Parser<'a, u8, Vec<Container>> {
	(
		whitespace() *
		list(
			call(container),
			whitespace()
		)
	)
}

fn main() -> Result<(), ()> {
	let input = br#"
Container
	Container
		a
		b
		c

	1
	2
	3

	Container
		q

Container
	foo
	bar

	Container
		baz
		quux
		"#;

	assert_eq!(
		mylang().parse(input),
		Ok(
			vec![
				Container {
					containers: vec![
						Container {
							containers: vec![],
							contents: vec![
								"a".into(),
								"b".into(),
								"c".into(),
							]
						},
						Container {
							containers: vec![],
							contents: vec![
								"q".into(),
							]
						}
					],
					contents: vec![
						"1".into(),
						"2".into(),
						"3".into(),
					]
				},
				Container {
					containers: vec![
						Container {
							contents: vec![
								"baz".into(),
								"quux".into(),
							],
							containers: vec![],
						},
					],
					contents: vec![
						"foo".into(),
						"bar".into(),
					]
				},
			]
		)
	);

	Ok(())
}
