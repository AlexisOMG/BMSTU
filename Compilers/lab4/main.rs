use std::{i128, collections::HashMap};

#[derive(Debug)]
enum TokenKinds {
		Ident(i64),
		Number(i128),
		Kek,
		Xx,
		Xxx,
		EOF,
		Empty,
		Error,
}

#[derive(Debug, Clone, Copy)]
struct Position {
	index: usize,
	line: usize,
	column: usize,
}

#[derive(Debug)]
struct Token {
	start: Position,
	end: Position,
	kind: TokenKinds,
}

#[derive(Debug)]
struct Lexer {
	text: Vec<char>,
	curr_pos: Position,
	prev_pos: Position,
	ident_table: HashMap<String, i64>,
	id: i64
}

impl Lexer {
		fn next_token(&mut self) -> Token {
			if self.has_next() {
				let mut tok = Token{
					start: self.curr_pos,
					end: self.curr_pos,
					kind: TokenKinds::Empty,
				};
				let x = self.next();
				if x.is_alphabetic() {

					let mut prev = x;
					let mut s = String::new();
					s.push(x);
					while self.has_next() {
							let curr = self.peak();
							if !curr.is_alphanumeric() {
								break;
							}
							s.push(curr);
							prev = self.next();
					}

					if prev.is_alphabetic() {
						tok.end = self.prev_pos;
						if s == "qeq" {
							tok.kind = TokenKinds::Kek;
						} else if s == "xx" {
							tok.kind = TokenKinds::Xx;
						} else if s == "xxx" {
							tok.kind = TokenKinds::Xxx;
						} else {
								let ident = self.ident_table.get(&s);
								match ident {
										Some(c) => {
											tok.kind = TokenKinds::Ident(*c);
										},
										None => {
											self.ident_table.insert(s, self.id);
											tok.kind = TokenKinds::Ident(self.id);
											self.id += 1;
										}
								}
						}
					} else {
						tok.start = self.prev_pos;
						tok.end = self.prev_pos;
						tok.kind = TokenKinds::Error;
					}
					
				} else if x == '0' {

						let mut num: String = String::new();
						while self.has_next() {
							let curr = self.peak();
							if !(curr >= 'a' && curr <= 'f' || curr >= 'A' && curr <= 'F' || curr.is_numeric()){
								break;
							}
							num.push(curr);
							self.next();
						}

						if self.has_next() && !self.peak().is_whitespace() {
							tok.start = self.prev_pos;
							tok.end = self.prev_pos;
							tok.kind = TokenKinds::Error;
						} else {
							tok.end = self.prev_pos;
							tok.kind = TokenKinds::Number(i128::from_str_radix(&*num, 16).unwrap());
						}
				} else if x.is_whitespace() {
						return self.next_token();
				} else {
						tok.start = self.prev_pos;
						tok.end = self.prev_pos;
						tok.kind = TokenKinds::Error;
				}
				return tok
			} else {
				Token { 
					start: self.curr_pos,
					end: self.curr_pos, 
					kind: TokenKinds::EOF,
				}
			}
		}

		fn has_next(&self) -> bool {
				self.curr_pos.index < self.text.len()
		}

		fn next(&mut self) -> char {
				let c = self.text[self.curr_pos.index];
				self.prev_pos = self.curr_pos;
				self.curr_pos.index += 1;
				if c == '\n' {
					self.curr_pos.line += 1;
					self.curr_pos.column = 1;
				} else {
						self.curr_pos.column += 1;
				}
				c
		}

		fn peak(&self) -> char {
			self.text[self.curr_pos.index]
		}

		fn new(s: String) -> Self {
				Self {
					text: s.chars().collect(),
					curr_pos: Position { index: 0, line: 1, column: 0 },
					prev_pos: Position { index: 0, line: 1, column: 0 },
					ident_table: HashMap::new(),
					id: 0
				}
		}
}

fn main() {
	let filename = std::env::args().nth(1).unwrap();
	println!("filename = {:?}", filename);
	let content = std::fs::read_to_string(filename).unwrap();

	let mut lex = Lexer::new(content);
	loop {
			let tok = lex.next_token();
			println!("{:?}", tok);
			match tok.kind {
					TokenKinds::EOF => break,
					_ => continue,
			}
	}

	for key in lex.ident_table.keys() {
		println!("{}:{}", key, lex.ident_table.get(key).unwrap());
	}
}
