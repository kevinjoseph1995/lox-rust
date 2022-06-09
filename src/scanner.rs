use crate::error::LoxError;
use crate::tokens::Token;
use crate::tokens::TokenType;

#[derive(Default)]
pub struct Scanner {
    output_tokens: Vec<Token>,
}

const KEYWORD_MAP: &'static [(&'static str, TokenType)] = &[
    ("and", TokenType::And),
    ("class", TokenType::Class),
    ("else", TokenType::Else),
    ("false", TokenType::False),
    ("fun", TokenType::Fun),
    ("for", TokenType::For),
    ("if", TokenType::If),
    ("nil", TokenType::Nil),
    ("or", TokenType::Or),
    ("print", TokenType::Print),
    ("return", TokenType::Return),
    ("super", TokenType::Super),
    ("this", TokenType::This),
    ("true", TokenType::True),
    ("var", TokenType::Var),
    ("while", TokenType::While),
];

impl Scanner {
    fn reset_scanner_state(&mut self) {
        self.output_tokens.clear();
    }

    pub fn scan_tokens(&mut self, input: &[u8]) -> Result<&[Token], LoxError> {
        self.reset_scanner_state();
        let mut index = 0;
        let mut line_number = 1;

        let is_number = |x: u8| return x >= b'0' && x <= b'9';
        let is_alphabet = |x: u8| return (x >= b'a' && x <= b'z') || (x >= b'A' && x <= b'Z');
        let is_alphanumeric = |x: u8| return is_number(x) || is_alphabet(x);

        while index < input.len() {
            let ch = input[index];
            match ch {
                // Single character lexemes
                b'(' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::LeftParen,
                }),
                b')' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::RightParen,
                }),
                b'{' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::LeftBrace,
                }),
                b'}' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::RightBrace,
                }),
                b',' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::Comma,
                }),
                b'.' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::Dot,
                }),
                b'-' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::Minus,
                }),
                b'+' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::Plus,
                }),
                b';' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::Semicolon,
                }),
                b'*' => self.output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::Star,
                }),
                // Two character operator lexemes
                b'!' => {
                    if index + 1 < input.len() && input[index + 1] == b'=' {
                        index += 1;
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::BangEqual,
                        });
                    } else {
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::Bang,
                        });
                    }
                }
                b'=' => {
                    if index + 1 < input.len() && input[index + 1] == b'=' {
                        index += 1;
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::EqualEqual,
                        });
                    } else {
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::Equal,
                        });
                    }
                }
                b'<' => {
                    if index + 1 < input.len() && input[index + 1] == b'=' {
                        index += 1;
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::LessEqual,
                        });
                    } else {
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::Less,
                        });
                    }
                }
                b'>' => {
                    if index + 1 < input.len() && input[index + 1] == b'=' {
                        index += 1;
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::GreaterEqual,
                        });
                    } else {
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::Greater,
                        });
                    }
                }
                b'/' => {
                    if index + 1 < input.len() && input[index + 1] == b'/' {
                        // This is the start of a comment, consume all characters till the end
                        index = index + 2;
                        loop {
                            if index >= input.len() {
                                break;
                            }
                            if input[index] == b'\n' {
                                index += 1;
                                line_number += 1;
                                break;
                            }
                            index += 1;
                        }
                    } else {
                        self.output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::Slash,
                        });
                    }
                }
                // Ignore all kinds of whitespace
                b' ' => {}
                b'\t' => {}
                b'\r' => {}
                b'\n' => {
                    line_number += 1;
                }
                b'"' => {
                    //Start of a string literal
                    let start = index + 1;
                    let start_line_number = line_number;
                    while index + 1 < input.len() && input[index + 1] != b'"' {
                        if input[index + 1] == b'\n' {
                            line_number += 1;
                        }
                        index += 1;
                    }
                    if index == input.len() - 1 {
                        return Err(LoxError::LexErr("Unterminated string".to_string()));
                    }
                    self.output_tokens.push(Token {
                        line_number: start_line_number,
                        token_type: TokenType::StringLiteral(input[start..index + 1].to_vec()),
                    });
                    index += 1; // This moves index to the closing quotation mark
                }
                c if is_number(c) => {
                    // Start of number literal
                    let start = index;
                    while index + 1 < input.len()
                        && (is_number(input[index + 1]) || input[index + 1] == b'.')
                    {
                        index += 1;
                    }
                    if input[index] == b'.' {
                        return Err(LoxError::LexErr(
                            "Number literal cannot terminate on a \".\"".to_string(),
                        ));
                    }
                    let number_str = std::str::from_utf8(&input[start..index + 1])?;
                    let parsed_number: f64 = number_str.parse()?;
                    self.output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::NumberLiteral(parsed_number),
                    });
                }
                c if is_alphabet(c) => {
                    // Start of an identifier or keyword
                    let start = index;
                    while index + 1 < input.len()
                        && (is_alphanumeric(input[index + 1]) || input[index + 1] == b'_')
                    {
                        index += 1;
                    }
                    let is_keyword = |x: &str| -> Option<TokenType> {
                        for (key, token_type) in KEYWORD_MAP {
                            if x == *key {
                                return Some(token_type.clone());
                            }
                        }
                        None
                    };
                    let name = std::str::from_utf8(&input[start..index + 1])?;
                    match is_keyword(name) {
                        Some(keyword_token_type) => {
                            self.output_tokens.push(Token {
                                line_number,
                                token_type: keyword_token_type,
                            });
                        }
                        _ => {
                            self.output_tokens.push(Token {
                                line_number,
                                token_type: TokenType::Identifier(input[start..index + 1].to_vec()),
                            });
                        }
                    }
                }
                _ => {
                    return Err(LoxError::LexErr(format!(
                        "Invalid character on line: {}",
                        line_number
                    )));
                }
            }
            index = index + 1;
        }
        self.output_tokens.push(Token {
            line_number,
            token_type: TokenType::Eof,
        });
        Ok(&self.output_tokens[..])
    }
}
