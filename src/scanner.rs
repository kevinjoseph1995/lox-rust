use crate::error::LoxError;
use crate::tokens::Token;
use crate::tokens::TokenType;

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
    ("println", TokenType::Println),
    ("return", TokenType::Return),
    ("super", TokenType::Super),
    ("this", TokenType::This),
    ("true", TokenType::True),
    ("var", TokenType::Var),
    ("while", TokenType::While),
];

pub fn scan_tokens(input: &[u8]) -> Result<Vec<Token>, LoxError> {
    let mut output_tokens: Vec<Token> = Vec::new();
    let mut index = 0;
    let mut line_number = 1;
    let mut column = 1;

    let is_number = |x: u8| return x >= b'0' && x <= b'9';
    let is_alphabet = |x: u8| return (x >= b'a' && x <= b'z') || (x >= b'A' && x <= b'Z');
    let is_alphanumeric = |x: u8| return is_number(x) || is_alphabet(x);

    let mut found_error = false;
    while index < input.len() {
        let ch = input[index];
        match ch {
            // Single character lexemes
            b'(' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::LeftParen,
                start: column,
                length: 1,
            }),
            b')' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::RightParen,
                start: column,
                length: 1,
            }),
            b'{' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::LeftBrace,
                start: column,
                length: 1,
            }),
            b'}' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::RightBrace,
                start: column,
                length: 1,
            }),
            b',' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::Comma,
                start: column,
                length: 1,
            }),
            b'.' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::Dot,
                start: column,
                length: 1,
            }),
            b'-' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::Minus,
                start: column,
                length: 1,
            }),
            b'+' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::Plus,
                start: column,
                length: 1,
            }),
            b';' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::Semicolon,
                start: column,
                length: 1,
            }),
            b'*' => output_tokens.push(Token {
                line_number,
                token_type: TokenType::Star,
                start: column,
                length: 1,
            }),
            // Two character operator lexemes
            b'!' => {
                if index + 1 < input.len() && input[index + 1] == b'=' {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::BangEqual,
                        start: column,
                        length: 2,
                    });
                    index += 1;
                    column += 1;
                } else {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::Bang,
                        start: column,
                        length: 1,
                    });
                }
            }
            b'=' => {
                if index + 1 < input.len() && input[index + 1] == b'=' {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::EqualEqual,
                        start: column,
                        length: 2,
                    });
                    index += 1;
                    column += 1;
                } else {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::Equal,
                        start: column,
                        length: 1,
                    });
                }
            }
            b'<' => {
                if index + 1 < input.len() && input[index + 1] == b'=' {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::LessEqual,
                        start: column,
                        length: 2,
                    });
                    index += 1;
                    column += 1
                } else {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::Less,
                        start: column,
                        length: 1,
                    });
                }
            }
            b'>' => {
                if index + 1 < input.len() && input[index + 1] == b'=' {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::GreaterEqual,
                        start: column,
                        length: 2,
                    });
                    index += 1;
                    column += 1;
                } else {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::Greater,
                        start: column,
                        length: 1,
                    });
                }
            }
            b'/' => {
                if index + 1 < input.len() && input[index + 1] == b'/' {
                    // This is the start of a comment, consume all characters till the end
                    index += 2;
                    column += 2;
                    loop {
                        if index >= input.len() {
                            break;
                        }
                        if input[index] == b'\n' {
                            line_number += 1;
                            break;
                        }
                        index += 1;
                        column += 1;
                    }
                } else {
                    output_tokens.push(Token {
                        line_number,
                        token_type: TokenType::Slash,
                        start: column,
                        length: 1,
                    });
                }
            }
            // Ignore all kinds of whitespace
            b' ' => {}
            b'\t' => {
                column += 7;
            }
            b'\r' => {}
            b'\n' => {
                line_number += 1;
                column = 0;
            }
            b'"' => {
                //Start of a string literal
                let start_column = column;
                let start = index + 1;
                let start_line_number = line_number;
                while index + 1 < input.len() && input[index + 1] != b'"' {
                    if input[index + 1] == b'\n' {
                        line_number += 1;
                    }
                    index += 1;
                    column += 1;
                }
                if index == input.len() - 1 {
                    found_error = true;
                    println!("Unterminated string");
                }
                let decoded_string = std::str::from_utf8(&input[start..index + 1])?;
                output_tokens.push(Token {
                    line_number: start_line_number,
                    token_type: TokenType::StringLiteral(decoded_string.to_string()),
                    start: start_column,
                    length: index + 1 - start + 2,
                });
                column += 1;
                index += 1; // This moves index to the closing quotation mark
            }
            c if is_number(c) => {
                // Start of number literal
                let start = index;
                let start_column = column;
                while index + 1 < input.len()
                    && (is_number(input[index + 1]) || input[index + 1] == b'.')
                {
                    index += 1;
                    column += 1;
                }
                if input[index] == b'.' {
                    found_error = true;
                    println!("Number literal cannot terminate on a \".\"");
                }
                let number_str = std::str::from_utf8(&input[start..index + 1])?;
                let parsed_number: f64 = number_str.parse()?;
                output_tokens.push(Token {
                    line_number,
                    token_type: TokenType::NumberLiteral(parsed_number),
                    start: start_column,
                    length: index + 1 - start,
                });
            }
            c if is_alphabet(c) => {
                // Start of an identifier or keyword
                let start = index;
                let start_column = column;
                while index + 1 < input.len()
                    && (is_alphanumeric(input[index + 1]) || input[index + 1] == b'_')
                {
                    index += 1;
                    column += 1;
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
                        output_tokens.push(Token {
                            line_number,
                            token_type: keyword_token_type,
                            start: start_column,
                            length: name.len(),
                        });
                    }
                    _ => {
                        let decoded_string = std::str::from_utf8(&input[start..index + 1])?;
                        output_tokens.push(Token {
                            line_number,
                            token_type: TokenType::Identifier(decoded_string.to_string()),
                            start: start_column,
                            length: name.len(),
                        });
                    }
                }
            }
            _ => {
                found_error = true;
                println!(
                    "Invalid character on line: {} column: {}",
                    line_number, column
                );
            }
        }
        index += 1;
        column += 1;
    }
    if found_error {
        return Err(LoxError::LexErr);
    }
    output_tokens.push(Token {
        line_number,
        token_type: TokenType::Eof,
        start: column,
        length: 0,
    });
    Ok(output_tokens)
}
