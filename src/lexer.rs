extern crate regex;

use crate::{Location, Value, ZishError};
use bigdecimal::BigDecimal;
use chrono::prelude::{DateTime, Utc};
use regex::Regex;
use std::char;
use std::cmp::Ordering;
use std::str::FromStr;

#[derive(PartialEq, Eq, Debug)]
pub enum PrimitiveValue {
    Timestamp(DateTime<Utc>),
    String(String),
    Bytes(Vec<u8>),
    Integer(i32),
    Bool(bool),
    Null,
    Decimal(BigDecimal),
}

impl PrimitiveValue {
    pub fn into_value(self) -> Value {
        match self {
            PrimitiveValue::Timestamp(x) => Value::Timestamp(x),
            PrimitiveValue::String(x) => Value::String(x),
            PrimitiveValue::Bytes(x) => Value::Bytes(x),
            PrimitiveValue::Integer(x) => Value::Integer(x),
            PrimitiveValue::Bool(x) => Value::Bool(x),
            PrimitiveValue::Null => Value::Null,
            PrimitiveValue::Decimal(x) => Value::Decimal(x),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    // Single character tokens
    StartMap(Location),

    FinishMap(Location),
    Colon(Location),
    Comma(Location),
    StartList(Location),
    FinishList(Location),
    Primitive(Location, PrimitiveValue), // General primitive type
    Finish,
}

#[derive(Debug)]
struct State {
    in_token: bool,
    consumed: bool,
    stage: Stage,
    payload: String,
    prev_c: Option<char>,
    location: Location,
}

struct Chars {
    doc: Vec<char>,
    idx: usize,
}

impl Chars {
    fn new(doc: &str) -> Chars {
        Chars {
            doc: doc.chars().collect(),
            idx: 0,
        }
    }
}

impl Iterator for Chars {
    type Item = Option<char>;

    fn next(&mut self) -> Option<Option<char>> {
        let res = match self.idx.cmp(&self.doc.len()) {
            Ordering::Less => Some(Some(self.doc[self.idx])),
            Ordering::Equal => Some(None),
            Ordering::Greater => None,
        };
        self.idx += 1;
        res
    }
}

#[derive(PartialEq, Eq, Debug)]
enum Stage {
    // Delimited tokens
    Bytes,
    String,

    Primitive, // General primitive type
    NoDelim,   // Non-delimited primitive
    Timestamp,
    Comment,
}

const NO_DELIM_END: &[char] = &[
    '{', '}', ':', ',', '[', ']', '/', '\u{0009}', // tab
    '\u{000A}', // line feed
    '\u{000B}', // vertical tab
    '\u{000C}', // form feed
    '\u{000D}', // carriage return
    '\u{0020}', // space
];

fn make_re_integer() -> Regex {
    Regex::new(r"^-?(0|[1-9]\d*)$").unwrap()
}

fn make_re_decimal() -> Regex {
    Regex::new(r"^-?(0|[1-9]\d*)(\.\d*)?([eE][+\-]?\d+)?$").unwrap()
}
fn make_re_timestamp() -> Regex {
    Regex::new(
        r"\d\d\d\d-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])T([01]\d|2[0-3]):[0-5]\d:[0-5]\d(\.\d+)?([zZ]|[+\-]([01]\d|2[0-3]):[0-5]\d)$").unwrap()
}

pub fn lex(zish_str: &str) -> Result<Vec<Token>, ZishError> {
    let mut tokens: Vec<Token> = vec![];

    let re_integer: &Regex = &make_re_integer();
    let re_decimal: &Regex = &make_re_decimal();
    let re_timestamp: &Regex = &make_re_timestamp();

    let mut state: State = State {
        in_token: false,
        consumed: false,
        stage: Stage::Primitive,
        payload: "".to_string(),
        prev_c: None,
        location: Location {
            line: 1,
            character: 0,
        },
    };

    for c in Chars::new(zish_str) {
        // println!("character state {:?}", state);
        // println!("tokens {:?}", tokens);

        // Set position
        match c {
            Some('\n') => {
                state.location.line += 1;
                state.location.character = 1;
            }
            _ => state.location.character += 1,
        };

        state.consumed = false;

        if state.in_token {
            let res: Result<Option<Token>, ZishError> = match state.stage {
                Stage::String => lex_string(c, &mut state),
                Stage::Bytes => lex_bytes(c, &mut state),
                Stage::NoDelim => lex_no_delim(c, &mut state, re_decimal, re_integer),
                Stage::Timestamp => lex_timestamp(c, &mut state, &re_timestamp),
                Stage::Comment => lex_comment(c, &mut state),
                _ => panic!("Shouldn't be a single token here."),
            };
            match res {
                Err(e) => return Err(e),
                Ok(None) => (),
                Ok(Some(token)) => tokens.push(token),
            };
        };

        if !state.in_token && !state.consumed {
            match c {
                Some(h) => match h {
                    '\u{0009}' => (), // tab
                    '\u{000A}' => (), // line feed
                    '\u{000B}' => (), // vertical tab
                    '\u{000C}' => (), // form feed
                    '\u{000D}' => (), // carriage return
                    '\u{0020}' => (), // space
                    '{' => tokens.push(Token::StartMap(state.location)),
                    '}' => tokens.push(Token::FinishMap(state.location)),
                    ':' => tokens.push(Token::Colon(state.location)),
                    ',' => tokens.push(Token::Comma(state.location)),
                    '[' => tokens.push(Token::StartList(state.location)),
                    ']' => tokens.push(Token::FinishList(state.location)),
                    '"' => {
                        state.stage = Stage::String;
                        state.in_token = true;
                        state.payload.clear();
                    }
                    '\'' => {
                        state.stage = Stage::Bytes;
                        state.in_token = true;
                        state.payload.clear();
                    }
                    x @ '/' => {
                        state.stage = Stage::Comment;
                        state.in_token = true;
                        state.payload.clear();
                        state.payload.push(x);
                    }
                    x => {
                        state.stage = Stage::NoDelim;
                        state.in_token = true;
                        state.payload.clear();
                        state.payload.push(x);
                    }
                },
                None => tokens.push(Token::Finish),
            };
        };
        state.prev_c = c;
    }
    Ok(tokens)
}

fn lex_string(c: Option<char>, state: &mut State) -> Result<Option<Token>, ZishError> {
    match (c, state.prev_c) {
        (Some('"'), Some(prev)) if prev != '\\' => {
            state.in_token = false;
            state.consumed = true;
            let val: PrimitiveValue = match unescape(&state.payload, state.location) {
                Ok(v) => PrimitiveValue::String(v),
                Err(e) => return Err(e),
            };
            Ok(Some(Token::Primitive(state.location, val)))
        }
        (None, _) => {
            let s: &str = &state.payload;
            let init: &str = match s.char_indices().nth(10) {
                None => s,
                Some((idx, _)) => &s[..idx],
            };
            Err(ZishError::Location(
            format!(
                "Parsing a string but can't find the ending '\"'. The first part of the string is: {}", init), state.location))
        }
        (Some(x), _) => {
            state.payload.push(x);
            Ok(None)
        }
    }
}

fn lex_bytes(c: Option<char>, state: &mut State) -> Result<Option<Token>, ZishError> {
    match c {
        Some('\'') => {
            state.in_token = false;
            state.consumed = true;
            match base64::decode(state.payload.trim()) {
                Ok(bytes) => Ok(Some(Token::Primitive(
                    state.location,
                    PrimitiveValue::Bytes(bytes),
                ))),
                Err(e) => Err(ZishError::Location(format!("{}", e), state.location)),
            }
        }
        None => Err(ZishError::Location(
            format!(
                "Parsing bytes but can't find the ending '\''. The first part of the bytes is: {}",
                &state.payload[..10]
            ),
            state.location,
        )),
        Some(x) => {
            state.payload.push(x);
            Ok(None)
        }
    }
}

fn lex_no_delim(
    c: Option<char>,
    state: &mut State,
    re_decimal: &Regex,
    re_integer: &Regex,
) -> Result<Option<Token>, ZishError> {
    match c {
        Some(x @ 'T') => {
            state.stage = Stage::Timestamp;
            state.payload.push(x);
            Ok(None)
        }
        Some(x) if !NO_DELIM_END.contains(&x) => {
            state.payload.push(x);
            Ok(None)
        }
        _ => {
            state.in_token = false;
            match state.payload.as_str() {
                "true" => Ok(Some(Token::Primitive(
                    state.location,
                    PrimitiveValue::Bool(true),
                ))),
                "false" => Ok(Some(Token::Primitive(
                    state.location,
                    PrimitiveValue::Bool(false),
                ))),
                "null" => Ok(Some(Token::Primitive(state.location, PrimitiveValue::Null))),
                ustr if re_integer.is_match(ustr) => Ok(Some(Token::Primitive(
                    state.location,
                    PrimitiveValue::Integer(ustr.parse::<i32>().unwrap()),
                ))),
                ustr if re_decimal.is_match(ustr) => Ok(Some(Token::Primitive(
                    state.location,
                    PrimitiveValue::Decimal(BigDecimal::from_str(ustr).unwrap()),
                ))),
                ustr => Err(ZishError::Location(
                    format!("The value {} is not recognized.", ustr),
                    state.location,
                )),
            }
        }
    }
}

fn lex_timestamp(
    c: Option<char>,
    state: &mut State,
    re_timestamp: &Regex,
) -> Result<Option<Token>, ZishError> {
    match c {
        None => lex_timestamp_str(state, re_timestamp),
        Some(x @ 'z') | Some(x @ 'Z') => {
            state.payload.push(x);
            lex_timestamp_str(state, re_timestamp)
        }
        Some(x) if state.payload.matches(':').count() == 3 && NO_DELIM_END.contains(&x) => {
            lex_timestamp_str(state, re_timestamp)
        }
        Some(x) => {
            state.payload.push(x);
            Ok(None)
        }
    }
}

fn lex_timestamp_str(state: &mut State, re_timestamp: &Regex) -> Result<Option<Token>, ZishError> {
    state.in_token = false;
    state.consumed = true;
    match &state.payload {
        tstr if re_timestamp.is_match(tstr) => match DateTime::parse_from_rfc3339(&tstr) {
            Ok(timestamp) => Ok(Some(Token::Primitive(
                state.location,
                PrimitiveValue::Timestamp(timestamp.with_timezone(&Utc)),
            ))),
            Err(e) => Err(ZishError::Location(
                format!("Can't parse the timestamp '{}': {}", tstr, e),
                state.location,
            )),
        },
        tstr => Err(ZishError::Location(
            format!("The timestamp {} is malformed.", tstr),
            state.location,
        )),
    }
}

fn lex_comment(c: Option<char>, state: &mut State) -> Result<Option<Token>, ZishError> {
    match (c, state.prev_c, state.payload.len()) {
        (None, ..) => Err(ZishError::Description(
            "Reached the end of the document without the comment being closed with a '*/'"
                .to_string(),
        )),
        (Some('/'), Some('*'), 0...2) => Err(ZishError::Location(
            "You can't have a comment that's '/*/', an empty comment is '/**/'.".to_string(),
            state.location,
        )),
        (Some('/'), Some('*'), _) => {
            state.in_token = false;
            state.consumed = true;
            Ok(None)
        }
        (Some(x), Some('/'), 1) if x != '*' => Err(ZishError::Location(
            "A comment starts with a '/*'.".to_string(),
            state.location,
        )),
        (Some(x), ..) => {
            state.payload.push(x);
            Ok(None)
        }
    }
}

fn unescape(escaped_str: &str, location: Location) -> Result<String, ZishError> {
    match escaped_str.find('\\') {
        None => Ok(escaped_str.to_string()),
        Some(i) => {
            let head_str: &str = &escaped_str[..i];
            let tail_str: &str = &escaped_str[i + 1..];
            for (k, v) in &[
                ("0", "\u{0000}"),        // NUL
                ("a", "\u{0007}"),        // alert BEL
                ("b", "\u{0008}"),        // backspace BS
                ("t", "\u{0009}"),        // horizontal tab HT
                ("n", "\u{000A}"),        // linefeed LF
                ("f", "\u{000C}"),        // form feed FF
                ("r", "\u{000D}"),        // carriage return CR
                ("v", "\u{000B}"),        // vertical tab VT
                ("\"", "\u{0022}"),       // double quote
                ("'", "\u{0027}"),        // single quote
                ("?", "\u{003F}"),        // question mark
                ("\\", "\u{005C}"),       // backslash
                ("/", "\u{002F}"),        // forward slash
                ("\u{000D}\u{000A}", ""), // empty string
                ("\u{000D}", ""),         // empty string
                ("\u{000A}", ""),         // empty string
            ] {
                if tail_str.starts_with(k) {
                    let un_tail: String = match unescape(&tail_str[k.len()..], location) {
                        Err(e) => return Err(e),
                        Ok(t) => t,
                    };
                    return Ok(format!("{}{}{}", head_str, v, un_tail));
                }
            }

            for (prefix, digits) in [("x", 2), ("u", 4), ("U", 8)].into_iter() {
                if tail_str.starts_with(prefix) {
                    let hex_str: &str = &tail_str[1..1 + *digits];
                    let i: u32 = match u32::from_str_radix(hex_str, 16) {
                        Ok(x) => x,
                        Err(e) => return Err(ZishError::Location(format!("{}", e), location)),
                    };
                    let v: char = match char::from_u32(i) {
                        None => {
                            return Err(ZishError::Description(
                                "Character escape '{}' is not a valid unicode character."
                                    .to_string(),
                            ));
                        }
                        Some(x) => x,
                    };
                    let un_tail: String = match unescape(&tail_str[1 + *digits..], location) {
                        Err(e) => return Err(e),
                        Ok(t) => t,
                    };
                    return Ok(format!("{}{}{}", head_str, v, un_tail));
                };
            }

            Err(ZishError::Description(format!(
                "Can't find a valid string following the first backslash \
                 of '{}'.",
                escaped_str
            )))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{
        lex, lex_no_delim, lex_string, make_re_decimal, make_re_integer, unescape, Location,
        PrimitiveValue, Stage, State, Token, ZishError,
    };
    use regex::Regex;

    #[test]
    fn test_lex_no_delim_timestamp() {
        let c: Option<char> = None;
        let mut state: State = State {
            in_token: true,
            consumed: true,
            stage: Stage::Timestamp,
            payload: "2007-01-01".to_string(),
            prev_c: Some('1'),
            location: Location {
                line: 0,
                character: 10,
            },
        };
        let re_decimal: &Regex = &make_re_decimal();
        let re_integer: &Regex = &make_re_integer();
        let expected: ZishError = ZishError::Location(
            "The value 2007-01-01 is not recognized.".to_string(),
            Location {
                line: 0,
                character: 10,
            },
        );
        match lex_no_delim(c, &mut state, re_decimal, re_integer) {
            Err(e) => {
                assert_eq!(expected, e);
            }
            _ => panic!(""),
        }
    }

    #[test]
    fn test_lex_no_delim_integer() {
        let c: Option<char> = None;
        let mut state: State = State {
            in_token: true,
            consumed: true,
            stage: Stage::NoDelim,
            payload: "2007".to_string(),
            prev_c: Some('7'),
            location: Location {
                line: 0,
                character: 10,
            },
        };
        let re_decimal: &Regex = &make_re_decimal();
        let re_integer: &Regex = &make_re_integer();
        let expected = Some(Token::Primitive(
            state.location,
            PrimitiveValue::Integer(2007),
        ));
        match lex_no_delim(c, &mut state, re_decimal, re_integer) {
            Ok(x) => assert_eq!(expected, x),
            _ => panic!("expected ok"),
        }
    }

    #[test]
    fn test_lex_no_delim_lbracket() {
        let c: Option<char> = Some(' ');
        let mut state: State = State {
            in_token: true,
            consumed: true,
            stage: Stage::NoDelim,
            payload: "(".to_string(),
            prev_c: Some('('),
            location: Location {
                line: 0,
                character: 1,
            },
        };
        let re_decimal: &Regex = &make_re_decimal();
        let re_integer: &Regex = &make_re_integer();
        match lex_no_delim(c, &mut state, re_decimal, re_integer) {
            Err(x) => assert_eq!(
                x,
                ZishError::Location(
                    "The value ( is not recognized.".to_string(),
                    Location {
                        line: 0,
                        character: 1
                    }
                )
            ),
            _ => panic!("expected ok"),
        }
    }

    #[test]
    fn test_re_integer() {
        let re = make_re_integer();
        assert_eq!(re.is_match("2007-01"), false);
    }

    #[test]
    fn test_re_decimal() {
        let re = make_re_decimal();
        assert_eq!(re.is_match("2007-01"), false);
    }

    #[test]
    fn test_lex_string() {
        let c: Option<char> = Some('"');
        let mut state: State = State {
            in_token: true,
            consumed: true,
            stage: Stage::String,
            payload: "\u{005C}\u{0022}".to_string(),
            prev_c: Some('"'),
            location: Location {
                line: 0,
                character: 10,
            },
        };

        let expected = Some(Token::Primitive(
            state.location,
            PrimitiveValue::String("\u{0022}".to_string()),
        ));
        match lex_string(c, &mut state) {
            Ok(x) => assert_eq!(expected, x),
            _ => panic!("expected ok"),
        }
    }

    #[test]
    fn test_unescape() {
        let escaped_str = "\u{005C}\u{0022}";
        let location = Location {
            line: 6,
            character: 7,
        };
        match unescape(escaped_str, location) {
            Ok(x) => assert_eq!(x, "\u{0022}"),
            _ => panic!("failed"),
        }
    }

    #[test]
    fn test_lex_list() {
        let expected: Vec<Token> = vec![
            Token::StartList(Location {
                line: 1,
                character: 1,
            }),
            Token::Primitive(
                Location {
                    line: 1,
                    character: 9,
                },
                PrimitiveValue::String("first".to_string()),
            ),
            Token::FinishList(Location {
                line: 1,
                character: 11,
            }),
            Token::Finish,
        ];
        match lex("[ \"first\" ]") {
            Ok(res) => assert_eq!(res, expected),
            Err(e) => panic!("{:?}", e),
        }
    }
}
