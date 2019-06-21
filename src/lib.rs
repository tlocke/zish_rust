extern crate base64;
extern crate bigdecimal;
extern crate chrono;

mod lexer;
use bigdecimal::BigDecimal;
use chrono::prelude::{DateTime, Utc};
use lexer::{Token, PrimitiveValue};
use std::collections::HashMap;
use std::io::{Read, Write};
use std::{error::Error, fmt};
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum ZishError {
    Description(String),
    Location(String, Location),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Location {
    line: u32,
    character: u32,
}

impl Error for ZishError {}

impl fmt::Display for ZishError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ZishError::Description(description) => write!(f, "{}", description),
            ZishError::Location(description, location) => write!(
                f,
                "Problem at line {} and character {}: {}",
                location.line, location.character, description
            ),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Value {
    Timestamp(DateTime<Utc>),
    String(String),
    Bytes(Vec<u8>),
    Integer(i32),
    Bool(bool),
    Null,
    Decimal(BigDecimal),
    List(Vec<Value>),
    Map(HashMap<Key, Value>),
}

impl Value {
    pub fn to_key(&self) -> Option<Key> {
        match self {
            Value::Timestamp(timestamp) => Some(Key::Timestamp(*timestamp)),
            Value::String(string) => Some(Key::String(string.clone())),
            Value::Bytes(bytes) => Some(Key::Bytes(bytes.clone())),
            Value::Integer(int) => Some(Key::Integer(*int)),
            Value::Bool(boolean) => Some(Key::Bool(*boolean)),
            Value::Decimal(dec) => Some(Key::Decimal(dec.clone())),
            _ => None,
        }
    }

    pub fn decimal_from_str(decimal_str: &str) -> Result<Value, ZishError> {
        match BigDecimal::from_str(decimal_str) {
            Ok(v) => Ok(Value::Decimal(v)),
            Err(e) => Err(ZishError::Description(format!("{}", e)))
        }
    }


}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Map(map) => write!(f, "{}", dump_map(map, "")),
            Value::Bool(true) => write!(f, "true"),
            Value::Bool(false) => write!(f, "false"),
            Value::List(list) => write!(f, "{}", dump_list(list, "")),
            Value::Integer(int) => write!(f, "{}", int.to_string()),
            Value::Decimal(decimal) => write!(f, "{}", decimal.to_string()),
            Value::String(string) => write!(f, "\"{}\"", string),
            Value::Bytes(bytes) => write!(f, "'{}'", base64::encode(bytes)),
            Value::Timestamp(timestamp) => write!(f, "{}", timestamp.format("%Y-%m-%dT%H:%M:%SZ")),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Ord, PartialOrd)]
pub enum Key {
    Timestamp(DateTime<Utc>),
    String(String),
    Bytes(Vec<u8>),
    Integer(i32),
    Bool(bool),
    Decimal(BigDecimal),
}

impl Key {
    fn to_value(&self) -> Value {
        match self {
            Key::Timestamp(x) => Value::Timestamp(*x),
            Key::String(x) => Value::String(x.clone()),
            Key::Bytes(x) => Value::Bytes(x.clone()),
            Key::Integer(x) => Value::Integer(*x),
            Key::Bool(x) => Value::Bool(*x),
            Key::Decimal(x) => Value::Decimal(x.clone()),
        }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Key::Bool(true) => write!(f, "true"),
            Key::Bool(false) => write!(f, "false"),
            Key::Integer(int) => write!(f, "{}", int.to_string()),
            Key::Decimal(decimal) => write!(f, "{}", decimal.to_string()),
            Key::String(string) => write!(f, "\"{}\"", string),
            Key::Bytes(bytes) => write!(f, "'{}'", base64::encode(bytes)),
            Key::Timestamp(timestamp) => write!(f, "{}", timestamp.to_rfc3339()),
        }
    }
}

pub fn from_reader<R>(mut r: R) -> Result<Value, ZishError>  where R: Read {
    let mut contents = String::new();
    match r.read_to_string(&mut contents) {
        Err(e) => return Err(ZishError::Description(
                format!("Trouble reading from file: {}.", e))),
        _ => (),
    }
    from_string(&contents)
}

pub fn to_writer<W>(obj: &Value, w: &mut W) -> Result<(), ZishError> where W: Write {
    match w.write_all(to_string(obj).as_bytes()) {
        Err(e) => return Err(ZishError::Description(
                format!("Trouble writing to file: {}.", e))),
        _ => Ok(()),
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum State {
    Start,
    Value,
    StartMap,
    MapKey,
    MapColon,
    MapValue,
    MapComma,
    StartList,
    ListValue,
    ListComma,
}

pub fn from_string(zish_str: &str) -> Result<Value, ZishError> {
    let tokens = match lexer::lex(zish_str) {
        Ok(t) => t,
        Err(e) => return Err(e),
    };

    let mut stack: Vec<Value> = Vec::new();
    let mut state: State = State::Start;
    let mut ret: Option<Value> = None;

    for token in tokens.into_iter() {
        match (state, token) {
            (State::Start, Token::Finish) => {
                return Err(ZishError::Description(
                    "There is no Zish value to parse.".to_string(),
                ));
            }
            (State::Start, Token::StartMap(_)) => {
                state = State::StartMap;
                stack.push(Value::Map(HashMap::new()));
            }
            (State::Start, Token::FinishMap(location)) => {
                return Err(ZishError::Location(
                    "Found a '}', but there isn't an open map.".to_string(),
                    location,
                ));
            }
            (State::Start, Token::Colon(location)) => {
                return Err(ZishError::Location(
                    "Found a ':', but there isn't an open map.".to_string(),
                    location,
                ));
            }
            (State::Start, Token::Comma(location)) => {
                return Err(ZishError::Location(
                    "Found a ',', outside a list or map.".to_string(),
                    location,
                ));
            }
            (State::Start, Token::StartList(_)) => {
                state = State::StartList;
                stack.push(Value::List(Vec::new()));
            }
            (State::Start, Token::FinishList(location)) => {
                return Err(ZishError::Location(
                    "Found a ']', but there's no list to close.".to_string(),
                    location,
                ));
            }
            (State::Start, Token::Primitive(_, x)) => {
                ret = Some(x.into_value());
                state = State::Value;
            }

            (State::Value, Token::StartMap(location)) => return Err(ZishError::Location(
                    "Multiple top-level Zish values aren't allowed. So you can't have a value followed by a map.".to_string(),
                    location,
                )),
            (State::Value,  Token::FinishMap(location))
                => return Err(ZishError::Location(
                "Multiple top-level Zish values aren't allowed. So you can't have anything after the first value.".to_string(),
                location,
            )),
            (State::Value,Token::Colon(location))
                => return Err(ZishError::Location(
                "Multiple top-level Zish values aren't allowed. So you can't have anything after the first value.".to_string(),
                location,
            )),
           (State::Value, Token::Comma(location))
                => return Err(ZishError::Location(
                "Multiple top-level Zish values aren't allowed. So you can't have anything after the first value.".to_string(),
                location,
            )),
            (State::Value,Token::StartList(location))
                => return Err(ZishError::Location(
                "Multiple top-level Zish values aren't allowed. So you can't have anything after the first value.".to_string(),
                location,
            )),
            (State::Value,Token::FinishList(location))
                => return Err(ZishError::Location(
                "Multiple top-level Zish values aren't allowed. So you can't have anything after the first value.".to_string(),
                location,
            )),
            (State::Value,Token::Primitive(location, _) )
                => return Err(ZishError::Location(
                "Multiple top-level Zish values aren't allowed. So you can't have anything after the first value.".to_string(),
                location,
            )),
            (State::Value,Token::Finish) => (),

            (State::StartMap, Token::Primitive(location, PrimitiveValue::Null)) => {
                return Err(ZishError::Location(
                    "null can't be a key in a map.".to_string(),
                    location,
                ));
            }
            (State::StartMap, Token::Primitive(location, primitive_value)) => match stack.pop().unwrap() {
                Value::Map(map) => {
                    let k: Value = primitive_value.into_value();
                    if map.contains_key(&k.to_key().unwrap()) {
                        return Err(ZishError::Location(format!("Duplicate map keys aren't allowed: {}.", k), location));
                    }
                    stack.push(Value::Map(map));
                    stack.push(k);
                    state = State::MapKey;
                }
                _ => panic!("Shouldn't happen."),
            }
            (State::StartMap, Token::StartList(location)) => {
                return Err(ZishError::Location(
                    "A list can't be a key in a map.".to_string(),
                    location,
                ));
            }
            (State::StartMap, Token::StartMap(location)) => {
                return Err(ZishError::Location(
                    "A map can't be a key in a map.".to_string(),
                    location,
                ));
            }
            (State::StartMap, Token::FinishMap(_)) => match stack.pop() {
                Some(Value::Map(map)) => match stack.pop() {
                    None => {
                        ret = Some(Value::Map(map));
                        state = State::Value;
                    }
                    Some(Value::List(mut list)) => {
                        list.push(Value::Map(HashMap::new()));
                        stack.push(Value::List(list));
                        state = State::ListValue;
                    }
                    Some(key) => match stack.pop().unwrap() {
                        Value::Map(mut umap) => {
                            umap.insert(key.to_key().unwrap(), Value::Map(map));
                            stack.push(Value::Map(umap));
                            state = State::MapValue;
                        }
                        _ => panic!("Should never happen."),
                    }
                }
                _ => panic!("Should never happen."),
            }
            (State::StartMap, Token::Colon(location)) => {
                return Err(ZishError::Location(
                    "A map key is expected here, but instead got a ':'".to_string(),
                    location,
                ));
            }
            (State::StartMap, Token::Comma(location)) => {
                return Err(ZishError::Location(
                    "A map key is expected here, but instead got a ','".to_string(),
                    location,
                ));
            }
            (State::StartMap, Token::FinishList(location)) => {
                return Err(ZishError::Location(
                    "A map key is expected here, but instead got a ']'".to_string(),
                    location,
                ));
            }
            (State::StartMap, Token::Finish) => {
                return Err(ZishError::Description(
                    "Reached the end of the document before the map was closed.".to_string())
                );
            }
            (State::MapKey, Token::Colon(_)) => state = State::MapColon,
            (State::MapKey, Token::FinishMap(location)) =>
                return Err(ZishError::Location(
                    "A ':' is expected here, but instead found a '}'".to_string(),
                    location,
                )),
            (State::MapKey, Token::Comma(location)) =>
                return Err(ZishError::Location(
                    "A ':' is expected here, but instead found a ','".to_string(),
                    location,
                )),
            (State::MapKey, Token::Primitive(location, _)) =>
                return Err(ZishError::Location(
                    "A ':' is expected here, but instead found a value.".to_string(),
                    location,
                )),
            (State::MapKey, Token::StartList(location)) =>
                return Err(ZishError::Location(
                    "A ':' is expected here, but instead found a '['.".to_string(),
                    location,
                )),
            (State::MapKey, Token::FinishList(location)) => 
                return Err(ZishError::Location(
                    "A ':' is expected here, but got a ']' instead.".to_string(),
                    location,
                )),
            (State::MapKey, Token::Finish) => 
                return Err(ZishError::Description(
                    "After the key, a ':' was expected, but reached the end of the document instead.".to_string())),
            (State::MapKey, Token::StartMap(location)) =>
                return Err(ZishError::Location(
                    "A ':' is expected here, but got a '{' instead.".to_string(),
                    location,
                )),
            (State::MapColon, Token::StartMap(_)) => { 
                stack.push(Value::Map(HashMap::new()));
                state = State::StartMap;
            }
            (State::MapColon, Token::FinishMap(location)) =>
                return Err(ZishError::Location(
                    "A map value is expected here, but instead found a '}'".to_string(),
                    location,
                )),
            (State::MapColon, Token::Comma(location)) =>
                return Err(ZishError::Location(
                    "A map value is expected here, but instead found a ','".to_string(),
                    location,
                )),
            (State::MapColon, Token::Colon(location)) =>
                return Err(ZishError::Location(
                    "A map value is expected here, but instead found another ':'".to_string(),
                    location,
                )),
            (State::MapColon, Token::Primitive(location, primitive_value)) => {
                let k: Value = stack.pop().unwrap();
                match stack.pop().unwrap() {
                    Value::Map(mut m) => {
                        match m.insert(k.to_key().unwrap(), primitive_value.into_value()) {
                            None => stack.push(Value::Map(m)),
                            _ => return Err(ZishError::Location(format!("Duplicate map keys aren't allowed: {}", k), location)),
                        }
                    }
                    _ => panic!("sh"),
                }
                state = State::MapValue;
            }
            (State::MapColon, Token::StartList(_)) =>  {
                stack.push(Value::List(Vec::new()));
                state = State::StartList;
            }
            (State::MapColon, Token::FinishList(location)) => 
                return Err(ZishError::Location(
                    "A value is expected here, but got a ']' instead.".to_string(),
                    location,
                )),
            (State::MapColon, Token::Finish) => 
                return Err(ZishError::Description(
                    "A value is expected here, but reached the end of the document instead.".to_string()
                )),
            (State::MapValue, Token::Primitive(location, _)) => 
                return Err(ZishError::Location(
                    "A ',' or '}' is expected here, but got a value instead.".to_string(),
                    location,
                )),
            (State::MapValue, Token::StartMap(location)) => 
                return Err(ZishError::Location(
                    "A ',' or '}' is expected here, but got a '{' instead.".to_string(),
                    location,
                )),
            (State::MapValue, Token::FinishMap(_)) => match stack.pop().unwrap() {
                Value::Map(map) => match stack.pop() {
                    None => {
                        state = State::Value;
                        ret = Some(Value::Map(map));
                    }
                    Some(Value::List(mut list)) => {
                        list.push(Value::Map(map));
                        state = State::ListValue;
                    }
                    Some(key) => match stack.pop().unwrap() {
                        Value::Map(mut umap) => {
                            umap.insert(key.to_key().unwrap(), Value::Map(map));
                            stack.push(Value::Map(umap));
                            state = State::MapValue;
                        }
                        _ => panic!("sh"),
                    }
                }
                _ => panic!("{:?}", stack),
            }
            (State::MapValue, Token::Colon(location)) => 
                return Err(ZishError::Location(
                    "A ',' or '}' is expected here, but got a ':' instead.".to_string(),
                    location,
                )),
            (State::MapValue, Token::Comma(_)) => state = State::MapComma,
            (State::MapValue, Token::StartList(location)) => 
                return Err(ZishError::Location(
                    "A ',' or '}' is expected here, but got a '[' instead.".to_string(),
                    location,
                )),
            (State::MapValue, Token::FinishList(location)) => 
                return Err(ZishError::Location(
                    "A ',' or '}' is expected here, but got a ']' instead.".to_string(),
                    location,
                )),
            (State::MapValue, Token::Finish) => 
                return Err(ZishError::Description(
                    "A ',' or '}' is expected here, but reached the end of the document instead.".to_string(),
                )),
            (State::MapComma, Token::Colon(location)) => 
                return Err(ZishError::Location(
                    "A key is expected here, but got a ':' instead.".to_string(),
                    location,
                )),
            (State::MapComma, Token::StartMap(location)) => 
                return Err(ZishError::Location(
                    "A key is expected here, but got a '{' instead. A map can't be a key.".to_string(),
                    location,
                )),
            (State::MapComma, Token::FinishMap(location)) => 
                return Err(ZishError::Location(
                    "A key is expected here, but got a '}' instead. A map can't be a key.".to_string(),
                    location,
                )),
            (State::MapComma, Token::Comma(location)) => 
                return Err(ZishError::Location(
                    "A key is expected here, but got a ',' instead.".to_string(),
                    location,
                )),
            (State::MapComma, Token::StartList(location)) => 
                return Err(ZishError::Location(
                    "A key is expected here, but got a '[' instead. A list can't be a key.".to_string(),
                    location,
                )),
            (State::MapComma, Token::FinishList(location)) => 
                return Err(ZishError::Location(
                    "A key is expected here, but got a ']' instead.".to_string(),
                    location,
                )),
            (State::MapComma, Token::Primitive(location, PrimitiveValue::Null)) => {
                return Err(ZishError::Location(
                    "A null can't be used as a map key.".to_string(),
                    location,
                ));
            }
            (State::MapComma, Token::Primitive(_, primitive_value)) => {
                stack.push(primitive_value.into_value());
                state = State::MapKey;
            }
            (State::MapComma, Token::Finish) => 
                return Err(ZishError::Description(
                    "A key is expected here, but reached the end of the document instead.".to_string()
                )),
            (State::StartList, Token::FinishList(_)) => match stack.pop().unwrap() {
                Value::List(list) => match stack.pop() {
                    None => {
                        ret = Some(Value::List(list));
                        state = State::Value;
                    }
                    Some(Value::List(mut ulist)) => {
                        ulist.push(Value::List(list));
                        stack.push(Value::List(ulist));
                        state = State::ListValue;
                    }
                    Some(key) => match stack.pop().unwrap() {
                        Value::Map(mut map) => {
                            map.insert(key.to_key().unwrap(), Value::List(list));
                            stack.push(Value::Map(map));
                            state = State::MapValue;
                        }
                        _ => panic!("sh"),
                    }
                }
                _ => panic!("sh"),
            }
            (State::StartList, Token::Primitive(_, x)) => match stack.pop().unwrap() {
                Value::List(mut list) => {
                    list.push(x.into_value());
                    stack.push(Value::List(list));
                    state = State::ListValue;
                }
                _ => panic!("expected a list here."),
            }
            (State::StartList, Token::StartList(_)) =>
                stack.push(Value::List(Vec::new())),
            (State::StartList, Token::StartMap(_)) => {
                    stack.push(Value::Map(HashMap::new()));
                    state = State::StartMap;
            }
            (State::StartList, Token::FinishMap(location)) => {
                return Err(ZishError::Location(
                    "Expected a value here, but got '}'.".to_string(),
                    location,
                ));
            }
            (State::StartList, Token::Colon(location)) => {
                return Err(ZishError::Location(
                    "Expected a value here, but got ':'.".to_string(),
                    location,
                ));
            }
            (State::StartList, Token::Comma(location)) => {
                return Err(ZishError::Location(
                    "Expected a value here, but got ','.".to_string(),
                    location,
                ));
            }
            (State::StartList, Token::Finish) => {
                return Err(ZishError::Description(
                    "The document finished before the list was closed.".to_string(),
                ));
            }
            (State::ListValue, Token::Finish) => {
                return Err(ZishError::Description(
                            "Expected a ',' or a ']', but reached the end of the document instead."
                                .to_string()
                        ));
            }
            (State::ListValue, Token::Comma(_))  => state = State::ListComma,
            (State::ListValue, Token::FinishList(_) )=> match stack.pop().unwrap() {
                Value::List(list) => match stack.pop() {
                    None => {
                        ret = Some(Value::List(list));
                        state = State::Value;
                    }
                    Some(Value::List(mut l)) => {
                        l.push(Value::List(list));
                        stack.push(Value::List(l));
                    }
                    Some(key) => match stack.pop().unwrap() {
                        Value::Map(mut map) => {
                            map.insert(key.to_key().unwrap(), Value::List(list));
                            stack.push(Value::Map(map));
                        }
                        _ => panic!("sh"),
                    }
                }
                _ => panic!("sh"),
            }
            (State::ListValue, Token::FinishMap(location)) => {
                return Err(ZishError::Location(
                    "Expected a ',' or a ']' here, but got '}'".to_string(),
                    location,
                ));
            }
            (State::ListValue, Token::Colon(location)) => {
                return Err(ZishError::Location(
                    "Expected a ',' or a ']' here, but got ':'".to_string(),
                    location,
                ));
            }
            (State::ListValue, Token::StartMap(location)) => {
                return Err(ZishError::Location(
                    "Expected a ',' or a ']' here, but got '{'".to_string(),
                    location,
                ));
            }
            (State::ListValue, Token::StartList(location)) => {
                return Err(ZishError::Location(
                    "Expected a ',' or a ']' here, but got '['".to_string(),
                    location,
                ));
            }
            (State::ListValue, Token::Primitive(location, _)) => {
                return Err(ZishError::Location(
                    "Expected a ',' or a ']' here, but got a value.".to_string(),
                    location,
                ));
            }
            (State::ListComma, Token::StartMap(_))  => {
                let map: HashMap<Key, Value> = HashMap::new();
                stack.push(Value::Map(map)); 
                state = State::StartMap;
            }
            (State::ListComma, Token::FinishMap(location))  => {
                return Err(ZishError::Location(
                    "Expected a value here, but found a '}'.".to_string(),
                    location,
                ));
            }
            (State::ListComma, Token::Colon(location))  => {
                return Err(ZishError::Location(
                    "Expected a value here, but found a ':'.".to_string(),
                    location,
                ));
            }
            (State::ListComma, Token::Comma(location))  => {
                return Err(ZishError::Location(
                    "Expected a value here, but got a ','.".to_string(),
                    location,
                ));
            }
            (State::ListComma, Token::StartList(_))  => {
                let list: Vec<Value> = Vec::new();
                stack.push(Value::List(list)); 
                state = State::StartList;
            }
            (State::ListComma, Token::Primitive(_, val))  => match stack.pop().unwrap() {
                Value::List(mut list) => {
                    list.push(val.into_value());
                    stack.push(Value::List(list));
                    state = State::ListValue;
                }
                _ => panic!("expected a list here."),
            }
            (State::ListComma, Token::FinishList(location))  => {
                return Err(ZishError::Location(
                    "Trailing commas aren't allowed in Zish.".to_string(),
                    location,
                ));
            }
            (State::ListComma, Token::Finish)  => {
                return Err(ZishError::Description(
                    "Expected a value, but reached the end of the document.".to_string())
                );
            }
        };
    }
    match ret {
        Some(v) => Ok(v),
        None => Err(ZishError::Description("Can't find a zish value in the document.".to_string())),
    }
}

pub fn to_string(obj: &Value) -> String {
    dump(obj, "")
}

fn dump(obj: &Value, indent: &str) -> String {
    match obj {
        Value::Map(map) => dump_map(map, indent),
        Value::List(list) => dump_list(list, indent),
        _ => obj.to_string(),
    }
}

fn dump_map(map: &HashMap<Key, Value>, indent: &str) -> String {
    let new_indent: String = format!("{}{}", indent, "  ");
    let mut items: Vec<String> = Vec::new();
    let pref: String = format!("\n{}", new_indent);
    let mut keys: Vec<&Key> =  map.keys().collect::<Vec<&Key>>();
    keys.sort();
    for k in keys {
        let v: &Value = map.get(k).unwrap();
        items.push(format!("{}{}: {}", pref, k.to_value(), dump(v, &new_indent)));
    }

    match items.len() {
        0 => "{}".to_string(),
        _ => format!("{{{}\n{}}}", items.join(","), indent),
    }
}

fn dump_list(list: &Vec<Value>, indent: &str) -> String {
    match list.len() {
        0 => "[]".to_owned(),
        _ => {
            let new_indent: String = indent.to_owned() + "  ";
            let mut pref: String = "\n".to_owned();
            pref.push_str(&new_indent);
            let mut strs: Vec<String> = Vec::new();
            for v in list {
                strs.push(format!("{}{}", pref, dump(v, &new_indent)));
            }
            let b: String = strs.join(",");
            format!("[{}\n{}]", b, indent)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{dump_map, to_writer, to_string, from_reader, Location, ZishError, Value, Key};
    use std::collections::HashMap;
    use chrono::{DateTime, Utc, TimeZone};
    use bigdecimal::BigDecimal;
    use std::str::FromStr;

    #[test]
    fn test_from_reader() {
        let v: Value = Value::Map(HashMap::new());
        let d: &[u8] = "{}".as_bytes(); 
        let a: Value = from_reader(d).unwrap();
        assert_eq!(a, v);
    }

    #[test]
fn test_dump() {
    let mut w: Vec<u8> = Vec::new();
    let v: Value = Value::Map(HashMap::new());
    to_writer(&v, &mut w).unwrap();

    let s: String = String::from_utf8(w).unwrap();
    assert_eq!(s, "{}");
}


    #[test]
    fn test_deserialize() {
        // {"center":{"x":1.0, "y":12.5}, "radius":3}
        let mut nested_map_1: HashMap<Key, Value> = HashMap::new();
        nested_map_1.insert(Key::String("x".to_string()), Value::Decimal(BigDecimal::from_str("1.0").unwrap()));
        nested_map_1.insert(Key::String("y".to_string()), Value::Decimal(BigDecimal::from_str("12.5").unwrap()));
        let mut nested_map: HashMap<Key, Value> = HashMap::new();
        nested_map.insert(Key::String("center".to_string()), Value::Map(nested_map_1));
        nested_map.insert(Key::String("radius".to_string()), Value::Integer(3));

        // "{ \"first\" : \"Tom\" , \"last\": \"Riddle\" }",
        let mut map_b: HashMap<Key, Value> = HashMap::new();
        map_b.insert(Key::String("first".to_string()), Value::String("Tom".to_string()));
        map_b.insert(Key::String("last".to_string()), Value::String("Riddle".to_string()));

        // { "":42 }
        let mut map_c: HashMap<Key, Value> = HashMap::new();
        map_c.insert(Key::String("".to_string()), Value::Integer(42));

        let tests: Vec<(&str, Result<Value, ZishError>)> = vec![
            //
            // Timestamp
            //

            // Error: Seconds are not optional
            (
                "2007-02-23T12:14Z",
                Err(ZishError::Location(
                    "The timestamp 2007-02-23T12:14Z is malformed.".to_string(),
                    Location {line: 1, character: 17})),
            ),

            // A timestamp with millisecond precision and PST local time
            (
                "2007-02-23T12:14:33.079-08:00",
                Ok(Value::Timestamp(DateTime::parse_from_rfc3339( "2007-02-23T12:14:33.079-08:00").unwrap().with_timezone(&Utc)))
            ),

            // The same instant in UTC ("zero" or "zulu")
            (
                "2007-02-23T20:14:33.079Z",
                Ok(Value::Timestamp(DateTime::parse_from_rfc3339("2007-02-23T20:14:33.079Z").unwrap().with_timezone(&Utc))),
            ),

            // The same instant, with explicit local offset
            (
                "2007-02-23T20:14:33.079+00:00",
                Ok(Value::Timestamp(DateTime::parse_from_rfc3339("2007-02-23T20:14:33.079+00:00").unwrap().with_timezone(&Utc))),
            ),

            // The same instant, with unknown local offset
            (
                "2007-02-23T20:14:33.079-00:00",
                Ok(Value::Timestamp(DateTime::parse_from_rfc3339("2007-02-23T20:14:33.079-00:00").unwrap().with_timezone(&Utc))),
            ),

            // Happy New Year in UTC, unknown local offset
            (
                "2007-01-01T00:00:00-00:00",
                Ok(Value::Timestamp(DateTime::parse_from_rfc3339("2007-01-01T00:00:00-00:00").unwrap().with_timezone(&Utc))),
            ),

            // Error: Must have a time
            (
                "2007-01-01",
                Err(ZishError::Location(
                    "The value 2007-01-01 is not recognized.".to_string(),
                    Location { line: 1, character: 11 }))
            ),


            // The same value, different syntax.
            // Shouldn't actually be an error, but arrow says it is.
            (
                "2007-01-01T",
                Err(ZishError::Location(
                    "The timestamp 2007-01-01T is malformed.".to_string(),
                    Location { line: 1, character: 12}))
            ),

            // The same instant, with months precision, unknown local offset
            // Shouldn't actually be an error, but arrow says it is.
            (
                "2007-01T",
                Err(ZishError::Location(
                  "The timestamp 2007-01T is malformed.".to_string(),
                        Location { line: 1, character: 9}))
            ),

            // The same instant, with years precision, unknown local offset
            // Shouldn't actually be an error, but arrow says it is.
            (
                "2007T",
                Err(ZishError::Location(
                        "The timestamp 2007T is malformed.".to_string(),
                        Location { line: 1, character: 6}))
            ),

            // Error: Must have a time part
            (
                "2007-02-23",
                Err(ZishError::Location(
                    "The value 2007-02-23 is not recognized.".to_string(),
                    Location { line: 1, character: 11}))
            ),

            // Error: Must have seconds
            (
                "2007-02-23T00:00Z",
                Err(ZishError::Location(
                    "The timestamp 2007-02-23T00:00Z is malformed.".to_string(),
                    Location { line: 1, character: 17}))
            ),

            // Error: Must have seconds
            (
                "2007-02-23T00:00+00:00",
                Err(ZishError::Location(
                    "The timestamp 2007-02-23T00:00+00:00 is malformed.".to_string(),
                    Location { line: 1, character: 23}))
            ),

            // The same instant, with seconds precision
            (
                "2007-02-23T00:00:00-00:00",
                Ok(Value::Timestamp(DateTime::parse_from_rfc3339("2007-02-23T00:00:00-00:00").unwrap().with_timezone(&Utc))),
            ),

            // Not a timestamp, but an int
            ("2007", Ok(Value::Integer(2007))),

            // ERROR: Must end with 'T' if not whole-day precision, this results
            // as an invalid-numeric-stopper error
            (
                "2007-01",
                Err(ZishError::Location(
                     "The value 2007-01 is not recognized.".to_string(), Location { line: 1, character: 8})),
            ),

            // ERROR: Must have at least one digit precision after decimal point.
            (
                "2007-02-23T20:14:33.Z",
                Err(ZishError::Location(
                    "The timestamp 2007-02-23T20:14:33.Z is malformed.".to_string(),
                    Location { line: 1, character: 21 })),
            ),

            /*
            * Null Values
            */

            (
                "null", Ok(Value::Null)
            ),

            /*
            * Booleans
            */

            (
                "true", Ok(Value::Bool(true))
            ),

            (
                "false", Ok(Value::Bool(false))
            ),

            /*
            * Integers
            */

            // Zero.  Surprise!

            ("0", Ok(Value::Integer(0))),

            // ...the same value with a minus sign
            ("-0", Ok(Value::Integer(0))),

            // A normal int
            ("123", Ok(Value::Integer(123))),

            // Another negative int
            ("-123", Ok(Value::Integer(-123))),

            // Error: An int can't be denoted in hexadecimal
            (
                "0xBeef",
                Err(ZishError::Location(
                    "The value 0xBeef is not recognized.".to_string(),
                    Location { line: 1, character: 7})),
            ),

            // Error: An int can't be denoted in binary
            (
                "0b0101",
                Err(ZishError::Location(
                    "The value 0b0101 is not recognized.".to_string(), Location { line: 1, character: 7})),
            ),

            // Error: An int can't have underscores
            (
                "1_2_3",
                Err(ZishError::Location(
                    "The value 1_2_3 is not recognized.".to_string(),
                    Location { line: 1, character: 6})),
            ),

            // Error: An int can't be denoted in hexadecimal with underscores
            (
                "0xFA_CE",
                Err(ZishError::Location(
                    "The value 0xFA_CE is not recognized.".to_string(),
                    Location { line: 1, character: 8})),
            ),

            // Error: An int can't be denoted in binary with underscores
            (
                "0b10_10_10",
                Err(ZishError::Location(
                    "The value 0b10_10_10 is not recognized.".to_string(),
                    Location { line: 1, character: 11 })),
            ),

            // ERROR: leading plus not allowed
            (
                "+1",
                Err(ZishError::Location(
                    "The value +1 is not recognized.".to_string(),
                    Location { line: 1, character: 3 })),
            ),

            // ERROR: leading zeros not allowed (no support for octal notation)
            (
                "0123",
                Err(ZishError::Location(
                    "The value 0123 is not recognized.".to_string(),
                    Location { line: 1, character: 5 })),
            ),

            // ERROR: trailing underscore not allowed
            (
                "1_",
                Err(ZishError::Location(
                    "The value 1_ is not recognized.".to_string(),
                    Location { line: 1, character: 3 })),
            ),

            // ERROR: consecutive underscores not allowed
            (
                "1__2",
                Err(ZishError::Location(
                    "The value 1__2 is not recognized.".to_string(),
                    Location { line: 1, character: 5})),
            ),

            // ERROR: underscore can only appear between digits
            // (the radix prefix is not a digit)
            (
                "0x_12",
                Err(ZishError::Location(
                    "The value 0x_12 is not recognized.".to_string(),
                    Location { line: 1, character: 6}))
            ),

            // ERROR: ints cannot start with underscores
            (
                "_1",
                Err(ZishError::Location(
                    "The value _1 is not recognized.".to_string(),
                    Location { line: 1, character: 3})),
            ),

            /*
            * Real Numbers
            */

            // Type is decimal
            ("0.123", Ok(Value::Decimal(BigDecimal::from_str("0.123").unwrap()))),

            // Type is decimal
            ("-0.12e4", Ok(Value::Decimal(BigDecimal::from_str("-0.12e4").unwrap()))),

            // ERROR: Exponent not denoted by 'd'
            (
                "0d0",
                Err(ZishError::Location(
                    "The value 0d0 is not recognized.".to_string(),
                    Location { line: 1, character: 4 }))
            ),

            // Zero with uppercase 'E' in exponent.
            ("0E0", Ok(Value::Decimal(BigDecimal::from_str("0").unwrap()))),

            // Zero as decimal
            ("0e0", Ok(Value::Decimal(BigDecimal::from_str("0").unwrap()))),

            // Error: Zero as decimal can't have uppercase 'D' in exponent.
            (
                "0D0",
                Err(ZishError::Location(
                    "The value 0D0 is not recognized.".to_string(),
                    Location { line: 1, character: 4}))
            ),

            //   ...the same value with different notation
            ("0.", Ok(Value::Decimal(BigDecimal::from_str("0").unwrap()))),

            // Negative zero float   (distinct from positive zero)
            ("-0e0", Ok(Value::Decimal(BigDecimal::from_str("-0").unwrap()))),

            // Error: Negative zero decimal with 'd' in expenent.
            (
                "-0d0",
                Err(ZishError::Location(
                        "The value -0d0 is not recognized.".to_string(),
                        Location { line: 1, character: 5 })),
            ),

            //   ...the same value with different notation
            ("-0.", Ok(Value::Decimal(BigDecimal::from_str("-0").unwrap()))),

            // Decimal maintains precision: -0. != -0.0
            ("-0e-1", Ok(Value::Decimal(BigDecimal::from_str("-0.0").unwrap()))),

            // Error: Decimal can't have underscores
            (
                "123_456.789_012",
                Err(ZishError::Location(
                    "The value 123_456.789_012 is not recognized.".to_string(),
                    Location { line: 1, character: 16 })),
            ),

            // ERROR: underscores may not appear next to the decimal point
            (
                "123_._456",
                Err(ZishError::Location(
                    "The value 123_._456 is not recognized.".to_string(),
                    Location { line: 1, character: 10}))
            ),

            // ERROR: consecutive underscores not allowed
            (
                "12__34.56",
                Err(ZishError::Location(
                    "The value 12__34.56 is not recognized.".to_string(),
                    Location { line: 1, character: 10 }))
            ),

            // ERROR: trailing underscore not allowed
            (
                "123.456_",
                Err(ZishError::Location(
                    "The value 123.456_ is not recognized.".to_string(),
                    Location { line: 1, character: 9 }))
            ),

            // ERROR: underscore after negative sign not allowed
            (
                "-_123.456",
                Err(ZishError::Location(
                    "The value -_123.456 is not recognized.".to_string(),
                    Location { line: 1, character: 10}))
            ),

            // ERROR: the symbol '_123' followed by an unexpected dot
            (
                "_123.456",
                Err(ZishError::Location(
                    "The value _123.456 is not recognized.".to_string(),
                    Location { line: 1, character: 9})),
            ),


            /*
            * Strings
            */

            // An empty string value
            ("\"\"", Ok(Value::String("".to_string()))),

            // A normal string
            ("\" my string \"", Ok(Value::String(" my string ".to_string()))),

            // Contains one double-quote character
            ("\"\\\"\"", Ok(Value::String("\"".to_string()))),

            // Contains one unicode character
            ("\"\\uABCD\"", Ok(Value::String("\u{ABCD}".to_string()))),

            // ERROR: Invalid blob
            (
                "xml::\"<e a='v'>c</e>\"",
                Err(ZishError::Location(
                        "The value xml is not recognized.".to_string(),
                        Location { line: 1, character: 4})),
            ),

            // Set with one element
            (
                "( \"hello\rworld!\"  )",
                Err(ZishError::Location(
                        "The value ( is not recognized.".to_string(),
                        Location { line: 1, character: 2 }))
            ),

            // The exact same set
            (
                "(\"hello world!\")",
                Err(ZishError::Location(
                    "The value (\"hello is not recognized.".to_string(),
                    Location { line: 1, character: 8 }))
            ),

            // This Zish value is a string containing three newlines. The
            // serialized form's first newline is escaped into nothingness.
            ("\"\
    The first line of the string.
    This is the second line of the string,
    and this is the third line.
    \"", Ok(Value::String("The first line of the string.
    This is the second line of the string,
    and this is the third line.
    ".to_string()))),

            /*
            * Blobs
            */

            // A valid blob value with zero padding characters.
            (
                "'
    +AB/
    '",
                Ok(Value::Bytes(base64::decode("+AB/").unwrap()))
            ),

            // A valid blob value with one required padding character.
            (
                "'VG8gaW5maW5pdHkuLi4gYW5kIGJleW9uZCE='",
                Ok(Value::Bytes(base64::decode("VG8gaW5maW5pdHkuLi4gYW5kIGJleW9uZCE=").unwrap()))
            ),

            // ERROR: Incorrect number of padding characters.
            (
                "' VG8gaW5maW5pdHkuLi4gYW5kIGJleW9uZCE== '",
                Err(ZishError::Location("Encoded text cannot have a 6-bit remainder.".to_string(), Location { line: 1, character: 41 }))
            ),

            // ERROR: Padding character within the data.
            (
                "' VG8gaW5maW5pdHku=Li4gYW5kIGJleW9uZCE= '",
                Err(ZishError::Location(
                    "Encoded text cannot have a 6-bit remainder.".to_string(),
                    Location { line: 1, character: 41}))
            ),

            // A valid blob value with two required padding characters.
            (
                "' dHdvIHBhZGRpbmcgY2hhcmFjdGVycw== '",
                Ok(Value::Bytes(base64::decode("dHdvIHBhZGRpbmcgY2hhcmFjdGVycw==").unwrap())),
            ),

            // ERROR: Invalid character within the data.
            (
                "' dHdvIHBhZGRpbmc_gY2hhcmFjdGVycw= '",
                Err(ZishError::Location(
                        "Invalid byte 95, offset 15.".to_string(), Location { line: 1, character: 36 }))
            ),


            /*
             * Maps
             */

            // An empty Map value
            (
                "{ }",
                Ok(Value::Map(HashMap::new()))
            ),

            // Map with two fields
            (
                "{ \"first\" : \"Tom\" , \"last\": \"Riddle\" }",
                Ok(Value::Map(map_b))
            ),

            // Nested map
            (
                "{\"center\":{\"x\":1.0, \"y\":12.5}, \"radius\":3}",
                Ok(Value::Map(nested_map))
            ),

            // Multiple top-level values
            (
                "{} 3",
                Err(ZishError::Location(
                    "Multiple top-level Zish values aren't allowed. So you can't have anything after the first value.".to_string(), Location { line: 1, character: 5})),
            ),

            // Map with only opening brace
            (
                "{",
                Err(ZishError::Description(
                    "Reached the end of the document before the map was closed.".to_string())),
            ),

            // Map with only opening brace and value
            (
                "{ \"Etienne\"",
                Err(ZishError::Description(
                    "After the key, a ':' was expected, but reached the end of the document instead.".to_string()))
            ),

            // Trailing comma is invalid in Zish (like JSON)
            (
                "{ x:1, }",
                Err(ZishError::Location(
                        "The value x is not recognized.".to_string(),
                        Location { line: 1, character: 4}))
            ),

            // A map value containing a field with an empty name
            (
                "{ \"\":42 }",
                Ok(Value::Map(map_c))
            ),

            // ERROR: repeated name 'x'
            (
                "{ \"x\":1, \"x\":null }",
                Err(ZishError::Location(
                    "Duplicate map keys aren't allowed: \"x\"".to_string(),
                    Location { line: 1, character: 18 }))
            ),

            // ERROR: missing field between commas
            (
                "{ \"x\":1, , }",
                Err(ZishError::Location(
                    "A key is expected here, but got a ',' instead.".to_string(),
                    Location { line: 1, character: 10 }))
            ),

            // ERROR: Integer after value in a map
            (
                "{ \"x\": 1 4 }",
                Err(ZishError::Location(
                    "A ',' or '}' is expected here, but got a value instead.".to_string(),
                    Location { line: 1, character:11 }))
            ),



            /*
             * Lists
             */

            // An empty list value
            (
                "[]",
                Ok(Value::List(vec![]))
            ),

            // List of three ints
            (
                "[1, 2, 3]",
                Ok(Value::List(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]))
            ),

            // List of an int and a string
            (
                "[ 1 , \"two\" ]",
                Ok(Value::List(vec![Value::Integer(1), Value::String("two".to_string())]))
            ),

            // Nested list
            (
                "[\"a\" , [\"b\"]]",
                Ok(Value::List(vec![Value::String("a".to_string()), Value::List(vec![Value::String("b".to_string())])]))
            ),

            // Trailing comma is invalid in Zish (like JSON)
            (
                "[ 1.2, ]",
                Err(ZishError::Location(
                    "Trailing commas aren't allowed in Zish.".to_string(),
                    Location { line: 1, character: 8}))
            ),

            // ERROR: missing element between commas
            (
                "[ 1, , 2 ]",
                Err(ZishError::Location(
                    "Expected a value here, but got a ','.".to_string(),
                    Location { line: 1, character: 6}))
            ),

            // Input string ending in a newline
            ("{}\n", Ok(Value::Map(HashMap::new()))),

            // Check good error for string that isn't finished
            (
                "\"",
                Err(ZishError::Location(
                    "Parsing a string but can't find the ending '\"'. The first part of the string is: ".to_string(),
                    Location { line: 1, character: 2}))
            ),

            // Check good error for map when ':' is expected.
            (
                "{\"num\" 1}",
                Err(ZishError::Location(
                        "A ':' is expected here, but instead found a value.".to_string(),
                        Location { line: 1, character: 9 }))
            ),

            // Error: List can't be a key in a map.
            (
                "{[\"num\", 1]: 1}",
                Err(ZishError::Location(
                        "A list can't be a key in a map.".to_string(),
                        Location { line: 1, character: 2},
                        ))
            ),

            // Invalid token after beginning of map.
            (
                "{:: 1}",
                Err(ZishError::Location(
                        "A map key is expected here, but instead got a ':'".to_string(),
                        Location { line: 1, character: 2}))
            ),

            // Error: Empty string
            (
                "",
                Err(ZishError::Description("There is no Zish value to parse.".to_string()))
            )
        ];
        for (zish_str, res) in tests {
            println!("zish str {}", zish_str);
            println!("zish res {:?}", res);
            let a: Result<Value, ZishError> = from_reader(zish_str.as_bytes());
            match (&res, &a) {
                (Ok(x), Ok(y)) => assert_eq!(x, y),
                (Err(x), Err(y)) => assert_eq!(x, y),
                _ => panic!("\nExpected: {:?}\nActual: {:?}\n", res, a),
            }
        }
    }

    #[test]
    fn test_dump_map() {
        let mut map: HashMap<Key, Value> = HashMap::new();
        map.insert(Key::Integer(3), Value::Integer(4));
        assert_eq!(dump_map(&map, ""), "{\n  3: 4\n}");
    }

    #[test]
    fn test_dump_timestamp() {
        let v = Value::Timestamp(Utc.ymd(2017, 7, 16).and_hms(14, 5, 0));
        assert_eq!(format!("{}", v), "2017-07-16T14:05:00Z");
    }

    #[test]
    fn test_serialize() {
        let mut map_1: HashMap<Key, Value> = HashMap::new();
        map_1.insert(Key::String("title".to_string()), Value::String("A Hero of Our Time".to_string()));
        map_1.insert(Key::String("read_date".to_string()), Value::Timestamp(Utc.ymd(2017, 7, 16).and_hms(14, 5, 0)));
        map_1.insert(Key::String("would_recommend".to_string()), Value::Bool(true));
        map_1.insert(Key::String("description".to_string()), Value::Null);
        map_1.insert(Key::String("number_of_novellas".to_string()), Value::Integer(5));
        map_1.insert(Key::String("price".to_string()), Value::Decimal(BigDecimal::from_str("7.99").unwrap()));
        map_1.insert(Key::String("weight".to_string()), Value::Decimal(BigDecimal::from_str("6.88").unwrap()));
        map_1.insert(Key::String("key".to_string()), Value::Bytes(b"kshhgrl".to_vec()));
        map_1.insert(Key::String("tags".to_string()), Value::List(vec![Value::String("russian".to_string()), Value::String("novel".to_string()), Value::String("19th centuary".to_string())]));

        let mut map_2: HashMap<Key, Value> = HashMap::new();
        map_2.insert(Key::Integer(1), Value::Integer(2));
        map_2.insert(Key::String("three".to_string()), Value::String("four".to_string()));

        let tests: Vec<(Value, &str)> = vec![
            (
                Value::Map(map_1),
                "{
  \"description\": null,
  \"key\": 'a3NoaGdybA==',
  \"number_of_novellas\": 5,
  \"price\": 7.99,
  \"read_date\": 2017-07-16T14:05:00Z,
  \"tags\": [
    \"russian\",
    \"novel\",
    \"19th centuary\"
  ],
  \"title\": \"A Hero of Our Time\",
  \"weight\": 6.88,
  \"would_recommend\": true
}"),

            (Value::List(Vec::new()), "[]"),

            (Value::decimal_from_str("0.000001").unwrap(), "0.000001"),

            (Value::decimal_from_str("0E-8").unwrap(), "0.00000000"),

            (Value::Map(map_2), "{
  \"three\": \"four\",
  1: 2
}")
        ];
        for (res, zish_str) in tests {
            println!("zish res {:?}", res);
            println!("zish str {}", zish_str);
            let a: String  = to_string(&res);
            assert_eq!(a, zish_str);
        }
    }
}
