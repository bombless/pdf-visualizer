const INPUT: &'static [u8] = include_bytes!("../test.pdf");
use std::collections::HashMap;
use std::error::Error;
use regex::Regex;


macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        &name[..name.len() - 3]
    }}
}

macro_rules! get_error {
    ($p:expr) => {{
        Err(format!("{}: unrecognized pattern `{}`", function!(), $p).into())
    }}
    
}


struct Parser {
    info: Info
}

struct Info {
    objects: Vec<Object>
}

struct Object {
    id: (u8, u8),
    dict: HashMap<String, Value>,
    stream: Vec<u8>,
}

#[derive(Debug)]
enum Value {
    Key(String),
    Dict(HashMap<String, Value>)
}

fn main() {
    use std::mem::take;

    let mut parser = Parser::new();

    let mut line_count = 1;
    let mut line_offset = 0;
    let mut line_error = 0;
    let mut cache = String::new();
    let mut start_pos = (1, 0);
    let mut mask = false;
    for x in INPUT {
        if x == &0xa {
            line_count += 1;
            line_error = 0;
            line_offset = 0;
            mask = false;
            if cache.is_empty() { continue }
        }
        line_offset += 1;
        if x == &b'%' {
            mask = true;
        }
        if mask { continue }
        if !x.is_ascii() {
            line_error += 1;
            if !cache.is_empty() && line_error <= 1 {
                println!("[{:?}-{:?}]", start_pos, (line_count, line_offset));
                parser.handle_fragment(take(&mut cache)).unwrap();
            }
            start_pos = (line_count, line_offset);
            println!("[{},{}]{:0x}", line_count, line_offset, x);
        } else {
            if x == &7 {
                println!("ring bell line {}", line_count)
            }
            cache.push(*x as _)
        }
    }
}

impl Parser {
    fn new() -> Self {
        Self { info: Info { objects: vec![] }}
    }

    fn handle_fragment(&mut self, input: String) -> Result<(), Box<dyn Error>> {
        let first_line = get_first_line(&input);
        self.handle_obj_start(first_line)?;
        self.parse_dict(&input[..][first_line.len() + 1..])?;
        Ok(())
    }

    fn handle_obj_start(&mut self, starter: &str) -> Result<(u8, u8), Box<dyn Error>> {
        let re = Regex::new(r"^(\d) (\d) obj").unwrap();
        if let Some(captures) = re.captures(starter) {
            let id: (u8, u8) = (captures[1].parse()?, captures[2].parse()?);
            return Ok(id);
        }
        get_error!(starter)
    }

    fn parse_dict(&mut self, stream: &str) -> Result<(usize, HashMap<String, Value>), Box<dyn Error>> {
        let mut offset = self.expect_dict_start(stream)?;
        let mut ret = HashMap::new();
        loop {
            let (len, k) = self.parse_key(&stream[offset..])?;
            offset += len;
            let (len, v) = self.parse_value(&stream[offset..])?;
            offset += len;
            println!("{:?} {:?}", k, v);
            ret.insert(k, v);
        }
    }

    fn expect_dict_start(&mut self, stream: &str) -> Result<usize, Box<dyn Error>> {
        if stream.starts_with("<<\n") {
            Ok(3)
        } else {
            get_error!(get_first_line(stream))
        }
    }

    fn parse_key(&mut self, stream: &str) -> Result<(usize, String), Box<dyn Error>> {
        let re = Regex::new(r"^\s*/(\S+)").unwrap();
        if let Some(captures) = re.captures(stream) {
            Ok((captures[0].len(), captures[1].to_owned()))
        } else {
            get_error!(get_first_line(stream))
        }
    }

    fn parse_value(&mut self, stream: &str) -> Result<(usize, Value), Box<dyn Error>> {
        let re = Regex::new(r"^\s*(\S)").unwrap();
        let captures = re.captures(stream);
        let first_char = captures.map(|x| x[1].as_bytes().get(0).map(u8::to_owned)).flatten();
        // println!("first_char, {:?} ... {}? {}? {:?}", first_char, b'<', b'/', first_char.map(|x| x as char));
        match first_char {
            Some(b'<') => wrap(self.parse_dict(stream), Value::Dict),
            Some(b'/') => wrap(self.parse_key(stream), Value::Key),
            _ => get_error!(get_first_line(stream))
        }
    }



}

fn wrap<U, V>(rs: Result<(usize, U), Box<dyn Error>>, f: impl Fn(U) -> V) -> Result<(usize, V), Box<dyn Error>> {
    match rs {
        Ok((offset, v)) => Ok((offset, f(v))),
        Err(err) => Err(err),
    }
}

fn get_first_line(text: &str) -> &str {
    if let Some(index) = text.find('\n') {
        &text[..index]
    } else {
        &text[..]
    }
}
