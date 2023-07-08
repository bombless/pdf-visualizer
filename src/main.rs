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
    Dict(HashMap<String, Value>),
    Ref((u8, u8)),
    String(String),
    Number(u32),
    List(Vec<Value>),
}

fn main() {
    use std::mem::take;

    let mut parser = Parser::new();
    return println!("{:?}", parser.handle_fragment(r"4 0 obj
    <<
      /Type /Font
      /Subtype /CIDFontType2
      /BaseFont /ABCDEF#2BKaiTi
      /CIDSystemInfo <<
        /Registry (Adobe)
        /Ordering (Identity)
        /Supplement 0
      >>
      /FontDescriptor 5 0 R
      /DW 0
      /CIDToGIDMap /Identity
      /W [1430 1430 1000 4019 4019 1000]
    >>
endobj
    ".into()));

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
        // println!(":{}", input);
        let mut offset = 0;
        loop {
            while input[..][offset..].starts_with("\n\n") {
                offset += 1;
            }
            let try_object_start = self.expect_obj_start(&input[..][offset..]);
            let (len, id) = if let Ok(x) = try_object_start {
                x
            } else {
                // println!("remaining {:?}", &input[..][offset..]);
                return Ok(())
            };
            offset += len;
            let (len, dict) = self.parse_dict(&input[..][offset..]).unwrap();
            offset += len;
            println!("{:?}", dict);
            let object = Object {
                id,
                dict,
                stream: vec![],
            };
            self.info.objects.push(object);
            let (len, ()) = self.expect_obj_end(&input[..][offset..])?;
            offset += len;
            println!("remaining <<<<<<\n{}\n>>>>>>>>", &input[..][offset..]);
        }
        get_error!(&input[..][offset..])
    }

    fn expect_obj_start(&mut self, starter: &str) -> Result<(usize, (u8, u8)), Box<dyn Error>> {
        let re = Regex::new("^(\\d) (\\d) obj\n").unwrap();
        if let Some(captures) = re.captures(starter) {
            let id: (u8, u8) = (captures[1].parse()?, captures[2].parse()?);
            return Ok((captures[0].len(), id));
        }
        get_error!(starter)
    }

    fn expect_obj_end(&mut self, tail: &str) -> Result<(usize, ()), Box<dyn Error>> {
        if tail.starts_with("\nendobj\n") {
            Ok(("\nendobj\n".len(), ()))
        } else {
            get_error!(get_heading(tail))
        }
    }

    fn parse_dict(&mut self, stream: &str) -> Result<(usize, HashMap<String, Value>), Box<dyn Error>> {
        let mut offset = self.expect_dict_start(stream)?;
        let mut ret = HashMap::new();
        loop {
            // println!("parsing {:?}", get_heading(&stream[offset..]));
            let (len, k) = match self.parse_key(&stream[offset..]) {
                Ok(ok) => ok,
                _ => {
                    let re = Regex::new("^\\s*>>").unwrap();
                    if let Some(capture) = re.captures(&stream[offset..]) {
                        return Ok((offset + capture[0].len(), ret))
                    }
                    return get_error!(get_first_line(&stream[offset..]))
                }
            };
            offset += len;
            let (len, v) = self.parse_value(&stream[offset..])?;
            offset += len;
            println!("{:?} {:?}", k, v);
            ret.insert(k, v);
        }
    }

    fn expect_dict_start(&mut self, stream: &str) -> Result<usize, Box<dyn Error>> {
        let trimed = stream.trim_start();
        if trimed.starts_with("<<") {
            Ok(stream.len() - trimed.len() + 2)
        } else {
            get_error!(get_first_line(stream))
        }
    }

    fn parse_key(&mut self, stream: &str) -> Result<(usize, String), Box<dyn Error>> {
        let re = Regex::new(r"^\s*/(\S+)").unwrap();
        if let Some(captures) = re.captures(stream) {
            Ok((captures[0].len(), captures[1].to_owned()))
        } else {
            get_error!(get_heading(stream))
        }
    }

    fn parse_ref(&mut self, stream: &str) -> Result<(usize, (u8, u8)), Box<dyn Error>> {
        let re = Regex::new(r"^\s*(\d+)\s+(\d+)\s+R").unwrap();
        if let Some(captures) = re.captures(stream) {
            let id: (u8, u8) = (captures[1].parse()?, captures[2].parse()?);
            return Ok((captures[0].len(), id));
        }
        get_error!(get_first_line(stream))
    }

    fn parse_string(&mut self, stream: &str) -> Result<(usize, String), Box<dyn Error>> {
        let re = Regex::new(r"^\s*\(([^)]*)\)").unwrap();
        if let Some(captures) = re.captures(stream) {
            return Ok((captures[0].len(), captures[1].into()));
        }
        get_error!(get_first_line(stream))
    }

    fn parse_number(&mut self, stream: &str) -> Result<(usize, u32), Box<dyn Error>> {
        let re = Regex::new(r"^\s*(\d+)").unwrap();
        if let Some(captures) = re.captures(stream) {
            return Ok((captures[0].len(), captures[1].parse().unwrap()));
        }
        get_error!(get_first_line(stream))
    }

    fn parse_list(&mut self, stream: &str) -> Result<(usize, Vec<Value>), Box<dyn Error>> {
        let mut ret = vec![];
        let (mut offset, ())  = self.expect_list_start(stream)?;
        
        while let Ok((len, v)) = self.parse_value(&stream[offset..]) {
            offset += len;
            ret.push(v);
        }
        let (len, ()) = self.expect_list_end(&stream[offset..])?;
        Ok((offset + len, ret))
    }

    fn expect_list_start(&mut self, stream: &str) -> Result<(usize, ()), Box<dyn Error>> {
        let trimed = stream.trim_start();
        if trimed.starts_with("[") {
            return Ok((stream.len() - trimed.len() + 1, ()))
        }
        get_error!(get_first_line(stream))        
    }

    fn expect_list_end(&mut self, stream: &str) -> Result<(usize, ()), Box<dyn Error>> {
        let trimed = stream.trim_start();
        if trimed.starts_with("]") {
            return Ok((stream.len() - trimed.len() + 1, ()))
        }
        get_error!(get_first_line(stream))        
    }



    fn parse_value(&mut self, stream: &str) -> Result<(usize, Value), Box<dyn Error>> {
        let re = Regex::new(r"^\s*(\S)").unwrap();
        let captures = re.captures(stream);
        let first_char = captures.map(|x| x[1].as_bytes().get(0).map(u8::to_owned)).flatten();
        // println!("first_char, {:?} ... {}? {}? {:?}", first_char, b'<', b'/', first_char.map(|x| x as char));
        match first_char {
            Some(b'<') => wrap(self.parse_dict(stream), Value::Dict),
            Some(b'/') => wrap(self.parse_key(stream), Value::Key),
            Some(b'[') => wrap(self.parse_list(stream), Value::List),
            Some(b'(') => wrap(self.parse_string(stream), Value::String),
            Some(c) if c >= b'0' && c <= b'9' => {
                let rf = self.parse_ref(stream);
                if rf.is_ok() { wrap(rf, Value::Ref) }
                else {
                    wrap(self.parse_number(stream), Value::Number)
                }
            }
            _ => {
                println!("parse_value failed {:?}", stream);
                get_error!(stream)
            }
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

fn get_heading(text: &str) -> &str {
    if let Some(index1) = text.find('\n') {
        if let Some(index2) = text[index1+1..].find('\n') {            
            if let Some(index3) = text[index1+1+index2+1..].find('\n') {
                &text[..index1+1+index2+1+index3]
            } else {
                &text[..index1+1+index2]
            }
        } else {
            &text[..index1]
        }
    } else {
        &text[..]
    }
}
