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
    info: Info,
    wait_stream: bool,
}

struct Info {
    objects: Vec<Object>,
}

struct Object {
    id: (u8, u8),
    dict: HashMap<String, Value>,
    stream: Vec<u8>,
}

#[derive(Debug, Clone)]
enum Value {
    Key(String),
    Dict(HashMap<String, Value>),
    Ref((u8, u8)),
    String(String),
    Number(f64),
    List(Vec<Value>),
}

struct State {
    line_count: i32,
    line_offset: i32,
    line_error: i32,
    cache: String,
    start_pos: (i32, i32, String),
    mask: bool,
    start: usize,
    padding: usize,
    next_pos: usize,
    pads: Vec<String>,
    parser: Parser,
}

impl State {
    fn new() -> Self {
        Self {
            line_count : 1,
            line_offset : 0,
            line_error : 0,
            cache : String::new(),
            start_pos : (1, 0, String::new()),
            mask : false,
        
            start : 0,
            padding : 0,
            
            next_pos : 0,
            pads : vec![],

            parser: Parser::new(),
        }
    }

    fn parse(&mut self, offset: usize) {
        use std::mem::take;

        println!("[{:?}-{:?}]", self.start_pos, (self.line_count, self.line_offset, format!("{:0x}", offset)));
        let result = self.parser.handle_fragment(take(&mut self.cache));
        match result {
            Ok(ask_data) => {
                if let Some(len) = ask_data.stream_offset() {
                    let base = self.start + self.padding;
                    let stream_start = base + len;
                    println!("{stream_start:0x}(stream_start) = \
                        {base:0x}(base) + \
                        {len:0x}(object header, from {} to {})", INPUT[base] as char, INPUT[stream_start] as char);
                        print!("{:x}:", base);
                        for i in 0 .. 10 {
                            print!("{}", INPUT[base + i] as char);
                        }
                        println!();
                    let (len, read) = ask_data.read(&INPUT[stream_start..]).unwrap();
                    self.next_pos = stream_start+len;
                    self.parser.info.objects.last_mut().unwrap().stream = read;
                    println!("{stream_start:0x} to {:0x} stream fetched, data length {len}", stream_start + len);
                    println!("{stream_start} + {len} = {}", stream_start + len);
                    println!("next: {:x}", INPUT[stream_start + len]);
                    if stream_start + len == 0x168c {
                        println!("hello");
                    }
                } else {
                    println!("no stream here {:0x}", self.start + self.padding)
                }
            }
            Err(err) => panic!("{}", err),
        }
        self.start = offset;
        self.padding = 0;
    }
}

fn main() {

    let mut parser = Parser::new();
    println!("{:?}", parser.handle_fragment(r"
    5 0 obj
    <<
      /Type /FontDescriptor
      /FontName /ABCDEF#2BKaiTi
      /Flags 131076
      /FontBBox [-46.875 -183.59375 1031.25 859.375]
      /ItalicAngle 0
      /Ascent 859.375
      /Descent -140.625
      /CapHeight 687.5
      /StemV 95.4
      /FontFile2 7 0 R
    >>
endobj
    ".into()).is_ok());

    let mut state = State::new();

    for offset in 0 .. INPUT.len() {
        let x = INPUT[offset];
        if offset < state.next_pos {
            state.padding += 1;
            state.pads.push(format!("offset {:?} {:?}", offset, x));
            continue;
        }
        // println!("offset {offset:x}");

        if x == 0xa {
            state.line_count += 1;
            state.line_error = 0;
            state.line_offset = 0;
            state.mask = false;
        }
        state.line_offset += 1;
        if x == b'%' {
            state.mask = true;
        }
        if state.mask {
            state.padding += 1;
            state.pads.push(format!("offset {:?} {:?}", offset, x));
            continue
        }
        if !x.is_ascii() {
            if !state.cache.is_empty() && state.line_error == 0 {
                state.parse(offset);
            } else {
                println!("[{},{},{:0x}]{:0x}", state.line_count, state.line_offset, offset, x);
            }
            state.padding += 1;
            state.line_error += 1;
            state.start_pos = (state.line_count, state.line_offset, format!("{:0x}", offset));
        } else {
            if x == 7 {
                println!("ring bell line {}", state.line_count)
            }
            state.cache.push(x as _)
        }
    }
    state.parse(INPUT.len())
}

impl Parser {
    fn new() -> Self {
        Self { wait_stream: false, info: Info { objects: vec![] }}
    }

    fn handle_fragment(&mut self, input: String) -> Result<AskData, Box<dyn Error>> {
        // println!(":{}", input);
        let mut offset = 0;
        if self.wait_stream {
            self.wait_stream = false;
            let (len, ()) = self.expect_stream_end(&input)?;
            offset += len;
        }
        while offset < input.len() {
            let try_object_start = self.expect_obj_start(&input[..][offset..]);
            let (len, id) = if let Ok(x) = try_object_start {
                x
            } else {
                println!("try_object_start {try_object_start:?}");
                return Ok(AskData::Nope(offset))
            };
            offset += len;
            let (len, dict) = self.parse_dict(&input[..][offset..]).unwrap();
            offset += len;
            println!("{:?}", dict);
            let length = dict.get("Length").map(Value::clone);
            let filter = dict.get("Filter").map(Value::clone);
            let object = Object {
                id,
                dict,
                stream: vec![],
            };
            self.info.objects.push(object);
            let (len, want_stream) = self.expect_obj_end(&input[..][offset..])?;
            offset += len;
            // println!("remaining <<<<<<\n{}\n>>>>>>>>", &input[..][offset..]);
            if want_stream {
                let length = if let Some(Value::Number(n)) = length {
                    n as _
                } else {
                    return get_error!("Length is strange now")
                };
                if filter.is_none() {
                    return Ok(AskData::Plain(offset, length))
                }
                else if let Some(Value::Key(filter)) = filter {
                    if filter != "FlateDecode" {
                        panic!("Unknown filter")
                    }
                    fn f(x: &[u8]) -> usize {
                        let sub_bytes : &'static [u8] = b"\nendstream\n";
                        x.windows(sub_bytes.len()).position(|window| window == sub_bytes).unwrap() + 1
                    }
                    return Ok(AskData::Flate(offset, length, f))
                }
                
                return get_error!("Length missing")
            }
        }
        Ok(AskData::Nope(offset))
    }

    fn expect_obj_start(&mut self, starter: &str) -> Result<(usize, (u8, u8)), Box<dyn Error>> {
        let re = Regex::new("^\\s*(\\d+) (\\d+) obj\n").unwrap();
        if let Some(captures) = re.captures(starter) {
            let id: (u8, u8) = (captures[1].parse()?, captures[2].parse()?);
            return Ok((captures[0].len(), id));
        }
        get_error!(starter)
    }

    fn expect_obj_end(&mut self, tail: &str) -> Result<(usize, bool), Box<dyn Error>> {
        if tail.starts_with("\nendobj\n") {
            Ok(("\nendobj\n".len(), false))
        } else if tail.starts_with("\nstream\n") {
            self.wait_stream = true;
            Ok(("\nstream\n".len(), true))
        } else {
            get_error!(get_heading(tail))
        }
    }

    fn expect_stream_end(&mut self, tail: &str) -> Result<(usize, ()), Box<dyn Error>> {
        if tail.starts_with("endstream\nendobj\n") {
            Ok(("endstream\nendobj\n".len(), ()))
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
            // println!("{:?} {:?}", k, v);
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

    fn parse_number(&mut self, stream: &str) -> Result<(usize, f64), Box<dyn Error>> {
        let re = Regex::new(r"^\s*(-?\d*\.?\d*)").unwrap();
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
            Some(b'-') | Some(b'.') => wrap(self.parse_number(stream), Value::Number),
            Some(c) if c >= b'0' && c <= b'9' => {
                let rf = self.parse_ref(stream);
                if rf.is_ok() { wrap(rf, Value::Ref) }
                else {
                    wrap(self.parse_number(stream), Value::Number)
                }
            }
            _ => {
                if !stream.trim_start().starts_with("]") { println!("parse_value failed {:?}", stream) };
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

fn decode(data: &[u8]) -> (usize, Vec<u8>) {
    use std::io::Read;
    use flate2::bufread::ZlibDecoder;

    let mut decoder = ZlibDecoder::new(data);
    let mut decompressed_data = Vec::new();
    
    let len = decoder.read_to_end(&mut decompressed_data).unwrap();
    (len, decompressed_data)
}

#[derive(Debug)]
enum AskData {
    Nope(usize),
    Flate(usize, usize, fn(&[u8])->usize),
    Plain(usize, usize),
}

impl AskData {
    fn stream_offset(&self) -> Option<usize> {
        match self {
            AskData::Plain(r, ..) | AskData::Flate(r, ..) => Some(*r),
            AskData::Nope(_, ..) => None,
        }
    }
    fn read(self, data: &[u8]) -> Option<(usize, Vec<u8>)> {
        match self {
            AskData::Nope(_) => None,
            AskData::Plain(_, n) => Some((n, data[..n].into())),
            AskData::Flate(_, expected_size, f) => {
                let offset = f(data);
                println!("stream:: length: {offset:0x} start: {:0x} end: {:0x}({})", data[0], data[offset], data[offset] as char);
                let (actual_size, data) = decode(&data[..offset]);
                if expected_size != actual_size {
                    println!("decompressed data length is strange, expected {expected_size} got {actual_size}")
                } else {
                    println!("!!great, decompressed data length is fine")
                }
                Some((offset, data))
            },
        }
    }
}