const INPUT: &'static [u8] = include_bytes!("../test.pdf");
fn main() {
    use std::mem::take;
    let mut line_count = 1;
    let mut line_offset = 0;
    let mut line_error = 0;
    let mut cache = String::new();
    let mut start_pos = (1, 0);
    for x in INPUT {
        if !x.is_ascii() {
            line_error += 1;
            if !cache.is_empty() && line_error <= 1 {
                print!("[{:?}-{:?}]", start_pos, (line_count, line_offset));
                handle_fragment(take(&mut cache));
            }
            start_pos = (line_count, line_offset);
            println!("[{},{}]{:0x}", line_count, line_offset, x);
        } else {
            if x == &7 {
                println!("ring bell line {}", line_count)
            }
            cache.push(*x as _)
        }
        if x == &0xa {
            line_count += 1;
            line_error = 0;
        }
        line_offset += 1;
    }
}

fn handle_fragment(input: String) {
    println!("{}", input)
}