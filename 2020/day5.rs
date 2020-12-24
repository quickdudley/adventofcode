use std::convert::TryFrom;

fn find_seat<I: Iterator<Item=char>>(source: &mut I) -> (u8, u8) {
    let mut row = 0;
    let mut col = 0;
    for c in source {
        match c {
            'F' => { row *= 2; },
            'B' => { row = row * 2 + 1; },
            'L' => { col *= 2; },
            'R' => { col = col * 2 + 1; },
            _ => (),
        }
    }
    (row,col)
}

fn seat_id(row: u8, col: u8) -> u16 {
    u16::from(row) * 8 + u16::from(col)
}

fn max_seat<I: Iterator<Item=u16>>(ids: &mut I) -> u16 {
    ids.max().unwrap()
}

fn my_seat<I: Iterator<Item=u16>>(ids: &mut I) -> u16 {
    let mut filled: Vec<bool> = Vec::with_capacity(1024);
    filled.resize(1024, false);
    for seat in ids {
        filled[usize::from(seat)] = true;
    };
    for (i,f) in filled.iter().enumerate() {
        if !f && i < 1023 && i > 0 && filled[i + 1] && filled[i - 1] {
            return u16::try_from(i).unwrap();
        }
    }
    panic!("not found")
}

fn main() {
    use std::env;
    use std::io::BufRead;
    let args: Vec<_> = env::args().collect();
    let lines = std::io::BufReader::new(std::fs::File::open(&args[1]).unwrap()).lines();
    let mut seats = lines.map(|l| {
        let (row,col) = find_seat(&mut l.unwrap().chars());
        seat_id(row, col)
    });
    println!("{}", match args[2].as_ref() {
        "1" => max_seat(&mut seats),
        "2" => my_seat(&mut seats),
        _ => panic!("unknown problem"),
    });
}
