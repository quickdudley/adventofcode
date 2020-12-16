use std::collections::HashMap;
use std::env;

fn extract_keys<S: AsRef<str>>(ln: S) -> HashMap<String,String> {
    let mut collected = HashMap::new();
    for field in ln.as_ref().split_whitespace() {
        let parts: Vec<_> = field.split(':').collect();
        collected.insert(String::from(parts[0]), String::from(parts[1]));
    };
    collected
}

const REQUIRED: &[(&str, fn(String) -> bool)] =
    &[
        ("byr", check_byr),
        ("iyr", check_iyr),
        ("eyr", check_eyr),
        ("hgt", check_hgt),
        ("hcl", check_hcl),
        ("ecl", check_ecl),
        ("pid", check_pid)
    ];

fn check_byr(ln: String) -> bool {
    ln.len() == 4 && match ln.parse::<usize>() {
        Ok(year) => year >= 1920 && year <= 2002,
        Err(_) => false,
    }
}

fn check_iyr(ln: String) -> bool {
    ln.len() == 4 && match ln.parse::<usize>() {
        Ok(year) => year >= 2010 && year <= 2020,
        Err(_) => false,
    }
}

fn check_eyr(ln: String) -> bool {
    ln.len() == 4 && match ln.parse::<usize>() {
        Ok(year) => year >= 2020 && year <= 2030,
        Err(_) => false,
    }
}

fn check_hgt(ln: String) -> bool {
    match ln.find(|c: char| !c.is_digit(10)) {
        Some(sep) => {
            let (num, unit) = ln.split_at(sep);
            match num.parse::<isize>() {
                Ok(num) => {
                    if unit == "in" {
                        num >= 59 && num <= 76
                    } else if unit == "cm" {
                        num >= 150 && num <= 193
                    } else {
                        false
                    }
                },
                Err(_) => false,
            }

        },
        None => false,
    }
}

fn check_hcl(ln: String) -> bool {
    if ln.len() != 7 {
        return false;
    }
    let mut ci = ln.chars();
    if ci.next() != Some('#') {
        return false;
    }
    for c in ci {
        if !c.is_digit(16) {
            return false;
        }
    }
    true
}

fn check_ecl(ln: String) -> bool {
    for ex in ["amb","blu","brn","gry","grn","hzl","oth"].iter() {
        if ln == *ex {
            return true;
        }
    }
    false
}

fn check_pid(ln: String) -> bool {
    ln.len() == 9 && ln.chars().all(|c| c.is_digit(10))
}

#[derive(Clone,Copy,Debug)]
enum Document {
  Passport,
  NorthPole,
  Invalid
}

struct DelimByEmpty<I> {
  inner: I,
}

impl<I> DelimByEmpty<I> {
    fn new(inner: I) -> Self {
        DelimByEmpty { inner: inner }
    }
}

impl<I: Iterator<Item=HashMap<Q,R>>, Q: Clone + std::cmp::Eq + std::hash::Hash, R: Clone> Iterator for DelimByEmpty<I> {
    type Item = HashMap<Q,R>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut result = HashMap::new();
        loop {
            match self.inner.next() {
                None => {
                    if result.is_empty() {
                        return None;
                    } else {
                        return Some(result);
                    }
                },
                Some(line) => {
                    if line.is_empty() {
                        return Some(result);
                    } else {
                        for (key,val) in line.iter() {
                            result.insert(key.clone(), val.clone());
                        }
                    }
                },
            }
        }
    }
}

fn validate_line(found: HashMap<String,String>, version: usize) -> Document {
    for (key,validator) in REQUIRED.iter() {
        match found.get(&String::from(*key)) {
            None => { return Document::Invalid; },
            Some(value) => {
                if version == 2 && !validator(value.clone()) {
                    return Document::Invalid;
                }
            },
        }
    };
    if found.contains_key(&String::from("cid")) {
        Document::Passport
    } else {
        Document::NorthPole
    }
}

fn main() {
    use std::io::BufRead;
    let args: Vec<_> = env::args().collect();
    let version: usize = args[2].parse().unwrap();
    let result: i32 = DelimByEmpty::new(
        std::io::BufReader::new(std::fs::File::open(&args[1]).unwrap())
            .lines()
            .map(Result::unwrap)
            .map(extract_keys)
        ).map(|d| validate_line(d, version))
        .map(|t| match t {
            Document::Invalid => 0,
            _ => 1,
        })
        .sum();
    println!("{:}",result);
}

