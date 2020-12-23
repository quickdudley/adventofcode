use std::collections::HashSet;

struct GroupedLines<I> {
    inner: I,
}

impl<I> GroupedLines<I> {
    fn new(inner: I) -> Self {
        GroupedLines { inner: inner }
    }
}

impl<I: Iterator<Item=S>, S: AsRef<str>> Iterator for GroupedLines<I> {
    type Item = Vec<String>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut r = Vec::new();
        loop {
            match self.inner.next() {
                Some(l) => {
                    if l.as_ref().len() == 0 {
                        break;
                    } else {
                        r.push(String::from(l.as_ref()));
                    }
                },
                None => break,
            }
        };
        if r.len() == 0 {
            None
        } else {
            Some(r)
        }
    }
}

fn any_checked(input: Vec<String>) -> HashSet<char> {
    input.iter().flat_map(|l| l.chars()).collect()
}

fn all_checked(input: Vec<String>) -> HashSet<char> {
    let mut i = input.iter();
    match i.next() {
        None => HashSet::new(),
        Some(l0) => {
            let mut r: HashSet<char> = l0.chars().collect();
            for l in i {
                r = r.intersection(&l.chars().collect()).map(Clone::clone).collect();
            }
            r
        },
    }
}

fn main() {
    use std::env;
    use std::io::BufRead;
    let args: Vec<_> = env::args().collect();
    let counts: usize = GroupedLines::new(
        std::io::BufReader::new(std::fs::File::open(&args[1]).unwrap()).lines()
        .map(Result::unwrap)
        ).map(match args[2].as_ref() {
            "any" => any_checked,
            "all" => all_checked,
            _ => panic!("No such aggregator"),
        })
        .map(|c| c.len())
        .sum();
    println!("{}", counts);
}

