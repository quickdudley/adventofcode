use std::str::FromStr;
use std::num::ParseIntError;

struct Rule {
    outer: String,
    inner: Vec<(String,u8)>,
}

#[derive(Debug,Clone)]
enum RuleParseError {
    Numeric(ParseIntError),
    Other,
}

impl From<ParseIntError> for RuleParseError {
    fn from(src: ParseIntError) -> Self {
        Self::Numeric(src)
    }
}

impl FromStr for Rule {
    type Err = RuleParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut words = s.split_whitespace();
        let mut outer = Vec::new();
        for w in words.by_ref() {
            if w == "bags" {
                break;
            } else {
                outer.push(w);
            }
        }
        let outer = Intercalate::new(outer.iter(), &" ").map(std::ops::Deref::deref).collect::<String>();
        match words.next() {
            Some("contain") => Ok(()),
            _ => Err(RuleParseError::Other),
        }?;
        let mut inner = Vec::new();
        loop {
            let count = match words.next() {
                None => Err(RuleParseError::Other),
                Some(w) => if w == "no" {
                   match words.next() {
                       Some("other") => Ok(()),
                       _ => Err(RuleParseError::Other),
                   }?;
                   match words.next() {
                       Some("bags.") => Ok(()),
                       _ => Err(RuleParseError::Other),
                   }?;
                   return Ok(Rule {outer: outer, inner: inner});
                } else {
                    w.parse().or_else(|e| Err(RuleParseError::Numeric(e)))
                },
            }?;
            let mut color = Vec::new();
            for w in words.by_ref() {
                if w == "bag," || w == "bags," || w == "bag." || w == "bags." {
                    inner.push((Intercalate::new(color.iter(), &" ").map(std::ops::Deref::deref).collect::<String>(), count));
                    if w.chars().next_back() == Some('.') {
                        return Ok(Rule { outer: outer, inner: inner });
                    } else {
                        break;
                    }
                } else {
                    color.push(w);
                }
            }
        }
    }
}

struct Intercalate<I: Iterator> {
    sep: <I as Iterator>::Item,
    inner: std::iter::Peekable<I>,
    sep_phase: bool,
}

impl<I: Iterator> Intercalate<I> {
    fn new(inner: I, sep: <I as Iterator>::Item) -> Self {
        Self { sep: sep, inner: inner.peekable(), sep_phase: false }
    }
}

impl<I: Iterator> Iterator for Intercalate<I> where I::Item: Clone {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.sep_phase {
            match self.inner.peek() {
                Some(_) => {
                    self.sep_phase = false;
                    Some(self.sep.clone())
                },
                None => None
            }
        } else {
            self.sep_phase = true;
            self.inner.next()
        }
    }
}

fn count_colors_containing(rules: Vec<Rule>, innermost: Vec<String>) -> usize {
    use std::collections::HashMap;
    use std::collections::HashSet;
    let mut flowback = HashMap::new();
    for rule in rules.iter() {
        for (inner,_) in rule.inner.iter() {
            flowback.entry(inner).or_insert_with(Vec::new).push(rule.outer.clone());
        }
    }
    let mut hits = HashSet::new();
    let mut q: Vec<String> = innermost.clone();
    loop {
        match q.pop() {
            None => break,
            Some(color) => {
                if !hits.contains(&color) {
                    hits.insert(color.clone());
                    match flowback.get(&color) {
                        None => (),
                        Some(colors) => q.append(&mut colors.clone()),
                    }
                }
            },
        }
    };
    for color in innermost.iter() {
        hits.remove(color);
    }
    hits.len()
}

fn count_bags_in(rules: Vec<Rule>, outer: String) -> usize {
    use std::collections::HashMap;
    let mut counted: HashMap<String,usize> = HashMap::new();
    let tree: HashMap<_,_> = rules.iter().map(|rule| (rule.outer.clone(), rule.inner.clone())).collect();
    let mut q = vec![outer.clone()];
    loop {
        match q.pop() {
            None => break,
            Some(c) => match counted.get(&c) {
                Some(_) => (),
                None => if tree
                    .get(&c)
                    .iter()
                    .flat_map(|v| v.iter()).all(|(r,_)|
                        counted.contains_key(r)
                ) {
                    counted.insert(c.clone(), tree
                                   .get(&c)
                                   .iter()
                                   .flat_map(|v| v.iter())
                                   .map(|(r,n)|
                                        (1 + counted
                                         .get(r)
                                         .map(|x| *x)
                                         .unwrap_or(0)
                                         ) * usize::from(*n)
                                   ).sum()
                        );
                } else {
                    q.push(c.clone());
                    for (r,_) in tree.get(&c).iter().flat_map(|v| v.iter()) {
                        q.push(r.clone())
                    }
                },
            },
        }
    };
    match counted.get(&outer) {
        Some(r) => *r,
        None => 0,
    }
}

fn main() {
    use std::env;
    use std::io::BufRead;
    let args: Vec<_> = env::args().collect();
    let rules: Vec<Rule> = std::io::BufReader::new(std::fs::File::open(&args[1]).unwrap())
        .lines()
        .map(Result::unwrap)
        .map(|s| s.parse())
        .map(Result::unwrap)
        .collect();
    println!("{}", match args[2].as_ref() {
        "1" => count_colors_containing(rules, vec![String::from("shiny gold")]),
        "2" => count_bags_in(rules, String::from("shiny gold")),
        _ => panic!("No such problem"),
    });
}

