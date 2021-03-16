use std::str::FromStr;

#[derive(Clone,Copy,PartialEq,Eq)]
enum Cardinal {
    N,
    S,
    E,
    W
}

#[derive(Clone,Copy,PartialEq,Eq)]
enum Direction {
    C(Cardinal,u16),
    T(i8),
    F(u16),
}

#[derive(Debug)]
enum ParseDirectionError {
    I(std::num::ParseIntError),
    D(char),
    Z,
}

impl From<std::num::ParseIntError> for ParseDirectionError {
    fn from(src: std::num::ParseIntError) -> Self {
        Self::I(src)
    }
}

impl std::fmt::Display for ParseDirectionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseDirectionError::I(e) => e.fmt(f),
            ParseDirectionError::D(c) => write!(f, "Unrecognised direction code: {}", c),
            ParseDirectionError::Z => write!(f, "Empty string"),
        }
    }
}

impl FromStr for Direction {
    type Err = ParseDirectionError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 0 {
            Err(Self::Err::Z)?;
        };
        let (c,ns) = s.split_at(1);
        match c {
            "N" => {
                let d = ns.parse::<u16>()?;
                Ok(Direction::C(Cardinal::N,d))
            },
            "S" => {
                let d = ns.parse::<u16>()?;
                Ok(Direction::C(Cardinal::S,d))
            },
            "E" => {
                let d = ns.parse::<u16>()?;
                Ok(Direction::C(Cardinal::E,d))
            },
            "W" => {
                let d = ns.parse::<u16>()?;
                Ok(Direction::C(Cardinal::W,d))
            },
            "L" => {
                let d = ns.parse::<i16>()?;
                Ok(Direction::T(((4 - (d / 90)) % 4) as i8))
            },
            "R" => {
                let d = ns.parse::<i16>()?;
                Ok(Direction::T(((d / 90) % 4) as i8))
            },
            "F" => {
                let d = ns.parse::<u16>()?;
                Ok(Direction::F(d))
            },
            _ => Err(ParseDirectionError::D(c.chars().next().unwrap())),
        }
    }
}

#[derive(Clone,Copy,PartialEq,Eq)]
struct Ship {
    x: i32,
    y: i32,
    bearing: Cardinal,
    waypoint_x: i32,
    waypoint_y: i32,
}

impl Ship {
    fn new() -> Self {
        Ship {x: 0, y: 0, bearing: Cardinal::E, waypoint_x: 10, waypoint_y: -1 }
    }

    fn step(&mut self, dir: Direction, revised: bool) {
        if revised {
            match dir {
                Direction::C(c,d) => match c {
                    Cardinal::N => { self.waypoint_y -= d as i32; },
                    Cardinal::S => { self.waypoint_y += d as i32; },
                    Cardinal::E => { self.waypoint_x += d as i32; },
                    Cardinal::W => { self.waypoint_x -= d as i32; },
                },
                Direction::T(qt) => match qt % 4 {
                    0 => {},
                    1 => {
                        let (x,y) = (self.waypoint_x, self.waypoint_y);
                        self.waypoint_x = -y;
                        self.waypoint_y = x;
                    },
                    2 => {
                        let (x,y) = (self.waypoint_x, self.waypoint_y);
                        self.waypoint_x = -x;
                        self.waypoint_y = -y;
                    },
                    3 => {
                        let (x,y) = (self.waypoint_x, self.waypoint_y);
                        self.waypoint_x = y;
                        self.waypoint_y = -x;
                    },
                    _ => panic!("_ % 4 >= 4 !???"),
                },
                Direction::F(d) => {
                    self.x += self.waypoint_x * d as i32;
                    self.y += self.waypoint_y * d as i32;
                },
            }
        } else {
            match dir {
                Direction::C(c,d) => match c {
                    Cardinal::N => { self.y -= d as i32; },
                    Cardinal::S => { self.y += d as i32; },
                    Cardinal::E => { self.x += d as i32; },
                    Cardinal::W => { self.x -= d as i32; },
                },
                Direction::T(qt) => {
                    let sqt: i8 = match self.bearing {
                        Cardinal::N => 8,
                        Cardinal::S => 10,
                        Cardinal::E => 9,
                        Cardinal::W => 11,
                    };
                    self.bearing = match (sqt + qt) % 4 {
                        0 => Cardinal::N,
                        1 => Cardinal::E,
                        2 => Cardinal::S,
                        3 => Cardinal::W,
                        _ => panic!("_ % 4 >= 4 !???"),
                    };
                },
                Direction::F(d) => self.step(Direction::C(self.bearing, d), false),
            }
        }
    }
}

fn main() {
    use std::io::BufRead;
    let args: Vec<_> = std::env::args().collect();
    let mut ship = Ship::new();
    let file = std::io::BufReader::new(std::fs::File::open(<String as AsRef<std::path::Path>>::as_ref(&args[1])).unwrap());
    let revised = match args[2].as_ref() {
        "1" => false,
        "2" => true,
        _ => panic!("Version number (1 or 2) expected")
    };
    for line in file.lines() {
        ship.step(line.unwrap().parse().unwrap(), revised);
    }
    println!("{}", ship.x.abs() + ship.y.abs());
}
