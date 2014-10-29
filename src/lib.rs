#![feature(if_let, macro_rules)]

extern crate regex;

use std::fmt;
use std::str::MaybeOwned;
use regex::Regex;

#[deriving(PartialEq, Eq, Clone)]
pub struct Loc {
    line: int,
    col: int,
}

impl Loc {
    pub fn start() -> Loc {
        Loc{ line: 1, col: 0 }
    }

    pub fn next(&self, c: &char) -> Loc {
        match *c {
            '\n' => Loc{ line: self.line + 1, col: 0 },
            _ => Loc{ line: self.line, col: self.col + 1 },
        }
    }

    pub fn advance(&self, s: &str) -> Loc {
        let mut line = self.line;
        let mut col = self.col;

        for c in s.chars() {
            match c {
                '\n' => { line += 1; col = 0 },
                _ => { col += 1 }
            }
        }
        
        Loc { line: line, col: col }
    }
}

impl fmt::Show for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(l:{}, c:{})", self.line, self.col)
    }
}

#[deriving(Show, PartialEq, Eq, Clone)]
pub struct State<'a>(&'a str, Loc);

pub trait ParserAction<A> {
    fn run<'a>(&self, st: &State<'a>) -> Result<(A, State<'a>), String>;
}

pub type Parser<'a, A> = Box<ParserAction<A> + 'a>;

pub fn then<'a, A: 'a, B: 'a>(p1: Parser<'a, A>, p2: Parser<'a, B>) -> Parser<'a, B> {
    #[allow(dead_code)]
    struct Closure<'a, A: 'a, B: 'a>(Parser<'a, A>, Parser<'a, B>);
    
    impl <'a, A, B>ParserAction<B> for Closure<'a, A, B> {
        fn run<'a>(&self, st: &State<'a>) -> Result<(B, State<'a>), String> {
            let &Closure(ref a1, ref a2) = self;
            let (_, st) = try!(a1.run(st));
            a2.run(&st)
        }
    }

    box Closure(p1, p2)
}

macro_rules! parser {
    (
        [$($capt:ident : $cty:ty),+] ($st:ident) -> $ty:ty $body:expr
    ) => (
        {
            #[allow(dead_code)]
            struct Closure($($cty),*);
            impl ParserAction<$ty> for Closure {
                fn run<'a>(&self, $st: &State<'a>) -> Result<($ty, State<'a>), String> {
                    let &Closure($($capt),*) = self; // Expand lambda captures
                    $body
                }
            }
            box Closure($($capt),*)
        }
    );
    (
        [] ($st:ident) -> $ty:ty $body:expr
    ) => (
        {
            #[allow(dead_code)]
            struct Closure;
            impl ParserAction<$ty> for Closure {
                fn run<'a>(&self, $st: &State<'a>) -> Result<($ty, State<'a>), String> {
                    $body
                }
            }
            box Closure
        }
    )
}

pub fn white<'a>() -> Parser<'static, ()> {
    parser!([] (st) -> () {
        let &State(s, l) = st;
        let ns = s.trim_left_chars(|c: char| c.is_whitespace());
        Ok(((), State(ns, l.advance(s.slice_to(s.len() - ns.len())))))
    })
}

pub fn charp<'a>(pred: fn (&char) -> bool) -> Parser<'static, char> {
    parser!([pred: fn (&char) -> bool] (st) -> char {
        let &State(s, l) = st;
        if s.len() == 0 { return Err("Unexpected End of Input".to_string()) }

        let c = s.char_at(0);
        if pred(&c) {
            Ok((c, State(s.slice_from(1), l.next(&c))))
        } else {
            Err(format!("Unexpected {}", c))
        }
    })
}

pub fn some_char<'a>(patt: char) -> Parser<'static, char> {
    parser!([patt: char] (st) -> char {
        let &State(s, l) = st;
        if s.len() == 0 { return Err("Unexpected End of Input".to_string()) }

        let c = s.char_at(0);
        if c == patt {
            Ok((c, State(s.slice_from(1), l.next(&c))))
        } else {
            Err(format!("Unexpected {}", c))
        }
    })
}

pub fn string<'a, 'patt>(patt: MaybeOwned<'patt>) -> Parser<'patt, String> {
    struct Closure<'patt>(MaybeOwned<'patt>);
    impl <'patt>ParserAction<String> for Closure<'patt> {
        fn run<'a>(&self, st: &State<'a>) -> Result<(String, State<'a>), String> {
            let &Closure(ref patt,) = self;
            let &State(s, l) = st;
            let len = s.len();
            if len < patt.len() { return Err("Unexpected End of Input".to_string()) }
            
            let ss = s.slice_to(len);
            if ss == patt.as_slice() {
                Ok((ss.to_string(), State(s.slice_from(len), l.advance(ss))))
            } else {
                Err("Unexpected Text".to_string())
            }
        }
    }
    box Closure(patt)
}

pub fn restr<'a>(patt: Regex) -> Parser<'static, String> {
    #[allow(dead_code)]
    struct Closure(Regex);

    impl ParserAction<String> for Closure {
        fn run<'a>(&self, st: &State<'a>) -> Result<(String, State<'a>), String> {
            let &Closure(ref patt,) = self;
            let &State(s, l) = st;
            
            if let Some((0, len)) = patt.find(s) {
                let ns = s.slice_to(len);
                Ok((ns.to_string(), State(s.slice_from(len), l.advance(ns))))
            } else {
                Err("Unexpected".to_string())
            }
        }
    }

    box Closure(patt)
}

