
extern crate nom;
use nom::combinator::map_res;

pub mod stringparser {

    #[derive(Debug)]
    enum Op {
        add,
    }

    use nom::{
        FindSubstring,
        combinator::map,
    };

    fn remove_whitespace(string: &str) -> String {
        string.split_whitespace().collect()
    }

    fn parse_char(string: &str) -> Option<usize> {
        if string.len() == 0 {
            None
        }else{
            string.find_substring("+")
        }
    }

    pub fn try_parse(string: &str) {
        match parse_char(string) {
            None => println!("Parsing failed, empty string! {}", string),
            Some(operator) => {
                println!("Parsing success! {}, + at [i] {}", string, operator)
            },
        }
    }
}