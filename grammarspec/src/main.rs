pub mod bootstrap;
pub mod types;

use chumsky::{prelude::*, stream::Stream};

fn main() {
    let mut error_tokens = vec![];
    for token in bootstrap::tokenize(include_str!("../../self-spec.ebnf")).filter_map(|x| {
        match x.to_grammar_input() {
            Ok(x) => Some(x),
            Err(x) => {
                if let Ok(error) = x.to_error() {
                    error_tokens.push(error);
                }
                None
            }
        }
    }) {
        println!("{token}");
    }
    println!();
    println!("Errors:");
    for error_token in error_tokens.iter() {
        println!(" - {error_token}");
    }


}
