pub mod bootstrap;
pub mod types;

use chumsky::prelude::*;

fn main() {
    let mut tokenizer_errors = vec![];
    let tokens: Vec<_> = bootstrap::tokenize(include_str!("../../self-spec.ebnf")).filter_map(|x| {
        match x.to_grammar_input() {
            Ok(x) => Some(x),
            Err(x) => {
                if let Ok(error) = x.to_error() {
                    tokenizer_errors.push(error);
                }
                None
            }
        }
    }).collect();
    let (parse_tree, parser_errors) = bootstrap::PtGrammar::parser().parse_recovery(tokens);

    println!("{:#?}", parse_tree);

    if !tokenizer_errors.is_empty() || !parser_errors.is_empty() {
        println!("Errors:");
        for error in tokenizer_errors.iter() {
            println!(" - {error}");
        }
        for error in parser_errors.iter() {
            println!(" - {error}");
        }
    }
}
