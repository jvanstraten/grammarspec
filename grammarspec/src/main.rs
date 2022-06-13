pub mod bootstrap;
pub mod types;

use chumsky::prelude::*;

fn main() {
    let mut tokenizer_errors = vec![];
    let tokens: Vec<_> = bootstrap::tokenize(include_str!("../../simple.ebnf")).filter_map(|x| {
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
    let (mut parse_tree, parser_errors) = bootstrap::PtGrammar::parser().parse_recovery(&tokens[..]);
    if let Some(pt) = parse_tree.as_mut() {
        pt.visit_mut(&mut bootstrap::Annotator::new(&tokens[..])).unwrap();
    }

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
