use crate::types;
use chumsky::{prelude::*, stream::Stream};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {
    /// Implicit token.
    Pls,

    Val,
}

impl types::TokenType for TokenType {
    type All = [TokenType; 2];

    fn all() -> Self::All {
        [Self::Pls, Self::Val]
    }

    fn regex(&self) -> &'static str {
        match self {
            Self::Pls => "\\+",
            Self::Val => "(?:[0-9])+",
        }
    }

    fn whitespace() -> &'static str {
        "(?:[ \\\\t\\\\r\\\\n])+"
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Tokenizes a piece of text.
pub fn tokenize<'s>(text: &'s str) -> types::Tokenizer<'s, TokenType> {
    types::Tokenizer::new(text)
}

/// Tokenizes a piece of text starting from the given source location.
pub fn tokenize_from<'s, L: types::Location>(
    location: L,
    text: &'s str,
) -> types::Tokenizer<'s, TokenType, L> {
    types::Tokenizer::new_with_location(location, text)
}

/// Helper type for a Chumsky parser in a box.
pub type BoxedParser<T, L> = Box<
    dyn Parser<
        types::GrammarInput<TokenType, L>,
        T,
        Error = Simple<types::GrammarInput<TokenType, L>>,
    >,
>;

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtPls<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtPls<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Pls);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtVal<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtVal<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Val);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtGrammar<L: types::Location + 'static>(PtSum<L>);

impl<L: types::Location + 'static> PtGrammar<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser() -> BoxedParser<PtGrammar<L>, L> {
        make_parsers().0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtSum<L: types::Location + 'static>(PtVal<L>, PtPls, PtVal<L>);

impl<L: types::Location + 'static> PtSum<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser() -> BoxedParser<PtSum<L>, L> {
        make_parsers().1
    }
}

/// Constructs Chumsky parsers for all the grammar rules.
fn make_parsers<L: types::Location + 'static>(
) -> (BoxedParser<PtGrammar<L>, L>, BoxedParser<PtSum<L>, L>) {
    let mut parse_Grammar = Recursive::declare();
    let mut parse_Sum = Recursive::declare();

    parse_Grammar.define(parse_Sum.map(|x| PtGrammar(x)));

    parse_Sum.define(
        just(types::GrammarInput::new_pattern(TokenType::Val))
            .map(PtVal::from)
            .then(just(types::GrammarInput::new_pattern(TokenType::Pls)).map(PtPls::from))
            .then(just(types::GrammarInput::new_pattern(TokenType::Val)).map(PtVal::from))
            .map(|((x1, x2), x3)| (x1, x2, x3))
            .map(|(x1, x2, x3)| PtSum(x1, x2, x3)),
    );

    (Box::new(parse_Grammar), Box::new(parse_Sum))
}
