use crate::types;
use chumsky::prelude::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {
    /// Implicit token.
    ColEqEq,

    /// Implicit token.
    Sem,

    /// Implicit token.
    Usc,

    /// All symbol names are written in kebab-case. GrammarSpec will convert the
    /// names to the appropriate case conventions for the target language.
    Symbol,

    /// Implicit token.
    Bar,

    /// Implicit token.
    ColColEq,

    /// Implicit token.
    MinGt,

    /// Implicit token.
    Qst,

    /// Implicit token.
    Ast,

    /// Implicit token.
    Pls,

    /// Implicit token.
    Lp,

    /// Implicit token.
    Rp,

    /// `'...'` and `"..."` match `...` literally. Empty string literals are not
    /// supported; instead use `?`.
    StringLiteral,

    /// `[...]` matches a single character within the specified set. `a-z` may be
    /// used to select a whole range of code points, and a `^` at the front of the
    /// `...` inverts the set.
    CharacterSet,

    /// Hash escape sequences for characters can also be matched outside the context
    /// of a string or character range.
    CharacterLiteral,

    /// /** comments are treated as docstrings. They may only appear immediately in
    /// front of a rule or toplevel alternative in a grammar production rule.
    Doc,
}

impl types::TokenType for TokenType {
    type All = [TokenType; 16];

    fn all() -> Self::All {
        [
            Self::ColEqEq,
            Self::Sem,
            Self::Usc,
            Self::Symbol,
            Self::Bar,
            Self::ColColEq,
            Self::MinGt,
            Self::Qst,
            Self::Ast,
            Self::Pls,
            Self::Lp,
            Self::Rp,
            Self::StringLiteral,
            Self::CharacterSet,
            Self::CharacterLiteral,
            Self::Doc,
        ]
    }

    fn regex(&self) -> &'static str {
        match self {
            Self::ColEqEq => ":==",
            Self::Sem => ";",
            Self::Usc => "_",
            Self::Symbol => "(?:[a-z])(?:(?:[a-z0-9\\-])*)",
            Self::Bar => "\\|",
            Self::ColColEq => "::=",
            Self::MinGt => "\\->",
            Self::Qst => "\\?",
            Self::Ast => "\\*",
            Self::Pls => "\\+",
            Self::Lp => "\\(",
            Self::Rp => "\\)",
            Self::StringLiteral => "(?:(?:')(?:(?:(?:[^'#])|(?:(?:#)(?:.)))+)(?:'))|(?:(?:\")(?:(?:(?:[^\"#])|(?:(?:#)(?:.)))+)(?:\"))",
            Self::CharacterSet => "(?:\\[)(?:(?:\\^)?)(?:(?:(?:(?:(?:[^\\^\\-\\]#])|(?:(?:(?:#)(?:(?:[^xuU])|(?:(?:x)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F])))|(?:(?:u)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F])))|(?:(?:U)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))))))))(?:(?:(?:\\-)(?:(?:(?:[^\\^\\-\\]#])|(?:(?:(?:#)(?:(?:[^xuU])|(?:(?:x)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F])))|(?:(?:u)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F])))|(?:(?:U)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F])))))))))?))*)(?:\\])",
            Self::CharacterLiteral => "(?:(?:#)(?:(?:[^xuU])|(?:(?:x)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F])))|(?:(?:u)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F])))|(?:(?:U)(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F]))(?:(?:[0-9a-fA-F])))))",
            Self::Doc => "(?:/\\*\\*)(?:(?:(?:(?:(?:[^\\*])|(?:(?:(?:\\*)+)(?:[^\\*/])))*)(?:(?:\\*)*)))(?:\\*/)",
        }
    }

    fn whitespace() -> &'static str {
        "(?:(?:/\\*)(?:(?:(?:[^\\*])(?:(?:(?:(?:(?:[^\\*])|(?:(?:(?:\\*)+)(?:[^\\*/])))*)(?:(?:\\*)*))))?)(?:\\*/))|(?:(?:[ \t\n\r])+)"
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
pub type BoxedParser<'a, T, L> = chumsky::BoxedParser<
    'a,
    types::GrammarInput<TokenType, L>,
    T,
    Simple<types::GrammarInput<TokenType, L>>,
>;

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtColEqEq<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtColEqEq<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::ColEqEq);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtSem<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtSem<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Sem);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtUsc<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtUsc<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Usc);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// All symbol names are written in kebab-case. GrammarSpec will convert the
/// names to the appropriate case conventions for the target language.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtSymbol<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtSymbol<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Symbol);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtBar<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtBar<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Bar);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtColColEq<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtColColEq<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::ColColEq);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtMinGt<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtMinGt<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::MinGt);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtQst<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtQst<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Qst);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtAst<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtAst<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Ast);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

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

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtLp<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtLp<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Lp);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Implicit token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtRp<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtRp<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Rp);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// `'...'` and `"..."` match `...` literally. Empty string literals are not
/// supported; instead use `?`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtStringLiteral<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtStringLiteral<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::StringLiteral);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// `[...]` matches a single character within the specified set. `a-z` may be
/// used to select a whole range of code points, and a `^` at the front of the
/// `...` inverts the set.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtCharacterSet<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtCharacterSet<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::CharacterSet);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// Hash escape sequences for characters can also be matched outside the context
/// of a string or character range.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtCharacterLiteral<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtCharacterLiteral<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::CharacterLiteral);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// /** comments are treated as docstrings. They may only appear immediately in
/// front of a rule or toplevel alternative in a grammar production rule.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtDoc<L: types::Location = types::SingleFileLocation> {
    pub text: String,
    pub span: types::Span<L>,
}

impl<L: types::Location> From<types::GrammarInput<TokenType, L>> for PtDoc<L> {
    fn from(input: types::GrammarInput<TokenType, L>) -> Self {
        let (token_type, text, span) = input.unwrap();
        assert!(token_type == TokenType::Doc);
        Self {
            text: text.unwrap_or_default(),
            span,
        }
    }
}

/// A grammar is a list of one or more rules.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtGrammar<L: types::Location + 'static>(Vec<Box<PtRule<L>>>);

impl<L: types::Location + 'static> PtGrammar<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtGrammar<L>, L> {
        make_parsers().0
    }
}

/// A rule can be either a grammar or a tokenizer rule. If a rule is defined
/// more than once, each definition will just add to the alternatives.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtRule<L: types::Location + 'static> {
    /// A tokenizer rule, of the form `symbol :== pattern ;`. The tokenizer (if
    /// any) is expected to tokenize the input as follows:
    ///
    ///  - try matching the _ token rule and all named token rules that are
    ///    *directly* referred to from grammar production rules on the remainder
    ///    of the input;
    ///  - emit the token corresponding to the longest match to the parser, or
    ///    drop it if said longest match is the _ token;
    ///     - if there is a tie in match lengths, emit the token that was defined
    ///       earlier in the grammar (the first definition is what counts, if
    ///       there are multiple);
    ///  - if no token matched, emit an unrecognized character error and (maybe)
    ///    try to recover.
    ///
    /// Parsers that do not have a tokenizer (i.e. their tokens are simply the
    /// characters on the input) can treat these as normal rules.
    ///
    /// Tokenizer rules may not directly or indirectly refer to themselves, i.e.
    /// they may not be recursive. This allows them to be mapped onto regular
    /// expressions.
    ///
    /// Tokenizer rules that are not directly used within grammar rules are not
    /// treated as actual tokens; rather, they are treated as reusable bits of a
    /// regular expression. Borrowing ANTLR terminology, they are called
    /// fragments (but unlike in ANTLR, they are detected automatically). This is
    /// just a readability thing; it makes no difference for the matched language.
    TokenRule(
        Option<PtDoc<L>>,
        PtSymbol<L>,
        PtColEqEq<L>,
        Box<PtAlternation<L>>,
        PtSem<L>,
    ),

    /// A whitespace rule, of the form `_ :== pattern ;`. Works like a normal
    /// tokenizer rule, but the token is not emitted to the parser. Parsers that
    /// do not have a tokenizer (i.e. their tokens are simply the characters on
    /// the input) must implicitly match _* at the start of the grammar and after
    /// every symbol in grammar production rules (i.e. `::=` rules).
    WhitespaceRule(
        Option<PtDoc<L>>,
        PtUsc<L>,
        PtColEqEq<L>,
        Box<PtAlternation<L>>,
        PtSem<L>,
    ),

    /// A grammar production rule, for the form `symbol ::= pattern ;`. The syntax
    /// here is a bit convoluted because the toplevel alternations can be given
    /// docstrings and can be named, but the syntax is otherwise the same as for
    /// token rules.
    ///
    /// Whenever literal patterns are used in a grammar rule, they are implicitly
    /// converted to a token rule. Be mindful of this when writing the grammar:
    /// they can cause tokenizer conflicts just like any other token!
    GrammarRule(
        Option<PtDoc<L>>,
        PtSymbol<L>,
        Box<PtFirstAlter<L>>,
        Vec<Box<PtSubseqAlter<L>>>,
        PtSem<L>,
    ),
}

impl<L: types::Location + 'static> PtRule<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtRule<L>, L> {
        make_parsers().1
    }
}

/// `a | b | ...` preferentially matches `a`, matches `b` if `a` does not
/// match, and so on.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtAlternation<L: types::Location + 'static>(
    Box<PtConcatenation<L>>,
    Vec<(PtBar<L>, Box<PtConcatenation<L>>)>,
);

impl<L: types::Location + 'static> PtAlternation<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtAlternation<L>, L> {
        make_parsers().2
    }
}

/// The alternatives at the root of a grammar production rule can be annotated
/// with a variant name (if not specified it is auto-generated when needed) at
/// the end and can be given a docstring before the ::= or | that starts the
/// variant. They are functionally indistinguishable from a normal, nested
/// alternation, though the parse tree may be generated in a different way.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtFirstAlter<L: types::Location + 'static>(
    Option<PtDoc<L>>,
    PtColColEq<L>,
    Box<PtConcatenation<L>>,
    Option<Box<PtAlterName<L>>>,
);

impl<L: types::Location + 'static> PtFirstAlter<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtFirstAlter<L>, L> {
        make_parsers().3
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtSubseqAlter<L: types::Location + 'static>(
    Option<PtDoc<L>>,
    PtBar<L>,
    Box<PtConcatenation<L>>,
    Option<Box<PtAlterName<L>>>,
);

impl<L: types::Location + 'static> PtSubseqAlter<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtSubseqAlter<L>, L> {
        make_parsers().4
    }
}

/// A toplevel alternative can be named by appending `-> name`. The names do not
/// have semantical meaning (they cannot be referred to within the grammar),
/// but are used to attach names to variants in the generated ASTs.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtAlterName<L: types::Location + 'static>(PtMinGt<L>, PtSymbol<L>);

impl<L: types::Location + 'static> PtAlterName<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtAlterName<L>, L> {
        make_parsers().5
    }
}

/// `a b ...` matches `a` followed by `b` and so on.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PtConcatenation<L: types::Location + 'static>(Vec<Box<PtRepetition<L>>>);

impl<L: types::Location + 'static> PtConcatenation<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtConcatenation<L>, L> {
        make_parsers().6
    }
}

/// The usual ?*+ characters can be used to specify repetition.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtRepetition<L: types::Location + 'static> {
    /// `a?` greedily matches `a` zero times or once.
    Maybe(Box<PtSingular<L>>, PtQst<L>),

    /// `a*` greedily matches `a` zero or more times.
    Any(Box<PtSingular<L>>, PtAst<L>),

    /// `a+` greedily matches `a` one or more times.
    Many(Box<PtSingular<L>>, PtPls<L>),

    /// `a` matches `a` exactly once. Note that this one must come last in the
    /// list of alternatives, because otherwise the other alternatives would
    /// never match (because alternatives pick the *first* match, not the longest
    /// one).
    One(Box<PtSingular<L>>),
}

impl<L: types::Location + 'static> PtRepetition<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtRepetition<L>, L> {
        make_parsers().7
    }
}

/// A single literal pattern or referenced rule to match, or a parenthesized
/// expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtSingular<L: types::Location + 'static> {
    Nested(PtLp<L>, Box<PtAlternation<L>>, PtRp<L>),

    Symbol(PtSymbol<L>),

    StringLiteral(PtStringLiteral<L>),

    CharacterSet(PtCharacterSet<L>),

    CharacterLiteral(PtCharacterLiteral<L>),
}

impl<L: types::Location + 'static> PtSingular<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtSingular<L>, L> {
        make_parsers().8
    }
}

/// Constructs Chumsky parsers for all the grammar rules.
fn make_parsers<'a, L: types::Location + 'static>() -> (
    BoxedParser<'a, PtGrammar<L>, L>,
    BoxedParser<'a, PtRule<L>, L>,
    BoxedParser<'a, PtAlternation<L>, L>,
    BoxedParser<'a, PtFirstAlter<L>, L>,
    BoxedParser<'a, PtSubseqAlter<L>, L>,
    BoxedParser<'a, PtAlterName<L>, L>,
    BoxedParser<'a, PtConcatenation<L>, L>,
    BoxedParser<'a, PtRepetition<L>, L>,
    BoxedParser<'a, PtSingular<L>, L>,
) {
    let mut parse_Grammar = Recursive::declare();
    let mut parse_Rule = Recursive::declare();
    let mut parse_Alternation = Recursive::declare();
    let mut parse_FirstAlter = Recursive::declare();
    let mut parse_SubseqAlter = Recursive::declare();
    let mut parse_AlterName = Recursive::declare();
    let mut parse_Concatenation = Recursive::declare();
    let mut parse_Repetition = Recursive::declare();
    let mut parse_Singular = Recursive::declare();

    parse_Grammar.define(
        parse_Rule
            .clone()
            .map(Box::new)
            .repeated()
            .at_least(1)
            .map(|x| PtGrammar(x)),
    );

    parse_Rule.define(
        just(types::GrammarInput::new_pattern(TokenType::Doc))
            .map(PtDoc::from)
            .or_not()
            .then(just(types::GrammarInput::new_pattern(TokenType::Symbol)).map(PtSymbol::from))
            .then(just(types::GrammarInput::new_pattern(TokenType::ColEqEq)).map(PtColEqEq::from))
            .then(parse_Alternation.clone().map(Box::new))
            .then(just(types::GrammarInput::new_pattern(TokenType::Sem)).map(PtSem::from))
            .map(|((((x1, x2), x3), x4), x5)| (x1, x2, x3, x4, x5))
            .map(|(x1, x2, x3, x4, x5)| PtRule::TokenRule(x1, x2, x3, x4, x5))
            .or(just(types::GrammarInput::new_pattern(TokenType::Doc))
                .map(PtDoc::from)
                .or_not()
                .then(just(types::GrammarInput::new_pattern(TokenType::Usc)).map(PtUsc::from))
                .then(
                    just(types::GrammarInput::new_pattern(TokenType::ColEqEq)).map(PtColEqEq::from),
                )
                .then(parse_Alternation.clone().map(Box::new))
                .then(just(types::GrammarInput::new_pattern(TokenType::Sem)).map(PtSem::from))
                .map(|((((x1, x2), x3), x4), x5)| (x1, x2, x3, x4, x5))
                .map(|(x1, x2, x3, x4, x5)| PtRule::WhitespaceRule(x1, x2, x3, x4, x5)))
            .or(just(types::GrammarInput::new_pattern(TokenType::Doc))
                .map(PtDoc::from)
                .or_not()
                .then(just(types::GrammarInput::new_pattern(TokenType::Symbol)).map(PtSymbol::from))
                .then(parse_FirstAlter.clone().map(Box::new))
                .then(parse_SubseqAlter.clone().map(Box::new).repeated())
                .then(just(types::GrammarInput::new_pattern(TokenType::Sem)).map(PtSem::from))
                .map(|((((x1, x2), x3), x4), x5)| (x1, x2, x3, x4, x5))
                .map(|(x1, x2, x3, x4, x5)| PtRule::GrammarRule(x1, x2, x3, x4, x5))),
    );

    parse_Alternation.define(
        parse_Concatenation
            .clone()
            .map(Box::new)
            .then(
                just(types::GrammarInput::new_pattern(TokenType::Bar))
                    .map(PtBar::from)
                    .then(parse_Concatenation.clone().map(Box::new))
                    .map(|(x1, x2)| (x1, x2))
                    .repeated(),
            )
            .map(|(x1, x2)| (x1, x2))
            .map(|(x1, x2)| PtAlternation(x1, x2)),
    );

    parse_FirstAlter.define(
        just(types::GrammarInput::new_pattern(TokenType::Doc))
            .map(PtDoc::from)
            .or_not()
            .then(just(types::GrammarInput::new_pattern(TokenType::ColColEq)).map(PtColColEq::from))
            .then(parse_Concatenation.clone().map(Box::new))
            .then(parse_AlterName.clone().map(Box::new).or_not())
            .map(|(((x1, x2), x3), x4)| (x1, x2, x3, x4))
            .map(|(x1, x2, x3, x4)| PtFirstAlter(x1, x2, x3, x4)),
    );

    parse_SubseqAlter.define(
        just(types::GrammarInput::new_pattern(TokenType::Doc))
            .map(PtDoc::from)
            .or_not()
            .then(just(types::GrammarInput::new_pattern(TokenType::Bar)).map(PtBar::from))
            .then(parse_Concatenation.clone().map(Box::new))
            .then(parse_AlterName.clone().map(Box::new).or_not())
            .map(|(((x1, x2), x3), x4)| (x1, x2, x3, x4))
            .map(|(x1, x2, x3, x4)| PtSubseqAlter(x1, x2, x3, x4)),
    );

    parse_AlterName.define(
        just(types::GrammarInput::new_pattern(TokenType::MinGt))
            .map(PtMinGt::from)
            .then(just(types::GrammarInput::new_pattern(TokenType::Symbol)).map(PtSymbol::from))
            .map(|(x1, x2)| (x1, x2))
            .map(|(x1, x2)| PtAlterName(x1, x2)),
    );

    parse_Concatenation.define(
        parse_Repetition
            .clone()
            .map(Box::new)
            .repeated()
            .at_least(1)
            .map(|x| PtConcatenation(x)),
    );

    parse_Repetition.define(
        parse_Singular
            .clone()
            .map(Box::new)
            .then(just(types::GrammarInput::new_pattern(TokenType::Qst)).map(PtQst::from))
            .map(|(x1, x2)| (x1, x2))
            .map(|(x1, x2)| PtRepetition::Maybe(x1, x2))
            .or(parse_Singular
                .clone()
                .map(Box::new)
                .then(just(types::GrammarInput::new_pattern(TokenType::Ast)).map(PtAst::from))
                .map(|(x1, x2)| (x1, x2))
                .map(|(x1, x2)| PtRepetition::Any(x1, x2)))
            .or(parse_Singular
                .clone()
                .map(Box::new)
                .then(just(types::GrammarInput::new_pattern(TokenType::Pls)).map(PtPls::from))
                .map(|(x1, x2)| (x1, x2))
                .map(|(x1, x2)| PtRepetition::Many(x1, x2)))
            .or(parse_Singular
                .clone()
                .map(Box::new)
                .map(|x| PtRepetition::One(x))),
    );

    parse_Singular.define(
        just(types::GrammarInput::new_pattern(TokenType::Lp))
            .map(PtLp::from)
            .then(parse_Alternation.clone().map(Box::new))
            .then(just(types::GrammarInput::new_pattern(TokenType::Rp)).map(PtRp::from))
            .map(|((x1, x2), x3)| (x1, x2, x3))
            .map(|(x1, x2, x3)| PtSingular::Nested(x1, x2, x3))
            .or(just(types::GrammarInput::new_pattern(TokenType::Symbol))
                .map(PtSymbol::from)
                .map(|x| PtSingular::Symbol(x)))
            .or(
                just(types::GrammarInput::new_pattern(TokenType::StringLiteral))
                    .map(PtStringLiteral::from)
                    .map(|x| PtSingular::StringLiteral(x)),
            )
            .or(
                just(types::GrammarInput::new_pattern(TokenType::CharacterSet))
                    .map(PtCharacterSet::from)
                    .map(|x| PtSingular::CharacterSet(x)),
            )
            .or(just(types::GrammarInput::new_pattern(
                TokenType::CharacterLiteral,
            ))
            .map(PtCharacterLiteral::from)
            .map(|x| PtSingular::CharacterLiteral(x))),
    );

    (
        parse_Grammar.boxed(),
        parse_Rule.boxed(),
        parse_Alternation.boxed(),
        parse_FirstAlter.boxed(),
        parse_SubseqAlter.boxed(),
        parse_AlterName.boxed(),
        parse_Concatenation.boxed(),
        parse_Repetition.boxed(),
        parse_Singular.boxed(),
    )
}
