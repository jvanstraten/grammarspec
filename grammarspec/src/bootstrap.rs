use crate::types;
use chumsky::{prelude::*, stream::Stream};

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

/// A grammar is a list of one or more rules.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtGrammar<L: types::Location> {
    Rule(Vec<types::Terminal<TokenType, L>>),
}

/// A rule can be either a grammar or a tokenizer rule. If a rule is defined
/// more than once, each definition will just add to the alternatives.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtRule<L: types::Location> {
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
        Option<types::Terminal<TokenType, L>>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
    ),

    /// A whitespace rule, of the form `_ :== pattern ;`. Works like a normal
    /// tokenizer rule, but the token is not emitted to the parser. Parsers that
    /// do not have a tokenizer (i.e. their tokens are simply the characters on
    /// the input) must implicitly match _* at the start of the grammar and after
    /// every symbol in grammar production rules (i.e. `::=` rules).
    WhitespaceRule(
        Option<types::Terminal<TokenType, L>>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
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
        Option<types::Terminal<TokenType, L>>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
        Vec<types::Terminal<TokenType, L>>,
        types::Terminal<TokenType, L>,
    ),
}

/// `a | b | ...` preferentially matches `a`, matches `b` if `a` does not
/// match, and so on.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtAlternation<L: types::Location> {
    Concatenation(
        types::Terminal<TokenType, L>,
        Vec<(types::Terminal<TokenType, L>, types::Terminal<TokenType, L>)>,
    ),
}

/// The alternatives at the root of a grammar production rule can be annotated
/// with a variant name (if not specified it is auto-generated when needed) at
/// the end and can be given a docstring before the ::= or | that starts the
/// variant. They are functionally indistinguishable from a normal, nested
/// alternation, though the parse tree may be generated in a different way.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtFirstAlter<L: types::Location> {
    Doc(
        Option<types::Terminal<TokenType, L>>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
        Option<types::Terminal<TokenType, L>>,
    ),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtSubseqAlter<L: types::Location> {
    Doc(
        Option<types::Terminal<TokenType, L>>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
        Option<types::Terminal<TokenType, L>>,
    ),
}

/// A toplevel alternative can be named by appending `-> name`. The names do not
/// have semantical meaning (they cannot be referred to within the grammar),
/// but are used to attach names to variants in the generated ASTs.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtAlterName<L: types::Location> {
    MinGt(types::Terminal<TokenType, L>, types::Terminal<TokenType, L>),
}

/// `a b ...` matches `a` followed by `b` and so on.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtConcatenation<L: types::Location> {
    Repetition(Vec<types::Terminal<TokenType, L>>),
}

/// The usual ?*+ characters can be used to specify repetition.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtRepetition<L: types::Location> {
    /// `a?` greedily matches `a` zero times or once.
    Maybe(types::Terminal<TokenType, L>, types::Terminal<TokenType, L>),

    /// `a*` greedily matches `a` zero or more times.
    Any(types::Terminal<TokenType, L>, types::Terminal<TokenType, L>),

    /// `a+` greedily matches `a` one or more times.
    Many(types::Terminal<TokenType, L>, types::Terminal<TokenType, L>),

    /// `a` matches `a` exactly once. Note that this one must come last in the
    /// list of alternatives, because otherwise the other alternatives would
    /// never match (because alternatives pick the *first* match, not the longest
    /// one).
    One(types::Terminal<TokenType, L>),
}

/// A single literal pattern or referenced rule to match, or a parenthesized
/// expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtSingular<L: types::Location> {
    Nested(
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
        types::Terminal<TokenType, L>,
    ),

    Symbol(types::Terminal<TokenType, L>),

    StringLiteral(types::Terminal<TokenType, L>),

    CharacterSet(types::Terminal<TokenType, L>),

    CharacterLiteral(types::Terminal<TokenType, L>),
}
