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
    types::TerminalToken<TokenType, L>,
    T,
    Simple<types::TerminalToken<TokenType, L>>,
>;

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtColEqEq<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtColEqEq<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::ColEqEq);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_ColEqEq(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_ColEqEq(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtSem<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtSem<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Sem);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Sem(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Sem(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtUsc<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtUsc<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Usc);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Usc(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Usc(self)
    }
}

/// All symbol names are written in kebab-case. GrammarSpec will convert the
/// names to the appropriate case conventions for the target language.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtSymbol<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtSymbol<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Symbol);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Symbol(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Symbol(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtBar<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtBar<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Bar);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Bar(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Bar(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtColColEq<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtColColEq<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::ColColEq);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_ColColEq(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_ColColEq(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtMinGt<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtMinGt<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::MinGt);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_MinGt(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_MinGt(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtQst<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtQst<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Qst);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Qst(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Qst(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtAst<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtAst<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Ast);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Ast(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Ast(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtPls<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtPls<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Pls);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Pls(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Pls(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtLp<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtLp<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Lp);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Lp(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Lp(self)
    }
}

/// Implicit token.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtRp<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtRp<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Rp);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Rp(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Rp(self)
    }
}

/// `'...'` and `"..."` match `...` literally. Empty string literals are not
/// supported; instead use `?`.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtStringLiteral<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtStringLiteral<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::StringLiteral);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_StringLiteral(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_StringLiteral(self)
    }
}

/// `[...]` matches a single character within the specified set. `a-z` may be
/// used to select a whole range of code points, and a `^` at the front of the
/// `...` inverts the set.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtCharacterSet<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtCharacterSet<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::CharacterSet);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_CharacterSet(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_CharacterSet(self)
    }
}

/// Hash escape sequences for characters can also be matched outside the context
/// of a string or character range.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtCharacterLiteral<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtCharacterLiteral<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::CharacterLiteral);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_CharacterLiteral(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_CharacterLiteral(self)
    }
}

/// /** comments are treated as docstrings. They may only appear immediately in
/// front of a rule or toplevel alternative in a grammar production rule.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtDoc<L: types::Location = types::SingleFileLocation> {
    pub data: Option<types::TerminalTokenData<L>>,
}

impl<L: types::Location> PtDoc<L> {
    fn new(token: types::TerminalToken<TokenType, L>, token_span: std::ops::Range<usize>) -> Self {
        let (token_type, text, span) = token.unwrap();
        assert!(token_type == TokenType::Doc);
        Self {
            data: Some(types::TerminalTokenData {
                index: token_span.start(),
                text: text.unwrap_or_default(),
                span,
            }),
        }
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Doc(self)
    }

    /// Visit this terminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Doc(self)
    }
}

/// A grammar is a list of one or more rules.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtGrammar<L: types::Location + 'static>(Vec<Box<PtRule<L>>>);

impl<L: types::Location + 'static> PtGrammar<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtGrammar<L>, L> {
        make_parsers().0
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Grammar(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Grammar(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        let x1 = &self.0;
        for x in x1.iter() {
            visitor.visit_Rule(x.as_ref())?;
        }
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        let x1 = &mut self.0;
        for x in x1.iter_mut() {
            visitor.visit_Rule(x.as_mut())?;
        }
        Ok(())
    }
}

/// A rule can be either a grammar or a tokenizer rule. If a rule is defined
/// more than once, each definition will just add to the alternatives.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtRule<L: types::Location + 'static = types::SingleFileLocation> {
    /// Used to recover from errors.
    Error,

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

impl<L: types::Location + 'static> Default for PtRule<L> {
    fn default() -> Self {
        PtRule::Error
    }
}

impl<L: types::Location + 'static> PtRule<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtRule<L>, L> {
        make_parsers().1
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Rule(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Rule(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        match self {
            PtRule::Error => (),
            PtRule::TokenRule(x1, x2, x3, x4, x5) => {
                if let Some(x) = x1.as_ref() {
                    visitor.visit_Doc(x)?;
                }
                visitor.visit_Symbol(x2)?;
                visitor.visit_ColEqEq(x3)?;
                visitor.visit_Alternation(x4.as_ref())?;
                visitor.visit_Sem(x5)?;
            }
            PtRule::WhitespaceRule(x1, x2, x3, x4, x5) => {
                if let Some(x) = x1.as_ref() {
                    visitor.visit_Doc(x)?;
                }
                visitor.visit_Usc(x2)?;
                visitor.visit_ColEqEq(x3)?;
                visitor.visit_Alternation(x4.as_ref())?;
                visitor.visit_Sem(x5)?;
            }
            PtRule::GrammarRule(x1, x2, x3, x4, x5) => {
                if let Some(x) = x1.as_ref() {
                    visitor.visit_Doc(x)?;
                }
                visitor.visit_Symbol(x2)?;
                visitor.visit_FirstAlter(x3.as_ref())?;
                for x in x4.iter() {
                    visitor.visit_SubseqAlter(x.as_ref())?;
                }
                visitor.visit_Sem(x5)?;
            }
        }
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        match self {
            PtRule::Error => (),
            PtRule::TokenRule(x1, x2, x3, x4, x5) => {
                if let Some(x) = x1.as_mut() {
                    visitor.visit_Doc(x)?;
                }
                visitor.visit_Symbol(x2)?;
                visitor.visit_ColEqEq(x3)?;
                visitor.visit_Alternation(x4.as_mut())?;
                visitor.visit_Sem(x5)?;
            }
            PtRule::WhitespaceRule(x1, x2, x3, x4, x5) => {
                if let Some(x) = x1.as_mut() {
                    visitor.visit_Doc(x)?;
                }
                visitor.visit_Usc(x2)?;
                visitor.visit_ColEqEq(x3)?;
                visitor.visit_Alternation(x4.as_mut())?;
                visitor.visit_Sem(x5)?;
            }
            PtRule::GrammarRule(x1, x2, x3, x4, x5) => {
                if let Some(x) = x1.as_mut() {
                    visitor.visit_Doc(x)?;
                }
                visitor.visit_Symbol(x2)?;
                visitor.visit_FirstAlter(x3.as_mut())?;
                for x in x4.iter_mut() {
                    visitor.visit_SubseqAlter(x.as_mut())?;
                }
                visitor.visit_Sem(x5)?;
            }
        }
        Ok(())
    }
}

/// `a | b | ...` preferentially matches `a`, matches `b` if `a` does not
/// match, and so on.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtAlternation<L: types::Location + 'static>(
    Box<PtConcatenation<L>>,
    Vec<(PtBar<L>, Box<PtConcatenation<L>>)>,
);

impl<L: types::Location + 'static> PtAlternation<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtAlternation<L>, L> {
        make_parsers().2
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Alternation(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Alternation(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        let x1 = &self.0;
        let x2 = &self.1;
        visitor.visit_Concatenation(x1.as_ref())?;
        for (x1, x2) in x2.iter() {
            visitor.visit_Bar(x1)?;
            visitor.visit_Concatenation(x2.as_ref())?;
        }
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        let x1 = &mut self.0;
        let x2 = &mut self.1;
        visitor.visit_Concatenation(x1.as_mut())?;
        for (x1, x2) in x2.iter_mut() {
            visitor.visit_Bar(x1)?;
            visitor.visit_Concatenation(x2.as_mut())?;
        }
        Ok(())
    }
}

/// The alternatives at the root of a grammar production rule can be annotated
/// with a variant name (if not specified it is auto-generated when needed) at
/// the end and can be given a docstring before the ::= or | that starts the
/// variant. They are functionally indistinguishable from a normal, nested
/// alternation, though the parse tree may be generated in a different way.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
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

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_FirstAlter(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_FirstAlter(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        let x1 = &self.0;
        let x2 = &self.1;
        let x3 = &self.2;
        let x4 = &self.3;
        if let Some(x) = x1.as_ref() {
            visitor.visit_Doc(x)?;
        }
        visitor.visit_ColColEq(x2)?;
        visitor.visit_Concatenation(x3.as_ref())?;
        if let Some(x) = x4.as_ref() {
            visitor.visit_AlterName(x.as_ref())?;
        }
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        let x1 = &mut self.0;
        let x2 = &mut self.1;
        let x3 = &mut self.2;
        let x4 = &mut self.3;
        if let Some(x) = x1.as_mut() {
            visitor.visit_Doc(x)?;
        }
        visitor.visit_ColColEq(x2)?;
        visitor.visit_Concatenation(x3.as_mut())?;
        if let Some(x) = x4.as_mut() {
            visitor.visit_AlterName(x.as_mut())?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
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

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_SubseqAlter(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_SubseqAlter(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        let x1 = &self.0;
        let x2 = &self.1;
        let x3 = &self.2;
        let x4 = &self.3;
        if let Some(x) = x1.as_ref() {
            visitor.visit_Doc(x)?;
        }
        visitor.visit_Bar(x2)?;
        visitor.visit_Concatenation(x3.as_ref())?;
        if let Some(x) = x4.as_ref() {
            visitor.visit_AlterName(x.as_ref())?;
        }
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        let x1 = &mut self.0;
        let x2 = &mut self.1;
        let x3 = &mut self.2;
        let x4 = &mut self.3;
        if let Some(x) = x1.as_mut() {
            visitor.visit_Doc(x)?;
        }
        visitor.visit_Bar(x2)?;
        visitor.visit_Concatenation(x3.as_mut())?;
        if let Some(x) = x4.as_mut() {
            visitor.visit_AlterName(x.as_mut())?;
        }
        Ok(())
    }
}

/// A toplevel alternative can be named by appending `-> name`. The names do not
/// have semantical meaning (they cannot be referred to within the grammar),
/// but are used to attach names to variants in the generated ASTs.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtAlterName<L: types::Location + 'static>(PtMinGt<L>, PtSymbol<L>);

impl<L: types::Location + 'static> PtAlterName<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtAlterName<L>, L> {
        make_parsers().5
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_AlterName(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_AlterName(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        let x1 = &self.0;
        let x2 = &self.1;
        visitor.visit_MinGt(x1)?;
        visitor.visit_Symbol(x2)?;
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        let x1 = &mut self.0;
        let x2 = &mut self.1;
        visitor.visit_MinGt(x1)?;
        visitor.visit_Symbol(x2)?;
        Ok(())
    }
}

/// `a b ...` matches `a` followed by `b` and so on.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct PtConcatenation<L: types::Location + 'static>(Vec<Box<PtRepetition<L>>>);

impl<L: types::Location + 'static> PtConcatenation<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtConcatenation<L>, L> {
        make_parsers().6
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Concatenation(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Concatenation(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        let x1 = &self.0;
        for x in x1.iter() {
            visitor.visit_Repetition(x.as_ref())?;
        }
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        let x1 = &mut self.0;
        for x in x1.iter_mut() {
            visitor.visit_Repetition(x.as_mut())?;
        }
        Ok(())
    }
}

/// The usual ?*+ characters can be used to specify repetition.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtRepetition<L: types::Location + 'static = types::SingleFileLocation> {
    /// Used to recover from errors.
    Error,

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

impl<L: types::Location + 'static> Default for PtRepetition<L> {
    fn default() -> Self {
        PtRepetition::Error
    }
}

impl<L: types::Location + 'static> PtRepetition<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtRepetition<L>, L> {
        make_parsers().7
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Repetition(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Repetition(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        match self {
            PtRepetition::Error => (),
            PtRepetition::Maybe(x1, x2) => {
                visitor.visit_Singular(x1.as_ref())?;
                visitor.visit_Qst(x2)?;
            }
            PtRepetition::Any(x1, x2) => {
                visitor.visit_Singular(x1.as_ref())?;
                visitor.visit_Ast(x2)?;
            }
            PtRepetition::Many(x1, x2) => {
                visitor.visit_Singular(x1.as_ref())?;
                visitor.visit_Pls(x2)?;
            }
            PtRepetition::One(x1) => {
                visitor.visit_Singular(x1.as_ref())?;
            }
        }
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        match self {
            PtRepetition::Error => (),
            PtRepetition::Maybe(x1, x2) => {
                visitor.visit_Singular(x1.as_mut())?;
                visitor.visit_Qst(x2)?;
            }
            PtRepetition::Any(x1, x2) => {
                visitor.visit_Singular(x1.as_mut())?;
                visitor.visit_Ast(x2)?;
            }
            PtRepetition::Many(x1, x2) => {
                visitor.visit_Singular(x1.as_mut())?;
                visitor.visit_Pls(x2)?;
            }
            PtRepetition::One(x1) => {
                visitor.visit_Singular(x1.as_mut())?;
            }
        }
        Ok(())
    }
}

/// A single literal pattern or referenced rule to match, or a parenthesized
/// expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PtSingular<L: types::Location + 'static = types::SingleFileLocation> {
    /// Used to recover from errors.
    Error,

    Nested(PtLp<L>, Box<PtAlternation<L>>, PtRp<L>),

    Symbol(PtSymbol<L>),

    StringLiteral(PtStringLiteral<L>),

    CharacterSet(PtCharacterSet<L>),

    CharacterLiteral(PtCharacterLiteral<L>),
}

impl<L: types::Location + 'static> Default for PtSingular<L> {
    fn default() -> Self {
        PtSingular::Error
    }
}

impl<L: types::Location + 'static> PtSingular<L> {
    /// Return a Chumsky parser for this rule.
    pub fn parser<'a>() -> BoxedParser<'a, PtSingular<L>, L> {
        make_parsers().8
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Singular(self)
    }

    /// Visit this nonterminal node with the given visitor.
    pub fn visit_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        visitor.visit_Singular(self)
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse<E, V: Visitor<L, E>>(&self, visitor: &mut V) -> Result<(), E> {
        match self {
            PtSingular::Error => (),
            PtSingular::Nested(x1, x2, x3) => {
                visitor.visit_Lp(x1)?;
                visitor.visit_Alternation(x2.as_ref())?;
                visitor.visit_Rp(x3)?;
            }
            PtSingular::Symbol(x1) => {
                visitor.visit_Symbol(x1)?;
            }
            PtSingular::StringLiteral(x1) => {
                visitor.visit_StringLiteral(x1)?;
            }
            PtSingular::CharacterSet(x1) => {
                visitor.visit_CharacterSet(x1)?;
            }
            PtSingular::CharacterLiteral(x1) => {
                visitor.visit_CharacterLiteral(x1)?;
            }
        }
        Ok(())
    }

    /// Visit the children of this nonterminal node with the given visitor.
    pub fn traverse_mut<E, V: VisitorMut<L, E>>(&mut self, visitor: &mut V) -> Result<(), E> {
        match self {
            PtSingular::Error => (),
            PtSingular::Nested(x1, x2, x3) => {
                visitor.visit_Lp(x1)?;
                visitor.visit_Alternation(x2.as_mut())?;
                visitor.visit_Rp(x3)?;
            }
            PtSingular::Symbol(x1) => {
                visitor.visit_Symbol(x1)?;
            }
            PtSingular::StringLiteral(x1) => {
                visitor.visit_StringLiteral(x1)?;
            }
            PtSingular::CharacterSet(x1) => {
                visitor.visit_CharacterSet(x1)?;
            }
            PtSingular::CharacterLiteral(x1) => {
                visitor.visit_CharacterLiteral(x1)?;
            }
        }
        Ok(())
    }
}

/// Constructs Chumsky parsers for all the grammar rules.
#[allow(non_snake_case)]
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
            .boxed()
            .map(|x| PtGrammar(x))
            .boxed(),
    );

    parse_Rule.define(
        just(types::TerminalToken::new_pattern(TokenType::Doc))
            .map_with_span(PtDoc::new)
            .boxed()
            .or_not()
            .boxed()
            .then(
                just(types::TerminalToken::new_pattern(TokenType::Symbol))
                    .map_with_span(PtSymbol::new)
                    .boxed(),
            )
            .then(
                just(types::TerminalToken::new_pattern(TokenType::ColEqEq))
                    .map_with_span(PtColEqEq::new)
                    .boxed(),
            )
            .then(parse_Alternation.clone().map(Box::new))
            .then(
                just(types::TerminalToken::new_pattern(TokenType::Sem))
                    .map_with_span(PtSem::new)
                    .boxed(),
            )
            .map(|((((x1, x2), x3), x4), x5)| (x1, x2, x3, x4, x5))
            .boxed()
            .map(|(x1, x2, x3, x4, x5)| PtRule::TokenRule(x1, x2, x3, x4, x5))
            .or(just(types::TerminalToken::new_pattern(TokenType::Doc))
                .map_with_span(PtDoc::new)
                .boxed()
                .or_not()
                .boxed()
                .then(
                    just(types::TerminalToken::new_pattern(TokenType::Usc))
                        .map_with_span(PtUsc::new)
                        .boxed(),
                )
                .then(
                    just(types::TerminalToken::new_pattern(TokenType::ColEqEq))
                        .map_with_span(PtColEqEq::new)
                        .boxed(),
                )
                .then(parse_Alternation.clone().map(Box::new))
                .then(
                    just(types::TerminalToken::new_pattern(TokenType::Sem))
                        .map_with_span(PtSem::new)
                        .boxed(),
                )
                .map(|((((x1, x2), x3), x4), x5)| (x1, x2, x3, x4, x5))
                .boxed()
                .map(|(x1, x2, x3, x4, x5)| PtRule::WhitespaceRule(x1, x2, x3, x4, x5)))
            .or(just(types::TerminalToken::new_pattern(TokenType::Doc))
                .map_with_span(PtDoc::new)
                .boxed()
                .or_not()
                .boxed()
                .then(
                    just(types::TerminalToken::new_pattern(TokenType::Symbol))
                        .map_with_span(PtSymbol::new)
                        .boxed(),
                )
                .then(parse_FirstAlter.clone().map(Box::new))
                .then(parse_SubseqAlter.clone().map(Box::new).repeated().boxed())
                .then(
                    just(types::TerminalToken::new_pattern(TokenType::Sem))
                        .map_with_span(PtSem::new)
                        .boxed(),
                )
                .map(|((((x1, x2), x3), x4), x5)| (x1, x2, x3, x4, x5))
                .boxed()
                .map(|(x1, x2, x3, x4, x5)| PtRule::GrammarRule(x1, x2, x3, x4, x5)))
            .boxed(),
    );

    parse_Alternation.define(
        parse_Concatenation
            .clone()
            .map(Box::new)
            .then(
                just(types::TerminalToken::new_pattern(TokenType::Bar))
                    .map_with_span(PtBar::new)
                    .boxed()
                    .then(parse_Concatenation.clone().map(Box::new))
                    .map(|(x1, x2)| (x1, x2))
                    .boxed()
                    .repeated()
                    .boxed(),
            )
            .map(|(x1, x2)| (x1, x2))
            .boxed()
            .map(|(x1, x2)| PtAlternation(x1, x2))
            .boxed(),
    );

    parse_FirstAlter.define(
        just(types::TerminalToken::new_pattern(TokenType::Doc))
            .map_with_span(PtDoc::new)
            .boxed()
            .or_not()
            .boxed()
            .then(
                just(types::TerminalToken::new_pattern(TokenType::ColColEq))
                    .map_with_span(PtColColEq::new)
                    .boxed(),
            )
            .then(parse_Concatenation.clone().map(Box::new))
            .then(parse_AlterName.clone().map(Box::new).or_not().boxed())
            .map(|(((x1, x2), x3), x4)| (x1, x2, x3, x4))
            .boxed()
            .map(|(x1, x2, x3, x4)| PtFirstAlter(x1, x2, x3, x4))
            .boxed(),
    );

    parse_SubseqAlter.define(
        just(types::TerminalToken::new_pattern(TokenType::Doc))
            .map_with_span(PtDoc::new)
            .boxed()
            .or_not()
            .boxed()
            .then(
                just(types::TerminalToken::new_pattern(TokenType::Bar))
                    .map_with_span(PtBar::new)
                    .boxed(),
            )
            .then(parse_Concatenation.clone().map(Box::new))
            .then(parse_AlterName.clone().map(Box::new).or_not().boxed())
            .map(|(((x1, x2), x3), x4)| (x1, x2, x3, x4))
            .boxed()
            .map(|(x1, x2, x3, x4)| PtSubseqAlter(x1, x2, x3, x4))
            .boxed(),
    );

    parse_AlterName.define(
        just(types::TerminalToken::new_pattern(TokenType::MinGt))
            .map_with_span(PtMinGt::new)
            .boxed()
            .then(
                just(types::TerminalToken::new_pattern(TokenType::Symbol))
                    .map_with_span(PtSymbol::new)
                    .boxed(),
            )
            .map(|(x1, x2)| (x1, x2))
            .boxed()
            .map(|(x1, x2)| PtAlterName(x1, x2))
            .boxed(),
    );

    parse_Concatenation.define(
        parse_Repetition
            .clone()
            .map(Box::new)
            .repeated()
            .at_least(1)
            .boxed()
            .map(|x| PtConcatenation(x))
            .boxed(),
    );

    parse_Repetition.define(
        parse_Singular
            .clone()
            .map(Box::new)
            .then(
                just(types::TerminalToken::new_pattern(TokenType::Qst))
                    .map_with_span(PtQst::new)
                    .boxed(),
            )
            .map(|(x1, x2)| (x1, x2))
            .boxed()
            .map(|(x1, x2)| PtRepetition::Maybe(x1, x2))
            .or(parse_Singular
                .clone()
                .map(Box::new)
                .then(
                    just(types::TerminalToken::new_pattern(TokenType::Ast))
                        .map_with_span(PtAst::new)
                        .boxed(),
                )
                .map(|(x1, x2)| (x1, x2))
                .boxed()
                .map(|(x1, x2)| PtRepetition::Any(x1, x2)))
            .or(parse_Singular
                .clone()
                .map(Box::new)
                .then(
                    just(types::TerminalToken::new_pattern(TokenType::Pls))
                        .map_with_span(PtPls::new)
                        .boxed(),
                )
                .map(|(x1, x2)| (x1, x2))
                .boxed()
                .map(|(x1, x2)| PtRepetition::Many(x1, x2)))
            .or(parse_Singular
                .clone()
                .map(Box::new)
                .map(|x| PtRepetition::One(x)))
            .boxed(),
    );

    parse_Singular.define(
        just(types::TerminalToken::new_pattern(TokenType::Lp))
            .map_with_span(PtLp::new)
            .boxed()
            .then(parse_Alternation.clone().map(Box::new))
            .then(
                just(types::TerminalToken::new_pattern(TokenType::Rp))
                    .map_with_span(PtRp::new)
                    .boxed(),
            )
            .map(|((x1, x2), x3)| (x1, x2, x3))
            .recover_with(skip_until([types::TerminalToken::new_pattern(TokenType::Sem)], |_| Default::default()).consume_end())
            .boxed()
            .map(|(x1, x2, x3)| PtSingular::Nested(x1, x2, x3))
            .or(just(types::TerminalToken::new_pattern(TokenType::Symbol))
                .map_with_span(PtSymbol::new)
                .boxed()
                .map(|x| PtSingular::Symbol(x)))
            .or(
                just(types::TerminalToken::new_pattern(TokenType::StringLiteral))
                    .map_with_span(PtStringLiteral::new)
                    .boxed()
                    .map(|x| PtSingular::StringLiteral(x)),
            )
            .or(
                just(types::TerminalToken::new_pattern(TokenType::CharacterSet))
                    .map_with_span(PtCharacterSet::new)
                    .boxed()
                    .map(|x| PtSingular::CharacterSet(x)),
            )
            .or(just(types::TerminalToken::new_pattern(
                TokenType::CharacterLiteral,
            ))
            .map_with_span(PtCharacterLiteral::new)
            .boxed()
            .map(|x| PtSingular::CharacterLiteral(x)))
            .boxed(),
    );

    (
        parse_Grammar.then_ignore(end()).boxed(),
        parse_Rule.then_ignore(end()).boxed(),
        parse_Alternation.then_ignore(end()).boxed(),
        parse_FirstAlter.then_ignore(end()).boxed(),
        parse_SubseqAlter.then_ignore(end()).boxed(),
        parse_AlterName.then_ignore(end()).boxed(),
        parse_Concatenation.then_ignore(end()).boxed(),
        parse_Repetition.then_ignore(end()).boxed(),
        parse_Singular.then_ignore(end()).boxed(),
    )
}

/// Visitor trait for immutably walking the parse tree.
pub trait Visitor<L = types::SingleFileLocation, E = ()>
where
    L: types::Location,
    Self: Sized,
{
    #[allow(non_snake_case)]
    fn visit_ColEqEq(&mut self, _x: &PtColEqEq<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Sem(&mut self, _x: &PtSem<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Usc(&mut self, _x: &PtUsc<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Symbol(&mut self, _x: &PtSymbol<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Bar(&mut self, _x: &PtBar<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_ColColEq(&mut self, _x: &PtColColEq<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_MinGt(&mut self, _x: &PtMinGt<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Qst(&mut self, _x: &PtQst<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Ast(&mut self, _x: &PtAst<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Pls(&mut self, _x: &PtPls<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Lp(&mut self, _x: &PtLp<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Rp(&mut self, _x: &PtRp<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_StringLiteral(&mut self, _x: &PtStringLiteral<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_CharacterSet(&mut self, _x: &PtCharacterSet<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_CharacterLiteral(&mut self, _x: &PtCharacterLiteral<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Doc(&mut self, _x: &PtDoc<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Grammar(&mut self, x: &PtGrammar<L>) -> Result<(), E> {
        x.traverse(self)
    }

    #[allow(non_snake_case)]
    fn visit_Rule(&mut self, x: &PtRule<L>) -> Result<(), E> {
        x.traverse(self)
    }

    #[allow(non_snake_case)]
    fn visit_Alternation(&mut self, x: &PtAlternation<L>) -> Result<(), E> {
        x.traverse(self)
    }

    #[allow(non_snake_case)]
    fn visit_FirstAlter(&mut self, x: &PtFirstAlter<L>) -> Result<(), E> {
        x.traverse(self)
    }

    #[allow(non_snake_case)]
    fn visit_SubseqAlter(&mut self, x: &PtSubseqAlter<L>) -> Result<(), E> {
        x.traverse(self)
    }

    #[allow(non_snake_case)]
    fn visit_AlterName(&mut self, x: &PtAlterName<L>) -> Result<(), E> {
        x.traverse(self)
    }

    #[allow(non_snake_case)]
    fn visit_Concatenation(&mut self, x: &PtConcatenation<L>) -> Result<(), E> {
        x.traverse(self)
    }

    #[allow(non_snake_case)]
    fn visit_Repetition(&mut self, x: &PtRepetition<L>) -> Result<(), E> {
        x.traverse(self)
    }

    #[allow(non_snake_case)]
    fn visit_Singular(&mut self, x: &PtSingular<L>) -> Result<(), E> {
        x.traverse(self)
    }
}

/// Visitor trait for mutably walking the parse tree.
pub trait VisitorMut<L = types::SingleFileLocation, E = ()>
where
    L: types::Location,
    Self: Sized,
{
    #[allow(non_snake_case)]
    fn visit_ColEqEq(&mut self, _x: &mut PtColEqEq<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Sem(&mut self, _x: &mut PtSem<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Usc(&mut self, _x: &mut PtUsc<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Symbol(&mut self, _x: &mut PtSymbol<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Bar(&mut self, _x: &mut PtBar<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_ColColEq(&mut self, _x: &mut PtColColEq<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_MinGt(&mut self, _x: &mut PtMinGt<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Qst(&mut self, _x: &mut PtQst<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Ast(&mut self, _x: &mut PtAst<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Pls(&mut self, _x: &mut PtPls<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Lp(&mut self, _x: &mut PtLp<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Rp(&mut self, _x: &mut PtRp<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_StringLiteral(&mut self, _x: &mut PtStringLiteral<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_CharacterSet(&mut self, _x: &mut PtCharacterSet<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_CharacterLiteral(&mut self, _x: &mut PtCharacterLiteral<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Doc(&mut self, _x: &mut PtDoc<L>) -> Result<(), E> {
        Ok(())
    }

    #[allow(non_snake_case)]
    fn visit_Grammar(&mut self, x: &mut PtGrammar<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }

    #[allow(non_snake_case)]
    fn visit_Rule(&mut self, x: &mut PtRule<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }

    #[allow(non_snake_case)]
    fn visit_Alternation(&mut self, x: &mut PtAlternation<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }

    #[allow(non_snake_case)]
    fn visit_FirstAlter(&mut self, x: &mut PtFirstAlter<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }

    #[allow(non_snake_case)]
    fn visit_SubseqAlter(&mut self, x: &mut PtSubseqAlter<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }

    #[allow(non_snake_case)]
    fn visit_AlterName(&mut self, x: &mut PtAlterName<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }

    #[allow(non_snake_case)]
    fn visit_Concatenation(&mut self, x: &mut PtConcatenation<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }

    #[allow(non_snake_case)]
    fn visit_Repetition(&mut self, x: &mut PtRepetition<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }

    #[allow(non_snake_case)]
    fn visit_Singular(&mut self, x: &mut PtSingular<L>) -> Result<(), E> {
        x.traverse_mut(self)
    }
}

/// Internal type used to propagate the text and span information from the
/// token list into the parse tree. This is necessary because Chumsky's
/// [just()] parser and friends place a clone of the token pattern into the
/// parse tree, rather than cloning the incoming token. [filter()] could work,
/// but can't derive which tokens were expected when constructing the error
/// message (for obvious reasons). [chumsky::custom()] and otherwise
/// implementing [chumsky::Parser] manually is also not possible, because
/// the necessary members of [chumsky::Stream] are private to the crate
/// (d'oh). So, here we are.
pub struct Annotator<'a, L: types::Location = types::SingleFileLocation> {
    tokens: &'a [types::TerminalToken<TokenType, L>],
}

impl<'a, L: types::Location> Annotator<'a, L> {
    pub fn new(tokens: &'a [types::TerminalToken<TokenType, L>]) -> Self {
        Self { tokens }
    }
}

impl<'a, L: types::Location> VisitorMut<L> for Annotator<'a, L> {
    fn visit_ColEqEq(&mut self, x: &mut PtColEqEq<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Sem(&mut self, x: &mut PtSem<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Usc(&mut self, x: &mut PtUsc<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Symbol(&mut self, x: &mut PtSymbol<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Bar(&mut self, x: &mut PtBar<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_ColColEq(&mut self, x: &mut PtColColEq<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_MinGt(&mut self, x: &mut PtMinGt<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Qst(&mut self, x: &mut PtQst<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Ast(&mut self, x: &mut PtAst<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Pls(&mut self, x: &mut PtPls<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Lp(&mut self, x: &mut PtLp<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Rp(&mut self, x: &mut PtRp<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_StringLiteral(&mut self, x: &mut PtStringLiteral<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_CharacterSet(&mut self, x: &mut PtCharacterSet<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_CharacterLiteral(&mut self, x: &mut PtCharacterLiteral<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }

    fn visit_Doc(&mut self, x: &mut PtDoc<L>) -> Result<(), ()> {
        if let Some(data) = x.data.as_mut() {
            data.annotate_from_token_list(self.tokens);
        }
        Ok(())
    }
}
