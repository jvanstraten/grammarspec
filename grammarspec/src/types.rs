
/// Trait for a source location marker, usually within a [Span]. The display
/// trait of the span will format the first element of the location with the
/// default formatter, and the second element with the alternate formatter.
pub trait Location: Clone + std::fmt::Debug + Eq + std::hash::Hash + std::fmt::Display + Default {
    /// Assuming the current location is at the start of the string slice,
    /// update the location such that it now represents the end of the string
    /// slice.
    fn advance(&mut self, text: &str);

    /// Compares the two locations, returning true if self comes strictly
    /// before other. If the two locations are insufficiently related for a
    /// comparison, None is returned.
    fn lt(&self, other: &Self) -> Option<bool>;

    /// Returns whether this and the other span belong to the same source.
    fn related(&self, other: &Self) -> bool {
        self.lt(other).is_some()
    }
}

/// A basic implementation of [Location] that stores character offset and
/// line/column information based on \n.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SingleFileLocation {
    pub line: u32,
    pub column: u32,
    pub offset: usize,
}

impl std::fmt::Display for SingleFileLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Default for SingleFileLocation {
    /// Returns the location at the start of the file.
    fn default() -> Self {
        SingleFileLocation {
            line: 1,
            column: 1,
            offset: 0,
        }
    }
}

impl Location for SingleFileLocation {
    fn advance(&mut self, text: &str) {
        for c in text.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.offset += 1;
        }
    }

    fn lt(&self, other: &Self) -> Option<bool> {
        Some(self.offset < other.offset)
    }
}

//=============================================================================

/// A span of two (related) source locations.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct Span<L: Location = SingleFileLocation> {
    a: L,
    b: L,
}

impl<L: Location> std::fmt::Display for Span<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{:#}", self.a, self.b)
    }
}

impl<L: Location> chumsky::Span for Span<L> {
    type Context = ();
    type Offset = L;

    fn new(_context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self { a: range.start, b: range.end }
    }

    fn context(&self) -> Self::Context {
        ()
    }

    fn start(&self) -> Self::Offset {
        self.a.clone()
    }

    fn end(&self) -> Self::Offset {
        self.b.clone()
    }
}

impl<'a, L: Location + 'a> Span<L> {
    /// Creates a span by means of two locations. b is exclusive. Panics if
    /// the two spans are not related.
    pub fn new(a: L, b: L) -> Self {
        assert!(a.related(&b));
        Self { a, b }
    }

    /// Creates a span for the given text at the given location by advancing
    /// the location past the text.
    pub fn for_text(loc: &mut L, text: &str) -> Self {
        let a = loc.clone();
        loc.advance(text);
        Self { a, b: loc.clone() }
    }

    /// Constructs a span that encompasses the given iterable of spans. Panics
    /// if the iterator does not yield any spans or when the spans are
    /// unrelated.
    pub fn combine<T: IntoIterator<Item = &'a Self>>(spans: T) -> Self {
        let mut iter = spans.into_iter().filter(|x| !x.is_null());
        if let Some(first) = iter.next() {
            let (mut a, mut b) = first.clone().unwrap();
            for span in iter {
                if span.a.lt(&a).unwrap() {
                    a = span.a.clone();
                }
                if b.lt(&span.b).unwrap() {
                    b = span.b.clone();
                }
            }
            Self { a, b }
        } else {
            Self::default()
        }
    }

    /// Returns whether this is a null/empty span, i.e. the locations are
    /// equal.
    pub fn is_null(&self) -> bool {
        self.a == self.b
    }

    /// Returns the origin of the span.
    pub fn from(&self) -> &L {
        &self.a
    }

    /// Returns one character past the end of the span.
    pub fn to(&self) -> &L {
        &self.b
    }

    /// Decomposes the span into its two locations.
    pub fn unwrap(self) -> (L, L) {
        (self.a, self.b)
    }
}

//=============================================================================

/// Trait for things that have a span.
pub trait Spanned {
    type Location: Location;

    fn span(&self) -> &Span<Self::Location>;
}

//=============================================================================

/// Trait for the types of tokens within a language.
pub trait TokenType: Copy + Eq + std::hash::Hash + std::fmt::Debug + std::fmt::Display {
    /// Array-like type returned by all().
    type All: IntoIterator<Item = Self>;

    /// Returns an iterator that iterates over all tokens, in the order in
    /// which they should be matched by the tokenizer.
    fn all() -> Self::All;

    /// Returns the regex for this token type.
    fn regex(&self) -> &'static str;

    /// Returns the regex for matching whitespace between the real tokens.
    fn whitespace() -> &'static str;
}

//=============================================================================

/// Token classification.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenClass<T: TokenType> {
    /// Used for normal tokens, that the parser should consume.
    Normal(T),

    /// Used for the whitespace between normal tokens.
    Whitespace,

    /// Used to recover from errors; matches any single character when nothing
    /// else matches.
    Error,
}

impl<T: TokenType> TokenClass<T> {
    /// Returns the embedded token type if there is one.
    fn token_type(&self) -> Option<T> {
        if let Self::Normal(token_type) = self {
            Some(*token_type)
        } else {
            None
        }
    }

    /// Returns whether this is an error token.
    fn is_error(&self) -> bool {
        matches!(self, Self::Error)
    }

    /// Returns whether this is a whitespace token.
    fn is_whitespace(&self) -> bool {
        matches!(self, Self::Whitespace)
    }
}

impl <T: TokenType> std::fmt::Display for TokenClass<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenClass::Normal(token_type) => std::fmt::Display::fmt(token_type, f),
            TokenClass::Whitespace => write!(f, "whitespace"),
            TokenClass::Error => write!(f, "error"),
        }
    }
}

//=============================================================================

/// Token type used as terminals at the input of the grammar. This type is a
/// bit derpy to make Chumsky work right: two terminal tokens are equal iff
/// they have the same type, with no bearing on the text or span. The text or
/// span are treated as optional annotations.
#[derive(Clone, Debug, Eq, Hash)]
pub struct GrammarInput<T: TokenType, L: Location = SingleFileLocation> {
    token_type: T,
    text: Option<String>,
    span: Span<L>,
}

impl<T: TokenType, L: Location> PartialEq for GrammarInput<T, L> {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type
    }
}

impl<T: TokenType, L: Location> GrammarInput<T, L> {
    /// Creates a new terminal node.
    pub fn new<S: ToString>(token_type: T, text: S, span: Span<L>) -> Self {
        Self { token_type, text: Some(text.to_string()), span }
    }

    /// Creates a new "pattern" node, only used for error messages and to
    /// compare against.
    pub fn new_pattern(token_type: T) -> Self {
        Self { token_type, text: None, span: Span::default() }
    }

    /// Returns the token type.
    pub fn token_type(&self) -> T {
        self.token_type
    }

    /// Returns the text enclosed by the token.
    pub fn text(&self) -> &str {
        self.text.as_ref().map(|x| &x[..]).unwrap_or("")
    }

    /// Unwraps the contents into a tuple.
    pub fn unwrap(self) -> (T, Option<String>, Span<L>) {
        (self.token_type, self.text, self.span)
    }
}

impl<T: TokenType, L: Location> std::fmt::Display for GrammarInput<T, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(text) = &self.text {
            write!(f, "{} ({:?}) at {}", self.token_type, text, self.span)
        } else {
            write!(f, "{}", self.token_type)
        }
    }
}

impl<T: TokenType, L: Location> Spanned for GrammarInput<T, L> {
    type Location = L;

    fn span(&self) -> &Span<Self::Location> {
        &self.span
    }
}

//=============================================================================

/// Error type used for a failure to tokenize.
#[derive(Clone, Debug)]
pub struct TokenizerError<L: Location = SingleFileLocation> {
    invalid_char: char,
    location: L,
}

impl<L: Location> std::fmt::Display for TokenizerError<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "failed to match character {:?} as the start of a token at {}", self.invalid_char, self.location)
    }
}

impl<L: Location> TokenizerError<L> {
    pub fn new(invalid_char: char, location: L) -> Self {
        Self { invalid_char, location }
    }
}

//=============================================================================

/// Tokens as emitted by the tokenizer.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token<'a, T: TokenType, L: Location = SingleFileLocation> {
    class: TokenClass<T>,
    text: &'a str,
    span: Span<L>,
}

impl <'a, T: TokenType, L: Location> Token<'a, T, L> {
    /// Creates a new token.
    pub fn new(class: TokenClass<T>, text: &'a str, span: Span<L>) -> Self {
        Self { class, text, span }
    }

    /// Converts self to a [GrammarInput] if it is a normal token. Otherwise
    /// returns Err(self).
    pub fn to_grammar_input(self) -> Result<GrammarInput<T, L>, Self> {
        if let Some(token_type) = self.class.token_type() {
            Ok(GrammarInput::new(token_type, self.text, self.span))
        } else {
            Err(self)
        }
    }

    /// Converts self to a [TokenizerError] if it is an error marker. Otherwise
    /// returns Err(self).
    pub fn to_error(self) -> Result<TokenizerError<L>, Self> {
        if self.class.is_error() {
            Ok(TokenizerError::new(self.text.chars().next().unwrap_or('?'), self.span.unwrap().0))
        } else {
            Err(self)
        }
    }

    /// Returns the class of this token.
    pub fn class(&self) -> TokenClass<T> {
        self.class
    }

    /// Returns the text encompassed by the token.
    pub fn text(&self) -> &'a str {
        self.text
    }
}

impl<'a, T: TokenType, L: Location> std::fmt::Display for Token<'a, T, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} ({:?} at {})", self.class, self.text, self.span)
    }
}

impl<'a, T: TokenType, L: Location> Spanned for Token<'a, T, L> {
    type Location = L;

    fn span(&self) -> &Span<Self::Location> {
        &self.span
    }
}

//=============================================================================

pub struct Tokenizer<'s, T: TokenType, L: Location = SingleFileLocation> {
    regexes: Vec<(regex::Regex, TokenClass<T>)>,
    location: L,
    remain: &'s str,
}

impl<'s, T: TokenType, L: Location> Tokenizer<'s, T, L> {
    /// Creates a tokenizer for a piece of text with the given initial location
    /// for spans.
    pub fn new_with_location(location: L, text: &'s str) -> Self {
        let mut regexes: Vec<_> = T::all().into_iter().map(|x| (regex::Regex::new(&format!("^(?:{})", x.regex())).unwrap(), TokenClass::Normal(x))).collect();
        regexes.push((regex::Regex::new(&format!("^(?:{})", T::whitespace())).unwrap(), TokenClass::Whitespace));
        Self {
            regexes,
            location,
            remain: text,
        }
    }

    /// Creates a tokenizer for a piece of text, using L::default() for the
    /// initial location.
    pub fn new(text: &'s str) -> Self {
        Self::new_with_location(L::default(), text)
    }
}

impl<'s, T: TokenType, L: Location> Iterator for Tokenizer<'s, T, L> {
    type Item = Token<'s, T, L>;

    fn next(&mut self) -> Option<Self::Item> {
        // Stop when we reach EOF.
        if self.remain.is_empty() {
            return None;
        }
        
        // Look for the first longest match.
        let mut token_type = TokenClass::Error;
        let mut length = 0;
        for (regex, candidate_token_type) in self.regexes.iter() {
            let candidate_length = regex.captures(self.remain).and_then(|x| x.get(0)).map(|x| x.as_str().len()).unwrap_or_default();
            if candidate_length > length {
                token_type = *candidate_token_type;
                length = candidate_length;
            }
        }
        
        // Advance by at least one character, even if nothing matched, in
        // an attempt to recover from errors.
        if length == 0 {
            length = 1;
        }

        // Generate the token and seek past it.
        let text = &self.remain[..length];
        self.remain = &self.remain[length..];
        let span = Span::for_text(&mut self.location, text);
        let token = Token::new(token_type, text, span);

        Some(token)
    }
}

//=============================================================================

// Error type used for both parsing and tokenization.
#[derive(Clone, Debug)]
pub enum Error<T: TokenType, L: Location = SingleFileLocation> {
    TokenizerFailed(TokenizerError<L>),
    ParserExpectedButFound(Vec<Option<GrammarInput<T, L>>>, Option<GrammarInput<T, L>>),
}

impl<T: TokenType, L: Location> chumsky::Error<GrammarInput<T, L>> for Error<T, L> {
    type Span = std::ops::Range<usize>;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<GrammarInput<T, L>>>>(
        _span: std::ops::Range<usize>,
        expected: Iter,
        found: Option<GrammarInput<T, L>>,
    ) -> Self {
        Self::ParserExpectedButFound(expected.into_iter().collect(), found)
    }

    fn with_label(mut self, label: Self::Label) -> Self { self }

    fn merge(mut self, mut other: Self) -> Self {
        if let (Self::ParserExpectedButFound(expected, _), Self::ParserExpectedButFound(expected_other, _)) = (
            &mut self,
            &mut other,
        ) {
            expected.append(expected_other);
        }
        self
    }
}

impl<T: TokenType, L: Location> std::fmt::Display for Error<T, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TokenizerFailed(x) => std::fmt::Display::fmt(x, f),
            Error::ParserExpectedButFound(expected, found) => {
                let formatter = |x: &Option<GrammarInput<T, L>>| {
                    x.as_ref().map(|x| x.to_string()).unwrap_or_else(|| String::from("end of file"))
                };
                write!(f, "expected ")?;
                match expected.len() {
                    0 => write!(f, "nothing")?,
                    1 => write!(f, "{}", formatter(&expected[0]))?,
                    2 => write!(f, "{} or {}", formatter(&expected[0]), formatter(&expected[1]))?,
                    _ => {
                        write!(f, "{}", formatter(&expected[0]))?;
                        for e in expected[1..expected.len() - 1].iter() {
                            write!(f, ", {}", formatter(e))?;
                        }
                        write!(f, ", or {}", formatter(&expected[expected.len() - 1]))?;
                    }
                }
                write!(f, " but found {}", formatter(found))
            },
        }
    }
}


/*

pub trait Nonterminal<L: types::Location>
where
    Self: Clone + std::fmt::Debug + Eq + std::hash::Hash
{
    fn parser() -> Box<dyn Parser<types::Terminal<TokenType, L>, Self, Error = types::Error<TokenType, L>>>;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AlterName<L: 'static + types::Location = types::SingleFileLocation>(types::Terminal<TokenType, L>, types::Terminal<TokenType, L>, types::Terminal<TokenType, L>);

impl<L: 'static + types::Location> AlterName<L> {
    fn make_parser() -> impl Parser<types::Terminal<TokenType, L>, Self, Error = types::Error<TokenType, L>> {
        just(types::Terminal::new_pattern(TokenType::MinGt))
            .then(just(types::Terminal::new_pattern(TokenType::Symbol)))
            .then(just(types::Terminal::new_pattern(TokenType::Symbol)))
            .map(|((x1, x2), x3)| AlterName(x1, x2, x3))
    }
}

impl<L: 'static + types::Location> Nonterminal<L> for AlterName<L> {
    fn parser() -> Box<dyn Parser<types::Terminal<TokenType, L>, Self, Error = types::Error<TokenType, L>>> {
        Box::new(Self::make_parser())
    }
}

*/