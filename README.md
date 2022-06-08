
# THIS IS VERY MUCH INCOMPLETE; WIP

Working title: GrammarSpec
==========================

GrammarSpec lets you specify grammars in such a way that they should be *both*
easy to read by a newcomer (without needing to first understand the many
intricacies of ANTLR or Yacc) *and* can be turned into a functional parser. It
aims to single-source-of-truth grammars of domain-specific (mini-)languages.

GrammarSpec can be used as:

 - a recursive-descent parser, capable of emitting the parse result in various
   formats (human-readable, JSON, binary protobuf, or in Rust form if you use
   GrammarSpec as a library);
 - a code generator, to convert its common EBNF dialect to the ANTLR or
   Flex/Yacc dialects or to a Rust/nom parser (by macro expansion).

As for the EBNF syntax used, https://xkcd.com/927/ unfortunately applies. It
tries to stick to https://www.w3.org/TR/REC-xml/#sec-notation as much as
possible, but there are some differences. The goal of this particular syntax
is to be as easy to *read*, both by a computer and by a human: cryptic
syntactic sugar intended only to make the language easier to write by a guru
is expressly avoided. All a reader should need to know is:

 - matching is greedy, then first come first serve;
 - the special `_` token can implicitly appear anywhere in a production rule.

The latter is unfortunately still quite cryptic, but the alternative for
practical grammars would be to manually insert optional whitespace symbols
literally everywhere. That not only destroys parser performance, but also makes
the grammar much more difficult to read and extremely annoying at best to
write.

To "prove" the readability point, we'll use our own grammar to describe the
rest [here](self-spec.ebnf). If you have worked with any BNF dialect before
and can't read it, GrammarSpec has already failed!

Note that while GrammarSpec can convert grammars to ANTLR4 and Flex/Yacc
syntax, it is up to the writer to ensure that the specification is written such
that the grammar actually works with those tools: ANTLR4 is LL(*) with some
extra logic tacked on for basic left-recursion, while Yacc is normally LALR.
Both furthermore first tokenize the input for performance, which requires the
grammar writer to think about which rules are tokens and which are grammar
production rules (distinguished using :== vs ::= respectively), and to ensure
that tokenization is not context-sensitive (an "else" within some specific
grammar rule cannot be distinguished from an identifier merely by context).
