
import re
from collections import namedtuple
import json

_Loc = namedtuple("_Loc", ["line", "col", "offs"])

class Loc(_Loc):
    def advance(self, s):
        line, col, offs = self
        for c in s:
            if c == "\n":
                line += 1
                col = 1
            else:
                col += 1
            offs += 1
        return Loc(line, col, offs)

    def __str__(self):
        return f"{self.line}:{self.col}"

_Span = namedtuple("_Span", ["a", "b"])

class Span(_Span):
    def __str__(self):
        return f"{self.a}..{self.b}"

    def __add__(self, other):
        a = min(self.a, other.a)
        b = max(self.b, other.b)
        return Span(a, b)

_Token = namedtuple("Token", ["name", "text", "span"])

class Token(_Token):
    def __str__(self):
        return f"{self.name} ({self.span})"

def tokenize(regexes, s):
    loc = Loc(1, 1, 0)
    failed = False
    while s:
        long_name = None
        long_length = 0
        long_match = None
        for name, regex in regexes:
            match = regex.match(s)
            if match:
                length = len(match.group(0))
                if length > long_length:
                    long_name = name
                    long_length = length
                    long_match = match
        long_length = max(1, long_length)

        loc_before = loc
        loc = loc.advance(s[:long_length])
        s = s[long_length:]
        span = Span(loc_before, loc)

        if not long_match:
            print(f"no token matched at {loc_before}, skipping...")
            failed = True
        elif long_name:
            yield Token(long_name, long_match.group(0), span)

    yield Token("EOF", "", Span(loc, loc))

    if failed:
        raise ValueError("tokenization errors occured")

class Node:
    def __init__(self, name, data, span):
        super().__init__()
        self.name = name
        self.data = data
        self.span = span

    @classmethod
    def _flatten(cls, nodes):
        for node in nodes:
            if node.name == "FLAT":
                yield from cls._flatten(node.data)
            else:
                yield node

    @classmethod
    def terminal(cls, token):
        return cls(token.name, token.text, token.span)

    @classmethod
    def nonterminal(cls, name, children):
        children = tuple(cls._flatten(children))
        span = None
        for child in children:
            if span is None:
                span = child.span
            else:
                span = span + child.span
        return cls(name, children, span)

    @classmethod
    def flattened(cls, children):
        return cls.nonterminal("FLAT", children)

    @classmethod
    def nothing(cls):
        return cls.flattened([])

    @classmethod
    def error(cls, msg, span):
        return cls("ERR", msg, span)

    def is_error(self):
        return self.name == "ERR"

    def is_terminal(self):
        return isinstance(self.data, str)

    def to_json(self):
        if self.is_terminal():
            data = self.data
        else:
            data = [child.to_json() for child in self.data]
        return {f"{self.name} at {self.span}": data}

    def filter(self, name):
        for child in self.data:
            if child.name == name or child.name.startswith(f"{name}."):
                yield child

    def first(self, name):
        try:
            return next(iter(self.filter(name)))
        except StopIteration:
            return None

    def nth(self, name, n):
        try:
            it = iter(self.filter(name))
            for _ in range(n):
                next(it)
            return next(it)
        except StopIteration:
            return None

    def __eq__(self, other):
        return type(self) == type(other) and self.name == other.name and self.data == other.data and self.span == other.span

    def __hash__(self):
        return hash((type(self), self.name, self.data, self.span))

class Debugger:
    _depth = 0

    def __init__(self, name):
        super().__init__()
        self.name = name
        self.failed = False

    def __enter__(self):
        type(self).print(f"-> {self.name}:")
        type(self)._depth += 1
        return self

    def set_failed(self):
        self.failed = True

    def __exit__(self, exc_type, *_):
        type(self)._depth -= 1
        if exc_type is not None:
            type(self).print(f"## {self.name}")
        elif self.failed:
            type(self).print(f"!! {self.name}")
        else:
            type(self).print(f"<- {self.name}")

    @classmethod
    def print(cls, msg):
        print(f"{'   '*cls._depth}{msg}")


# parse(tokens) -> (node, remaining tokens)
# if node.is_error(), remaining tokens must equal incoming tokens

class ParserFactory:

    @classmethod
    def term(cls, name):
        def out_parser(tokens):
            if tokens[0].name == name:
                return Node.terminal(tokens[0]), tokens[1:]
            return Node.error(f"expected {name}, found {tokens[0].name}", tokens[0].span), tokens
        return cls.debugged(f"term({name})", out_parser)

    @classmethod
    def maybe(cls, parser):
        def out_parser(tokens):
            node, tokens = parser(tokens)
            if node.is_error():
                node = Node.nothing()
            return node, tokens
        return cls.debugged("maybe", out_parser)

    @classmethod
    def any(cls, parser):
        def out_parser(tokens):
            nodes = []
            while True:
                node, tokens = parser(tokens)
                if node.is_error():
                    return Node.flattened(nodes), tokens
                nodes.append(node)
        return cls.debugged("any", out_parser)

    @classmethod
    def many(cls, parser):
        return cls.debugged("many", cls.concat(parser, cls.any(parser)))

    @classmethod
    def concat(cls, *parsers):
        def out_parser(tokens):
            initial_tokens = tokens
            nodes = []
            for parser in parsers:
                node, tokens = parser(tokens)
                if node.is_error():
                    return node, initial_tokens
                nodes.append(node)
            return Node.flattened(nodes), tokens
        return cls.debugged("concat", out_parser)

    @classmethod
    def alter(cls, *parsers):
        def out_parser(tokens):
            for parser in parsers:
                node, new_tokens = parser(tokens)
                if not node.is_error():
                    return node, new_tokens
            return node, tokens
        return cls.debugged("alter", out_parser)

    @classmethod
    def named(cls, name, parser):
        def out_parser(tokens):
            node, tokens = parser(tokens)
            if node.is_error():
                return node, tokens
            return Node.nonterminal(name, [node]), tokens
        return cls.debugged(f"named({name})", out_parser)

    @staticmethod
    def debugged(msg, parser):
        return parser
        def out_parser(tokens):
            with Debugger(f"{msg} at {tokens[0].span.a}") as dbg:
                node, tokens = parser(tokens)
                if node.is_error():
                    dbg.set_failed()
                return node, tokens
        return out_parser

class Parser:
    def __init__(self, name):
        super().__init__()
        self.name = name
        self.options = []
        self.parser = None

    def define_opt(self, name, *parsers):
        if self.parser is not None:
            raise ValueError("can't add definitions after use")
        if len(parsers) == 1:
            parser = parsers[0]
        else:
            parser = ParserFactory().concat(*parsers)
        self.options.append((name, parser))

    def define_one(self, *parsers):
        self.define_opt("", *parsers)

    def __call__(self, tokens):
        if self.parser is None:
            if not self.options:
                raise ValueError(f"no definitions for rule {self.name}")
            pf = ParserFactory()
            if len(self.options) == 1:
                self.parser = pf.named(self.name, self.options[0][1])
            else:
                self.parser = pf.alter(*(pf.named(f"{self.name}.{name}", parser) for name, parser in self.options))
        return self.parser(tokens)

    def parse(self, tokens):
        node, tokens = self(tokens)
        if node.is_error():
            raise ValueError(f"parse error: {node.data} at {node.span}")
        eof_node, tokens = ParserFactory().term("EOF")(tokens)
        if eof_node.is_error():
            for token in tokens:
                print(token)
            raise ValueError(f"parse error: {eof_node.data} at {eof_node.span}")
        return node

class AstGrammar:
    def _clean_doc(self, doc):
        if doc is None:
            return ""
        assert doc.name == "Doc"
        doc = doc.data
        assert doc.startswith("/**")
        assert doc.endswith("*/")
        def strip_line(s):
            s = s.strip()
            if s.startswith("*"):
                s = s[1:]
                if s.startswith(" "):
                    s = s[1:]
            return s
        return "\n".join(map(strip_line, doc[3:-2].strip().split("\n")))

    def _derive_name(self, node):
        if node.is_terminal():
            return ""
        symbol = node.first("Symbol")
        if symbol is not None:
            return symbol.data
        string_literal = node.first("StringLiteral")
        if string_literal is not None:
            return AstLiteral.from_parse_tree(string_literal).as_symbol()
        character_literal = node.first("CharacterLiteral")
        if character_literal is not None:
            return AstLiteral.from_parse_tree(character_literal).as_symbol()
        if not node.data:
            return ""
        for child in node.data:
            name = self._derive_name(child)
            if name:
                return name
        return ""

    def __init__(self, node, case_convert=lambda x: x):
        super().__init__()
        assert node.name == "Grammar"

        # Convert from parse tree to AST for all the rules.
        grammar_rules = [] # [(name, doc, alter_name, alter_doc, rule)]
        token_rules = [] # [(name, doc, rule)]
        used_rules = set("_") # names of rules reachable from any grammar rule
        for rule in node.data:
            doc = self._clean_doc(rule.first("Doc"))
            name = rule.first("Symbol")
            if name is not None:
                name = case_convert(name.data)
            alters = [rule.first("Alternation"), rule.first("FirstAlter")] + list(rule.filter("SubseqAlter"))
            for alter in alters:
                if alter is None:
                    continue
                alter_doc = self._clean_doc(alter.first("Doc"))
                alter_name = alter.first("AlterName")
                if alter_name is not None:
                    alter_name = case_convert(alter_name.first("Symbol").data)
                for sub_alter in alter.filter("Concatenation"):
                    if alter_name is not None:
                        sub_alter_name = alter_name
                    else:
                        sub_alter_name = case_convert(self._derive_name(sub_alter))
                    concat = AstConcatenation.from_parse_tree(sub_alter, case_convert)
                    if rule.name == "Rule.GrammarRule":
                        token_rules.extend(concat.gather_implicit_tokens(case_convert))
                        for used_name in concat.gather_used_tokens(case_convert):
                            used_rules.add(used_name)
                        grammar_rules.append((name, doc, sub_alter_name, alter_doc, concat))
                    elif rule.name == "Rule.TokenRule":
                        token_rules.append((name, doc, concat))
                    elif rule.name == "Rule.WhitespaceRule":
                        token_rules.append(("_", "Whitespace.", concat))
                    else:
                        assert False

        # Combine multiple definitions of grammar- and token rules as
        # alternatives. Note that token_rule_map contains fragments, but
        # token_order does not.
        self.first_rule = grammar_rules[0][0]
        self.grammar_rules = {} # {name: (doc, [(alter_name, alter_doc, rule)])}
        self.token_order = [] # [name]
        self.token_rules = {} # {name: (doc, rule)}
        self.fragment_rules = {} # {name: (doc, rule)}
        token_rule_map = {} # {name: (doc, [rule])}
        token_frag_lookup = {} # {name: rule}
        for name, doc, alter_name, alter_doc, rule in grammar_rules:
            if name in self.grammar_rules:
                self.grammar_rules[name][1].append((alter_name, alter_doc, rule))
            else:
                self.grammar_rules[name] = (doc, [(alter_name, alter_doc, rule)])
        for name, doc, rule in token_rules:
            if name in token_rule_map:
                if rule not in token_rule_map[name][1]:
                    token_rule_map[name][1].append(rule)
            else:
                if name != "_" and name in used_rules:
                    self.token_order.append(name)
                token_rule_map[name] = (doc, [rule])
        if "_" in token_rule_map:
            self.token_order.append("_")
        for name, (doc, rules) in token_rule_map.items():
            if len(rules) == 1:
                rule = rules[0]
            else:
                rule = AstAlternation(rules)
            if name in used_rules:
                self.token_rules[name] = (doc, rule)
            else:
                self.fragment_rules[name] = (doc, rule)
            token_frag_lookup[name] = rule

        # Assert that all rules used in the grammar actually exist and are
        # defined as either a grammar rule or a token rule (but not both).
        for used_rule in used_rules:
            is_grammar = used_rule in self.grammar_rules
            is_token = used_rule in token_rule_map
            if is_token and is_grammar:
                raise ValueError(f"{used_rule} is defined as both a grammar and token rule")
            if not is_token and not is_grammar:
                raise ValueError(f"{used_rule} is not defined")

        # Build regular expressions for the token rules.
        self.token_regexes = {} # {name: regex}
        for name, (_, rule) in self.token_rules.items():
            regex = rule.as_regex(token_frag_lookup, [name])
            self.token_regexes[name] = re.compile(regex)

        # Build ad-hoc recursive-descent parsers for the grammar rules.
        self.grammar_parsers = {name: Parser(name) for name in self.grammar_rules}
        for name, (_, alters) in self.grammar_rules.items():
            parser = self.grammar_parsers[name]
            if len(alters) == 1:
                parser.define_one(alters[0][2].as_parser(self.grammar_parsers, case_convert))
            else:
                for name, _, rule in alters:
                    parser.define_opt(name, rule.as_parser(self.grammar_parsers, case_convert))

    def tokenize(self, s):
        rules = [(token if token != "_" else None, self.token_regexes[token]) for token in self.token_order]
        return tokenize(rules, s)

    def parse(self, tokens, name=None):
        if name is None:
            name = self.first_rule
        return self.grammar_parsers[name].parse(tokens)

    def tokenize_and_parse(self, s, name=None):
        return self.parse(name, list(self.tokenize(s)))



class AstAlternation:
    def __init__(self, elements):
        super().__init__()
        self.elements = elements

    @classmethod
    def from_parse_tree(cls, node, case_convert):
        assert node.name == "Alternation"
        elements = [AstConcatenation.from_parse_tree(x, case_convert) for x in node.filter("Concatenation")]
        if len(elements) == 1:
            return elements[0]
        return cls(elements)

    def gather_implicit_tokens(self, case_convert):
        for element in self.elements:
            yield from element.gather_implicit_tokens(case_convert)

    def gather_used_tokens(self, case_convert):
        for element in self.elements:
            yield from element.gather_used_tokens(case_convert)

    def as_regex(self, rule_lookup, recursion):
        return "|".join((f"(?:{element.as_regex(rule_lookup, recursion)})" for element in self.elements))

    def as_parser(self, parser_lookup, case_convert):
        return ParserFactory().alter(*(element.as_parser(parser_lookup, case_convert) for element in self.elements))

    def __eq__(self, other):
        return type(self) == type(other) and self.elements == other.elements

    def __hash__(self):
        return hash((type(self), self.elements))


class AstConcatenation:
    def __init__(self, elements):
        super().__init__()
        self.elements = elements

    @classmethod
    def from_parse_tree(cls, node, case_convert):
        assert node.name == "Concatenation"
        elements = [AstRepetition.from_parse_tree(x, case_convert) for x in node.filter("Repetition")]
        if len(elements) == 1:
            return elements[0]
        return cls(elements)

    def gather_implicit_tokens(self, case_convert):
        for element in self.elements:
            yield from element.gather_implicit_tokens(case_convert)

    def gather_used_tokens(self, case_convert):
        for element in self.elements:
            yield from element.gather_used_tokens(case_convert)

    def as_regex(self, rule_lookup, recursion):
        return "".join((f"(?:{element.as_regex(rule_lookup, recursion)})" for element in self.elements))

    def as_parser(self, parser_lookup, case_convert):
        return ParserFactory().concat(*(element.as_parser(parser_lookup, case_convert) for element in self.elements))

    def __eq__(self, other):
        return type(self) == type(other) and self.elements == other.elements

    def __hash__(self):
        return hash((type(self), self.elements))


class AstRepetition:
    def __init__(self, child, mode):
        super().__init__()
        assert mode in "?*+"
        self.child = child
        self.mode = mode

    @classmethod
    def from_parse_tree(cls, node, case_convert):
        if node.name == "Repetition.One":
            mode = ""
        elif node.name == "Repetition.Maybe":
            mode = "?"
        elif node.name == "Repetition.Any":
            mode = "*"
        elif node.name == "Repetition.Many":
            mode = "+"
        else:
            assert False

        node = node.first("Singular")
        if node.name == "Singular.Nested":
            child = AstAlternation.from_parse_tree(node.first("Alternation"), case_convert)
        elif node.name == "Singular.Symbol":
            child = AstReference.from_parse_tree(node.first("Symbol"), case_convert)
        elif node.name in "Singular.StringLiteral":
            child = AstLiteral.from_parse_tree(node.first("StringLiteral"), case_convert)
        elif node.name == "Singular.CharacterSet":
            child = AstCharacterSet.from_parse_tree(node.first("CharacterSet"), case_convert)
        elif node.name in "Singular.CharacterLiteral":
            child = AstLiteral.from_parse_tree(node.first("CharacterLiteral"), case_convert)
        else:
            assert False

        if not mode:
            return child
        return cls(child, mode)

    def gather_implicit_tokens(self, case_convert):
        yield from self.child.gather_implicit_tokens(case_convert)

    def gather_used_tokens(self, case_convert):
        yield from self.child.gather_used_tokens(case_convert)

    def as_regex(self, rule_lookup, recursion):
        return f"(?:{self.child.as_regex(rule_lookup, recursion)}){self.mode}"

    def as_parser(self, parser_lookup, case_convert):
        if self.mode == "?":
            return ParserFactory().maybe(self.child.as_parser(parser_lookup, case_convert))
        elif self.mode == "*":
            return ParserFactory().any(self.child.as_parser(parser_lookup, case_convert))
        elif self.mode == "+":
            return ParserFactory().many(self.child.as_parser(parser_lookup, case_convert))
        assert False

    def __eq__(self, other):
        return type(self) == type(other) and self.child == other.child and self.mode == other.mode

    def __hash__(self):
        return hash((type(self), self.child, self.other))


class AstLiteral:

    def __init__(self, s):
        super().__init__()
        self.s = s

    @staticmethod
    def _slugify(s):
        elements = []
        prev_is_literal = False
        for c in s:
            cp = ord(c)
            names = ["nul", "soh", "stx", "etx", "eot", "enq", "ack", "bel", "bs", "ht", "lf", "vt", "ff", "cr", "so", "si", "dle", "dc1", "dc2", "dc3", "dc4", "nak", "syn", "etb", "can", "em", "sub", "esc", "fs", "gs", "rs", "us", "sp", "ex", "dqt", "hsh", "dlr", "prc", "amp", "sqt", "lp", "rp", "ast", "pls", "com", "min", "prd", "sl", "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "col", "sem", "lt", "eq", "gt", "qst", "at", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "lsq", "bsl", "rsq", "crt", "usc", "bqt", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "lcr", "bar", "rcr", "tld", "del"]
            if cp < len(names):
                name = names[cp]
            else:
                name = f"u{cp:x}"
            is_literal = len(name) == 1
            if not elements or not is_literal or not prev_is_literal:
                elements.append(name)
            elif prev_is_literal:
                elements[-1] += name
            prev_is_literal = is_literal
        return "-".join(elements)

    @staticmethod
    def scan_char(s):
        if not s.startswith("#"):
            return s[:1], s[1:]
        c = s[1:2]
        s = s[2:]
        if c == "x":
            return chr(int(s[:2], 16)), s[2:]
        elif c == "u":
            return chr(int(s[:4], 16)), s[4:]
        elif c == "U":
            return chr(int(s[:8], 16)), s[8:]
        elif c == "t":
            return "\t", s
        elif c == "n":
            return "\n", s
        elif c == "r":
            return "\r", s
        return c, s

    @classmethod
    def _clean_char(cls, s):
        c, s = cls.scan_char(s)
        assert not s
        return c

    @classmethod
    def _clean_string(cls, s):
        assert s[0] in "'\""
        assert s[-1] in "'\""
        s = s[1:-1]
        cleaned = ""
        while s:
            c, s = cls.scan_char(s)
            cleaned += c
        return cleaned

    @classmethod
    def from_parse_tree(cls, node, case_convert=None):
        if node.name == "StringLiteral":
            return cls(cls._clean_string(node.data))
        elif node.name == "CharacterLiteral":
            return cls(cls._clean_char(node.data))
        else:
            assert False

    def as_symbol(self):
        return self._slugify(self.s)

    def gather_implicit_tokens(self, case_convert):
        yield (case_convert(self.as_symbol()), "Implicit token.", self)

    def gather_used_tokens(self, case_convert):
        yield case_convert(self.as_symbol())

    @staticmethod
    def escape_regex(s):
        escaped = ""
        for c in s:
            if c in ".+*?^$()[]{}|\\-":
                escaped += f"\\{c}"
            else:
                escaped += c
        return escaped

    def as_regex(self, rule_lookup, recursion):
        return self.escape_regex(self.s)

    def as_parser(self, parser_lookup, case_convert):
        return ParserFactory().term(case_convert(self.as_symbol()))

    def __eq__(self, other):
        return type(self) == type(other) and self.s == other.s

    def __hash__(self):
        return hash((type(self), self.s))


class AstReference:

    def __init__(self, s):
        super().__init__()
        self.s = s

    @classmethod
    def from_parse_tree(cls, node, case_convert):
        assert node.name == "Symbol"
        return cls(case_convert(node.data))

    def gather_implicit_tokens(self, case_convert):
        return []

    def gather_used_tokens(self, case_convert):
        yield self.s

    def as_regex(self, rule_lookup, recursion):
        if self.s in recursion:
            raise ValueError(f"recursive token rule: {recursion}")
        recursion = recursion + [self.s]
        rule = rule_lookup.get(self.s, None)
        if rule is None:
            raise ValueError(f"unknown token rule: {self.s}")
        return f"(?:{rule.as_regex(rule_lookup, recursion)})"

    def as_parser(self, parser_lookup, case_convert):
        if self.s in parser_lookup:
            return parser_lookup[self.s]
        else:
            return ParserFactory().term(self.s)

    def __eq__(self, other):
        return type(self) == type(other) and self.s == other.s

    def __hash__(self):
        return hash((type(self), self.s))


class AstCharacterSet:

    def __init__(self, inverted, ranges):
        super().__init__()
        self.inverted = inverted
        self.ranges = ranges # [(from, to)]

    @classmethod
    def from_parse_tree(cls, node, case_convert):
        assert node.name == "CharacterSet"
        assert node.data.startswith("[")
        assert node.data.endswith("]")
        s = node.data[1:-1]
        if s.startswith("^"):
            inverted = True
            s = s[1:]
        else:
            inverted = False
        ranges = []
        while s:
            a, s = AstLiteral.scan_char(s)
            if s.startswith("-"):
                b, s = AstLiteral.scan_char(s[1:])
            else:
                b = a
            if ord(a) > ord(b):
                r = f"{a}-{b}"
                raise ValueError(f"invalid character range {r!r}")
            ranges.append((a, b))
        return cls(inverted, ranges)

    def gather_implicit_tokens(self, case_convert):
        raise ValueError("character sets are only allowed within the context of token rules")

    def gather_used_tokens(self, case_convert):
        raise ValueError("character sets are only allowed within the context of token rules")

    def as_regex(self, rule_lookup, recursion):
        body = ""
        for a, b in self.ranges:
            if a == b:
                body += AstLiteral.escape_regex(a)
            else:
                body += AstLiteral.escape_regex(a) + "-" + AstLiteral.escape_regex(b)
        if self.inverted:
            if body:
                return f"[^{body}]"
            else:
                return "."
        return f"[{body}]"

    def as_parser(self, parser_lookup, case_convert):
        raise ValueError("character sets are only allowed within the context of token rules")

    def __eq__(self, other):
        return type(self) == type(other) and self.s == other.s

    def __hash__(self):
        return hash((type(self), self.s))


if __name__ == "__main__":

    # Manually coded copy of the EBNF grammar.
    regexes = [
        ("ColEqEq", re.compile(r":==")),
        ("Sem", re.compile(r";")),
        ("Usc", re.compile(r"_")),
        ("Symbol", re.compile(r"[a-z][a-z0-9\-]*")),
        ("Bar", re.compile(r"\|")),
        ("ColColEq", re.compile(r"::=")),
        ("MinGt", re.compile(r"->")),
        ("Qst", re.compile(r"\?")),
        ("Ast", re.compile(r"\*")),
        ("Pls", re.compile(r"\+")),
        ("Lp", re.compile(r"\(")),
        ("Rp", re.compile(r"\)")),
        ("StringLiteral", re.compile(r"('([^'#]|#.)+'|\"([^\"#]|#.)+\")")),
        ("CharacterSet", re.compile(r"(\[\^?(([^\^\-\]#]|#([^xuU]|x[0-9a-fA-F][0-9a-fA-F]|u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]|U[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]))(-([^\^\-\]#]|#([^xuU]|x[0-9a-fA-F][0-9a-fA-F]|u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]|U[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])))?)*\])")),
        ("CharacterLiteral", re.compile(r"(#([^xuU]|x[0-9a-fA-F][0-9a-fA-F]|u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]|U[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]))")),
        ("Doc", re.compile(r"(/\*\*([^\*]|\*+[^\*/])*\**\*/)")),
        (None, re.compile(r"(/\*([^\*]|\*+[^\*/])*\**\*/|[ \t\n\r]+)")),
    ]

    r_grammar = Parser("Grammar")
    r_rule = Parser("Rule")
    r_alternation = Parser("Alternation")
    r_first_alter = Parser("FirstAlter")
    r_subseq_alter = Parser("SubseqAlter")
    r_alter_name = Parser("AlterName")
    r_concatenation = Parser("Concatenation")
    r_repetition = Parser("Repetition")
    r_singular = Parser("Singular")

    pf = ParserFactory()

    r_grammar.define_one(pf.many(r_rule))
    r_rule.define_opt(
        "TokenRule",
        pf.maybe(pf.term("Doc")),
        pf.term("Symbol"),
        pf.term("ColEqEq"),
        r_alternation,
        pf.term("Sem"),
    )
    r_rule.define_opt(
        "WhitespaceRule",
        pf.maybe(pf.term("Doc")),
        pf.term("Usc"),
        pf.term("ColEqEq"),
        r_alternation,
        pf.term("Sem"),
    )
    r_rule.define_opt(
        "GrammarRule",
        pf.maybe(pf.term("Doc")),
        pf.term("Symbol"),
        r_first_alter,
        pf.any(r_subseq_alter),
        pf.term("Sem"),
    )
    r_alternation.define_one(
        r_concatenation,
        pf.any(pf.concat(pf.term("Bar"), r_concatenation))
    )
    r_first_alter.define_one(
        pf.maybe(pf.term("Doc")),
        pf.term("ColColEq"),
        r_concatenation,
        pf.maybe(r_alter_name),
    )
    r_subseq_alter.define_one(
        pf.maybe(pf.term("Doc")),
        pf.term("Bar"),
        r_concatenation,
        pf.maybe(r_alter_name),
    )
    r_alter_name.define_one(pf.term("MinGt"), pf.term("Symbol"))
    r_concatenation.define_one(pf.many(r_repetition))
    r_repetition.define_opt("Maybe", r_singular, pf.term("Qst"))
    r_repetition.define_opt("Any", r_singular, pf.term("Ast"))
    r_repetition.define_opt("Many", r_singular, pf.term("Pls"))
    r_repetition.define_opt("One", r_singular)
    r_singular.define_opt("Nested", pf.term("Lp"), r_alternation, pf.term("Rp"))
    r_singular.define_opt("Symbol", pf.term("Symbol"))
    r_singular.define_opt("StringLiteral", pf.term("StringLiteral"))
    r_singular.define_opt("CharacterSet", pf.term("CharacterSet"))
    r_singular.define_opt("CharacterLiteral", pf.term("CharacterLiteral"))

    # Parse the self-specification.
    with open("self-spec.ebnf", "r", encoding="utf-8") as fil:
        tokens = list(tokenize(regexes, fil.read()))
    parse_tree = r_grammar.parse(tokens)

    # Analyze the self-specification.
    def case_convert(x):
        #return tuple(x.split("-"))
        return "".join(map(str.title, x.split("-")))
    grammar = AstGrammar(parse_tree, case_convert)

    # Parse the self-specification with the grammar we just constructed from
    # it.
    with open("self-spec.ebnf", "r", encoding="utf-8") as fil:
        tokens2 = list(grammar.tokenize(fil.read()))
    parse_tree2 = grammar.parse(tokens2)

    # Assert that both result in exactly the same thing.
    assert tokens == tokens2
    assert parse_tree == parse_tree2
