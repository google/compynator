# Copyright 2019 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import operator

from .core import Parser, Result, One, Succeed, Empty, Failure, Fail, Terminal

# Some regex char classes.
#: Exactly one decimal digit.
Digit = One.where(lambda c: '0' <= c <= '9')
#: Exactly one hexadecimal digit.
HexDigit = Digit | One.where(lambda c: 'a' <= c <= 'f' or 'A' <= c <= 'F')
#: Exactly one octadecimal digit.
OctDigit = One.where(lambda c: '0' <= c <= '7')
#: Exactly one letter a-z.
Lower = One.where(lambda c: 'a' <= c <= 'z')
#: Exactly one letter A-Z
Upper = One.where(lambda c: 'A' <= c <= 'Z')
#: Exactly one letter a-zA-Z
Alpha = Lower | Upper
#: Exactly one ASCII letter or digit.
Alnum = Alpha | Digit


class Regex(Parser):
    """Regex matcher."""
    def __init__(self, regex):
        self.__regex = regex

    def parse_with_context(self, tokens, context):
        m = self.__regex.match(tokens)
        if m:
            v = m.group(0)
            r = tokens[len(v):]
            return Succeed(v)(r)
        return Failure(self, f'Expecting a match of {self.__regex!r}.')


class ITerminal(Parser):
    """Case insensitive terminal."""
    def __init__(self, terminal):
        self.__terminal = terminal

    def parse_with_context(self, tokens, context):
        if len(tokens) < len(self.__terminal):
            return Failure(self,
                    f'Input sequence is shorter than {self.__terminal!r}.',
                    tokens)
        head = tokens[:len(self.__terminal)]
        if head.casefold() != self.__terminal.casefold():
            return Failure(self,
                    f'Expecting case insensitive terminal {self.__terminal!r}.',
                    tokens)
        return Succeed(head)(tokens[len(head):])


class Forward(Parser):
    """A forward declaration of a rule.

    This is useful in case the rule is defined recursively. For example, the BNF
    rule ``exp ::= (exp '-' exp) | 'o'`` could be defined as followed::

        >>> exp = Forward()
        >>> exp.is_(((exp + '-' + exp) ^ 'o').memoize())
        >>> set(exp('o'))
        {Result(value='o', remain='')}
        >>> sorted(exp('o-o'))
        [Result(value='o', remain='-o'), Result(value='o-o', remain='')]

    A forward declaration of Parser is the same as referring to that parser in
    a lambda::

        >>> exp = (Succeed(None).then(lambda _: exp + '-' + exp) ^ 'o').memoize()
        >>> set(exp('o'))
        {Result(value='o', remain='')}
        >>> sorted(exp('o-o'))
        [Result(value='o', remain='-o'), Result(value='o-o', remain='')]

    A ``RuntimeError`` will be raised if a ``Forward`` has not called ``is_``,
    or if that method is called more than once.

        >>> exp = Forward()
        >>> exp('abc')
        Traceback (most recent call last):
          File "<stdin>", line 1, in ?
        RuntimeError: A forward declaration has no definition.
        >>> exp.is_('abc')
        >>> exp.is_('abc')
        Traceback (most recent call last):
          File "<stdin>", line 1, in ?
        RuntimeError: Already defined.
    """

    def __init__(self):
        self.__parser = None

    def is_(self, parser):
        """Defines a forward declaration.

        If ``parser`` is not typed ``Parser``, its string representation will
        be made into a ``Terminal``.

        This method must be called exactly once for each ``Forward`` object. A
        ``RuntimeError`` will be raised if it is called more than once.
        """
        if self.__parser:
            raise RuntimeError('Already defined.')
        self.__parser = self._to_parser(parser)

    def parse_with_context(self, tokens, context):
        if not self.__parser:
            raise RuntimeError('A forward declaration has no definition.')
        return self.__parser.parse_with_context(tokens, context)


class Collect(Parser):
    """A combinator that runs through all ``parsers`` in sequence and collects
    their results in a collection of many flattened collections.

    This is best described with examples:

        >>> a, b, c = [Terminal(x) for x in 'abc']
        >>> p = Collect(a, b, c)
        >>> set(p('adc'))
        set()
        >>> p('adc')
        Failure('Failed to collect.', 'adc', [Failure("Parser index 1: Expecting terminal 'b'.", 'dc', ())])
        >>> set(p('abc'))
        {Result(value=('a', 'b', 'c'), remain='')}
        >>> a = a.repeat(0, 2, take_all=True)
        >>> p = Collect(a, a, a)
        >>> rs = p('a')
        >>> len(rs)
        4
        >>> Result(value=('', '', ''), remain='a') in rs
        True
        >>> Result(value=('', '', 'a'), remain='') in rs
        True
        >>> Result(value=('', 'a', ''), remain='') in rs
        True
        >>> Result(value=('a', '', ''), remain='') in rs
        True
        >>> len(p('aa'))  # -/-/-, -/-/a, -/a/-, a/-/-, \
        ...               # -/-/aa, -/a/a, -/aa/-, a/-/a, a/a/-, aa/-/-
        10

    Note that the final ``ResultSet`` could grow exponentially.
    """
    def __init__(self, *parsers):
        assert parsers
        self.__parsers = parsers

    def parse_with_context(self, tokens, context):
        failures = []
        ret = Fail(tokens)
        flattened = [None] * len(self.__parsers)
        def helper(level, remain):
            nonlocal ret
            if level >= len(self.__parsers):
                ret = ret.add(Result(value=tuple(flattened), remain=remain))
                return
            rs = self.__parsers[level].parse_with_context(remain, context)
            if not rs:
                if not ret:
                    rs.message = f'Parser index {level}: {rs.message}'
                    failures.append(rs)
                return
            for v, r in rs:
                flattened[level] = v
                helper(level + 1, r)
        helper(0, tokens)
        if not ret:
            ret = Failure(self, 'Failed to collect.', tokens, failures)
        return ret


class Lookahead(Parser):
    """Tries ``parser`` but does not consume input.

    If the truth value of the parse result is ``take_if``, a ``Success`` of
    ``value`` is returned. Otherwise, a ``Failure`` is returned.
    """
    def __init__(self, parser, take_if=True, value=''):
        self.__parser = parser
        self.__take_if = take_if
        self.__value = value

    def parse_with_context(self, tokens, context):
        rs = self.__parser.parse_with_context(tokens, context)
        if bool(rs) == self.__take_if:
            return Succeed(self.__value)(tokens)
        else:
            return Failure(self, 'take_if {self.__take_if}', rs)
