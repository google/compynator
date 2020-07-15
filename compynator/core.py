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

"""The essentials of all parser combinators.

The six basic regex definitions are mapped according to:

=============  ================
    Regex         Compynator
=============  ================
empty set      ``Fail``
epsilon        ``Empty``
character      ``Terminal``
-------------  ----------------
concatenation  ``+``
alternative    ``|`` or ``^``
Kleene star    ``repeat``
=============  ================

Monadic properties are ``Succeed`` for unit, ``Parser.then`` for bind, and
optionally ``Fail`` for zero.
"""

import collections
import collections.abc
import functools
import operator
import sys


class Result(collections.namedtuple('Result', 'value remain')):
    """Holds the parsed results.

    Each result is a 2-tuple of value and remaining unparsed sequence of tokens.

    **NOTE:** The input tokens are assumed to be immutable and ``len(remain)``
    is sufficient to tell if two ``Result.remain``s are equal.
    """

    def __hash__(self):
        # Instead of calculating hash of the ``remain`` sequence,
        # we just hash the length of that sequence.
        return hash((self.value, len(self.remain)))


class ResultSet(collections.abc.Iterable, collections.abc.Sized):
    """A sized iterable collection of ``Result``.

    To incrementally construct a result set, first start with a ``Failure``,
    then add more ``Result`` via ``add`` or ``add_all``.
    """
    def add(self, result):
        """Returns a ``ResultSet`` (could be ``self``) with ``result``."""
        raise NotImplementedError()

    def add_all(self, results):
        """Returns a ``ResultSet`` (could be ``self``) with all ``results``."""
        raise NotImplementedError()


class Success(ResultSet):
    """A collection of ``Result`` in a successful parse.

    A ``Success`` must have at least one ``Result``. The constructor can take
    either keyword argument ``result`` or ``results``, but not both at the same
    time.
    """
    def __init__(self, *args, result=None, results=None):
        assert (result is not None) ^ (results is not None)
        if result:
            self.__results = [result]
        else:
            self.__results = list(results)

    def add(self, result):
        if result not in self.__results:
            self.__results.append(result)
        return self

    def add_all(self, results):
        for result in results:
            self.add(result)
        return self

    def __iter__(self):
        return iter(self.__results)

    def __len__(self):
        return len(self.__results)

    def __eq__(self, other):
        if not isinstance(other, Success):
            return False
        return self.__results == other.__results

    def __str__(self):
        return str(self.__results)


class Failure(ResultSet):
    """A collection of zero ``Result``s.

    This class is used to propagate parse failures and incrementally construct
    a ``Success``. We usually start out with an instance of this class, then add
    more ``Result`` objects to it to produce a ``Success``.

        >>> parser = 'a' + Terminal('b') + 'c'
        >>> ret = Failure(parser, 'Parser fails.', None)
        >>> s = ret.add_all(parser('abc'))
        >>> isinstance(s, Success)
        True
    """
    def __init__(self, parser, message, remain='', cause_or_causes=None):
        self.__parser = parser
        self.message = message
        self.__remain = remain
        if cause_or_causes is None:
            self.__causes = tuple()
        elif isinstance(cause_or_causes, Failure):
            self.__causes = (cause_or_causes,)
        else:
            self.__causes = cause_or_causes

    def add(self, result):
        return Success(result=result)

    def add_all(self, results):
        ret = Success(results=results) or self
        return ret

    def __iter__(self):
        return self

    def __next__(self):
        raise StopIteration()

    def __len__(self):
        return 0

    def __get_causes(self, include_unnamed=False):
        for cause in self.__causes:
            if include_unnamed:
                yield cause
            elif not cause.__causes:
                # The root cause since there's no further failures.
                yield cause
            elif self.__parser.is_named():
                # Errors from named parser are always reported.
                yield cause
            else:
                yield from cause.__get_causes(include_unnamed)

    def describe(self, indent=4, level=0, include_unnamed=False):
        lead = ' ' * indent * level
        desc = (
            f'{lead}{self.__parser} at -{len(self.__remain)}: '
            f'{self.message}'
        )
        tail = ''
        if self.__causes:
            tail = (
                f'\n{lead}Caused by:\n' +
                '\n'.join(x.describe(indent, level + 1)
                          for x in self.__get_causes(include_unnamed))
            )
        return desc + tail

    def __str__(self):
        return self.describe()

    def __repr__(self):
        return (
            f'{self.__class__.__name__}({self.message!r}, '
            f'{self.__remain!r}, {self.__causes!r})'
        )


#: Options to configure parsing operations.
#
# max_recursion:
#     Maximum recursion depth used in memoization. ``None`` will automatically
#     pick out an appropriate value, depending on the input sequence.
ParseOptions = collections.namedtuple('ParseOptions', ['max_recursion'])


class ParseContext:
    """Internal book-keeping data structure."""
    def __init__(self, options=None):
        self._memotable = {}
        if options is None:
            options = ParseOptions(max_recursion=None)
        self.options = options


def default_parse_context(tokens):
    """Returns ``ParseContext`` for ``tokens``."""
    return ParseContext()


class Parser:
    """Callable that takes a sequence of tokens & returns a ``ResultSet``.

    The specific types of inputs and outputs are not known. However, inputs
    usually are strings. The requirements for inputs are:

    #. they must have ``__len__``
    #. they are indexable and slicable

    A parser must return a collection of ``Result``s. Their ``value`` elements
    can be any type but their ``remain`` elements must be a slice of the input
    tokens.

    This class can be used as a decorator::

        @Parser
        def head(tokens):
            if len(tokens) >= 1:
                return Success(result=Result(tokens[0], tokens[1:]))
            return Failure(head, 'Unable to obtain more tokens', tokens)

    In that example, ``head`` is a parser that returns the first element of the
    sequence of tokens. Then ``head`` can be chained (``then``, ``+``) with
    other parsers, filtered (``where``, ``value``), or composed together to be
    more useful.
    """

    def __init__(self, parser_function):
        """Decorates ``parser_function`` as a Parser object."""
        self.__parser_function = parser_function
        self.__name = parser_function.__name__
        functools.update_wrapper(self, parser_function)

    def is_named(self):
        try:
            return bool(self.__name)
        except AttributeError:
            return False

    def set_name(self, name):
        self.__name = name
        return self

    def get_name(self):
        try:
            return self.__name
        except AttributeError:
            return self.__class__.__name__

    def parse_with_context(self, tokens, context):
        """Parses input ``tokens`` under the context of ``context``."""
        return self.__parser_function(tokens)

    def parse(self, tokens):
        """Parses the input ``tokens`` under the default context."""
        return self.parse_with_context(tokens, default_parse_context(tokens))
    __call__ = parse

    def _to_parser(self, object):
        if isinstance(object, Parser):
            return object
        return Terminal(str(object))

    def _to_binder(self, object):
        if not isinstance(object, Parser):
            if callable(object):
                return object
            object = Terminal(str(object))
        return lambda _: object

    def __or__(self, other):
        """Composes with ``other`` to a biased choice between two parsers.

        The current parser is favored over the ``other``. That means only when
        the current parser fails, the ``other`` parser will be evaluated. Both
        parsers will be evaluated on the same input ``tokens``.

        ``other`` can either be a ``Parser`` object, or a value, in which case
        it will be converted to ``Terminal(str(other))``.

        This combinator is useful in case of greedy consumption::

            >>> empty = Succeed('')
            >>> s = (Terminal('s') + (lambda _: s)) | empty
            >>> set(s('ssss'))
            {Result(value='ssss', remain='')}
        """
        return _Or(self, self._to_parser(other), biased=True)

    def __ror__(self, other):
        """Convenient short form to allow ``'abc' | parser``.

        ``other`` is converted into a ``Terminal(str(other))`` if it is not
        already a Parser.

            >>> p = Terminal('def')
            >>> set(('abc' | p)('abcdefghi'))
            {Result(value='abc', remain='defghi')}
        """
        return self._to_parser(other) | self

    def __xor__(self, other):
        """Similar to the ``__or__`` operator, but both parsers are evaluated.

        ``other`` can either be a ``Parser`` object, or a value, in which case
        it will be converted to ``Terminal(str(other))``.

        The returned ``ResultSet`` is a union of both parsers. This combinator
        supports ambiguity in a grammar.

            >>> empty = Succeed('')
            >>> s = (Terminal('s') + (lambda _: s)) ^ empty
            >>> assert set(s('ss')) == {
            ...     Result(value='', remain='ss'),
            ...     Result(value='s', remain='s'),
            ...     Result(value='ss', remain=''),
            ... }
        """
        return _Or(self, self._to_parser(other), biased=False).memoize()

    def __rxor__(self, other):
        """Convenient short form to allow ``'abc' ^ parser``.

        ``other`` is converted into a ``Terminal(str(other))`` if it is not
        already a Parser.

            >>> p = Terminal('def')
            >>> set(('abc' ^ p)('abcdefghi'))
            {Result(value='abc', remain='defghi')}
        """
        return self._to_parser(other) ^ self

    def then(self, binder, reducer=lambda x, y: y):
        """Chains ``self`` and parser(s) returned by ``binder`` via ``reducer``.

        This is the ``bind`` function in monadic sense. ``binder`` is a callable
        that takes in a ``Result.value`` and returns a ``Parser`` object. This
        parser is then applied on ``Result.remain``.

        ``binder`` can also be a ``Parser`` object. In this case, ``binder`` is
        used directly as the second parser.

        If not, ``binder`` will be converted into a ``Terminal(str(binder))``.

        ``reducer`` takes two arguments, the first is ``Result.value`` of this
        parser, and the second is the ``Result.value`` of the second parser. The
        result of ``reducer`` makes up the final result of the composed parser.

        The default ``reducer`` only takes the second ``Result.value``.

        In code, this looks like::

            ret = Fail(tokens)
            for value, remain self(tokens):
                next_parser = binder(value)
                for next_value, next_remain in next_parser(remain):
                    final_value = reducer(value, next_value)
                    ret = ret.add(Result(final_value, next_remain))
        """
        return _Then(self, self._to_binder(binder), reducer)

    def skip(self, binder):
        """Similar to ``then``, but the reducer takes the first value."""
        return self.then(binder, lambda x, y: x)

    def __add__(self, other):
        """Equivalent to ``then`` with ``operator.concat`` as ``reducer``.

            >>> a = Terminal('a')
            >>> a8 = a + 890
            >>> set(a8('a890abc'))
            {Result(value='a890', remain='abc')}
        """
        return self.then(other, operator.concat)

    def __radd__(self, other):
        """Convenient short form to allow ``'abc' + parser``.

        ``other`` is converted into a ``Terminal(str(other))`` if it is not
        already a Parser.

            >>> p = Terminal('def')
            >>> set(('abc' + p)('abcdefghi'))
            {Result(value='abcdef', remain='ghi')}
        """
        return self._to_parser(other) + self

    def repeat(self, lower=0, upper=None,
               reducer=operator.concat, value='', take_all=False):
        """Repeatedly parses [lower, upper] occurrences.

        If ``upper`` is ``None``, there is no upper bound. The ``reducer`` is
        used to join the results together similar to how it is used in ``then``.
        The zeroth parse result (``parser`` is not invoked yet) is a ``Success``
        of ``value``. The first reduction is between zeroth and first results.
        If ``take_all``, then all results are returned. If not ``take_all``,
        then only the greediest results are returned.

            >>> p = Terminal('a').repeat()
            >>> set(p(''))
            {Result(value='', remain='')}
            >>> set(p('b'))
            {Result(value='', remain='b')}
            >>> set(p('a'))
            {Result(value='a', remain='')}
            >>> set(p('aa'))
            {Result(value='aa', remain='')}
        """
        return _Repeat(self, lower, upper, reducer, value, take_all)

    def value(self, converter_or_value):
        """Converts ``Result.value`` into a different value.

        ``converter_or_value`` can be a callable, or an object. If it is a
        callable, it takes ``Result.value`` and returns a converted value. If it
        is a value, that value is used.

        For example:

            >>> digit = One.where(lambda c: '0' <= c <= '9')
            >>> set(digit('8bc'))
            {Result(value='8', remain='bc')}
            >>> digit_as_int = digit.value(int)
            >>> set(digit_as_int('8bc'))
            {Result(value=8, remain='bc')}
        """
        if callable(converter_or_value):
            return self.then(lambda v: Succeed(converter_or_value(v)))
        return self.then(Succeed(converter_or_value))

    def filter(self, callback, take_if=True):
        """Executes ``callback`` on a successful parse and filters results.

        ``callback`` must take a ``Result``. Every possible result of a rule
        will be passed to ``callback``.

        If truth value as returned by ``callback`` is the same as ``take_if``,
        that ``Result`` object is included.

        **NOTE:** The ordering between ``filter`` and ``memoize`` is important
        and may result in ``callback`` not being invoked.
        """
        return _Filter(self, callback, take_if=take_if)

    def where(self, predicate):
        """Selects results whose values pass ``predicate``.

        ``predicate`` is a callable that takes ``Result.value`` and returns
        ``True`` if that ``Result`` should be included. This is a convenient
        wrapper around ``filter``.

        For example::

            >>> digit = One.where(lambda c: '0' <= c <= '9')
            >>> set(digit('abc'))
            set()
            >>> set(digit('8bc'))
            {Result(value='8', remain='bc')}
        """
        return self.filter(lambda r: predicate(r.value), take_if=True)

    def call(self, callback):
        """Simple wrapper around ``filter`` to always call ``callback``.

        In ambiguous grammar (like the example below), there might be repeated
        results if ``call`` makes up a part of the variable. Please note the
        difference in two definitions of the same production rule.

            >>> count = 0
            >>> def cb(r):
            ...     global count
            ...     count += 1
            >>> empty = Succeed('')
            >>> s = ((Terminal('s') + (lambda _: s)) ^ empty).call(cb)
            >>> r = s('ss')
            >>> assert len(r) == 3
            >>> count
            6
            >>> count = 0
            >>> s = (Terminal('s') + (lambda _: s)) ^ empty
            >>> r = s.call(cb)('ss')
            >>> assert len(r) == 3
            >>> count
            3
        """
        return self.filter(lambda r: callback(r) or True)

    def memoize(self):
        """Memoizes parsed results of ``self``.

        The memoization allows for ambiguous grammar to be processed
        efficiently. See the paper `Parser Combinators for Ambiguous Left
        Recursive Grammars <https://doi.org/10.1007/978-3-540-77442-6_12>`_.

        This modifier is recommended when the unbiased ``__xor__`` operator is
        used, or when left recursion is in the grammar::

            >>> empty = Succeed('')
            >>> s = ((Terminal('s') + (lambda _: s) + (lambda _: s)) ^
            ...      empty).memoize()
            >>> len(s('s' * 20))
            21

        Without the ``memoize`` modifier in the above example, it would take a
        very long time to parse.
        """
        return _Memoize(self)

    def __repr__(self):
        name = self.get_name()
        return f'Parser<{name}@{id(self)}>'


# Some useful initial parsers.

class Succeed(Parser):
    """Always returns a parsed result of ``value`` regardless of input.

    For example::

        >>> s = Succeed(10)
        >>> set(s('abc'))
        {Result(value=10, remain='abc')}
        >>> set(s('def'))
        {Result(value=10, remain='def')}
    """

    def __init__(self, value):
        self.__value = value

    def parse_with_context(self, tokens, context):
        return Success(result=Result(self.__value, tokens))

    def __call__(self, tokens):
        return Success(result=Result(self.__value, tokens))


#: An empty string. This always succeeds.
Empty = Succeed('')


@Parser
def Fail(tokens):
    """Always returns a ``Failure``.

        >>> set(Fail('abc'))
        set()
        >>> set(Fail('def'))
        set()
    """
    return Failure(Fail, 'Requested failure.', tokens)


@Parser
def One(tokens):
    """Returns the first element from the input ``tokens``.

    This is most useful when combined with other parsers or filters, such as
    ``where``. For example::

        >>> set(One(''))
        set()
        >>> digit = One.where(lambda c: '0' <= c <= '9')
        >>> set(digit('0123'))
        {Result(value='0', remain='123')}
    """
    return Succeed(tokens[0])(tokens[1:]) if tokens else Failure(
            One, 'Expecting at least one token.', tokens)


class Terminal(Parser):
    """Matches ``terminal`` to the beginning of input tokens.

        >>> t = Terminal('t')
        >>> set(t(''))
        set()
        >>> set(t('t'))
        {Result(value='t', remain='')}
    """

    def __init__(self, terminal):
        self.__terminal = terminal

    def parse_with_context(self, tokens, context):
        if tokens[:len(self.__terminal)] != self.__terminal:
            return Failure(self, f'Expecting terminal {self.__terminal!r}.',
                           tokens)
        return Succeed(self.__terminal)(tokens[len(self.__terminal):])


# Various combinators for internal use.

class _Filter(Parser):
    def __init__(self, parser, predicate, take_if=False):
        self.__parser = parser
        self.__predicate = predicate
        self.__take_if = take_if

    def parse_with_context(self, tokens, context):
        rs = self.__parser.parse_with_context(tokens, context)
        if not rs:
            return Failure(self, 'Empty result set before filtering.', tokens,
                           rs)
        ret = Success(results=(r for r in rs
                               if self.__predicate(r) == self.__take_if))
        if not ret:
            ret = Failure(
                    self, f'{self.__predicate!r} emptied out {rs}.', tokens)
        return ret


class _Or(Parser):
    def __init__(self, parser1, parser2, biased=True):
        self.__parser1 = parser1
        self.__parser2 = parser2
        self.__biased = biased

    def parse_with_context(self, tokens, context):
        r1 = self.__parser1.parse_with_context(tokens, context)
        r = r1
        if not r or not self.__biased:
            r2 = self.__parser2.parse_with_context(tokens, context)
            r = r1.add_all(r2)
        if not r1 and not r2:
            r = Failure(self, 'Parsers produced empty set.', tokens,
                        (r1, r2))
        return r


class _Then(Parser):
    def __init__(self, parser, binder, reducer):
        self.__parser = parser
        self.__binder = binder
        self.__reducer = reducer

    def parse_with_context(self, tokens, context):
        first_rs = self.__parser.parse_with_context(tokens, context)
        if not first_rs:
            return Failure(self, 'First parser returned empty set.', tokens,
                           first_rs)
        failures = []
        ret = Fail(tokens)
        for v, r in first_rs:
            next_parser = self.__binder(v)
            next_rs = next_parser.parse_with_context(r, context)
            if not next_rs:
                failures.append(next_rs)
                continue
            ret = ret.add_all((Result(self.__reducer(v, nv), nr)
                              for nv, nr in next_rs))
        if not ret:
            return Failure(self, 'The chain returned empty.', tokens, failures)
        return ret


class _Memoize(Parser):
    def __init__(self, parser):
        self.__parser = parser

    def memoize(self):
        return self

    def parse_with_context(self, tokens, context):
        result_key = (1, id(self.__parser), len(tokens))
        recursion_key = (2, id(self.__parser), len(tokens))
        try:
            return context._memotable[result_key]
        except KeyError:
            max_depth = len(tokens) + 1
            if context.options.max_recursion is not None:
                max_depth = min(max_depth, context.options.max_recursion)
            context._memotable[recursion_key] = context._memotable.get(
                    recursion_key, 0) + 1
            if context._memotable[recursion_key] > max_depth:
                return Failure(self, 'Deep recursion.', tokens)
            rs = self.__parser.parse_with_context(tokens, context)
            context._memotable[result_key] = rs
            return rs


class _Repeat(Parser):
    """Concatenates ``parser`` between ``lower`` and ``upper`` times, inclusive.

    If ``take_all`` is ``True``, all parsed results in that range will be
    returned. Otherwise, only the greediest results are returned.

    **NOTE:** The zeroth parse will result in a ``Success`` of ``value``. It is
    important that ``reducer`` takes that into account when it reduces the
    zeroth and the first parse results (when ``parser`` is invoked the first
    time).
    """

    def __init__(self, parser, lower=0, upper=None,
                 reducer=operator.concat,
                 value='',
                 take_all=False):
        assert upper is None or lower <= upper
        self.__parser = parser
        self.__lower = lower if lower else 0
        self.__upper = upper
        self.__reducer = reducer
        self.__take_all = take_all
        self.__value = value

    def parse_with_context(self, tokens, context):
        ret = Fail(tokens)
        results = Succeed(self.__value)(tokens)
        count = 0
        if count >= self.__lower:
            ret = results
        failures = []
        while (self.__upper is None or count < self.__upper):
            count += 1
            if count <= self.__lower:
                failures.clear()
            next_results = Fail(tokens)
            for v, r in results:
                this_loop = self.__parser.parse_with_context(r, context)
                if not this_loop and count <= self.__lower:
                    failures.append(this_loop)
                    continue
                next_results = next_results.add_all(
                        Result(self.__reducer(v, nv), nr)
                        for nv, nr in this_loop)
            if not next_results:
                break
            results = next_results
            if self.__take_all and (count >= self.__lower):
                ret = ret.add_all(results)
            else:
                ret = results
        if not ret:
            ret = Failure(
                self,
                f'Repeat {self.__lower}-{self.__upper!r} produced empty set.',
                tokens,
                failures
            )
        return ret
