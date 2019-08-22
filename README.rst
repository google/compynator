==========
Compynator
==========

Introduction
============

Compynator is a tiny (~400 SLOCs), pure Python implementation of `parser
combinators <https://en.wikipedia.org/wiki/Parser_combinator>`_. With this
library, one can build up a complex parser from primitive parsers such as "get
one token" (the ``One`` parser), or compose parsers together such as "if this
parser fails, try that parser" (the ``Or`` combinator), etc. The mental model is
a binary tree of execution nodes through which a sequence of input tokens flows.

The implementation in this package supports optional memoization and curtailment
as described in [Frost2007]_. This allows a memoized parser to achieve
asymptotically best performance, and support ambiguous grammars.

Compynator is not an official Google product.

Examples
--------

An example that solves
https://leetcode.com/problems/parsing-a-boolean-expression:

.. code-block:: python

    t = Terminal('t').value(True)
    f = Terminal('f').value(False)

    e = Forward()
    n = Terminal('!(').then(e).value(lambda x: not x).skip(')')

    a_empty = Succeed(True)
    a_e_tail = Forward()
    a_e_tail.is_(
        Terminal(',').then(e).then(
        a_e_tail, lambda x, y: x and y) | a_empty)
    a = Terminal('&(').then(e).then(
        a_e_tail, lambda x, y: x and y).skip(')')

    o_empty = Succeed(False)
    o_e_tail = Forward()
    o_e_tail.is_(
        Terminal(',').then(e).then(
        o_e_tail, lambda x, y: x or y) | o_empty)
    o = Terminal('|(').then(e).then(
        o_e_tail, lambda x, y: x or y).skip(')')

    e.is_(t | f | n | a | o)

    self.assertEqual(e('!(f)'), {Result(True, '')})
    self.assertEqual(e('|(f,t)'), {Result(True, '')})
    self.assertEqual(e('&(t,f)'), {Result(False, '')})
    self.assertEqual(e('|(&(t,f,t),!(t))'), {Result(False, '')})

See `test_benchmark.py <tests/test_benchmark.py>`_ for a literal translation of
the ABNF rules for URI parsing as given in [RFC3986]_ into a structured tree::

    Uri
        Scheme http
        HierPart
            Authority
                UserInfo None
                Host www.ics.uci.edu
                Port None
            Path /pub/ietf/uri/
        Query query
        Fragment fragment

There are more examples in the `tests <tests>`_ directory.

Translating Augmented Backus-Naur Form (ABNF)
---------------------------------------------

Care should be taken to ensure that ``repeat`` takes all possible results
instead of greedily parsing for the most. For example, the ABNF ``*3"a" "aa"``
cannot be simply translated as ``Terminal('a').repeat(0, 3) + 'aa'``. This
translation will fail to parse ``aaaa`` because it greedily matches the first
3 ``a``'s, then fails to find the remaining 2 ``a``'s. The correct translation
should be ``Terminal('a').repeat(0, 3, take_all=True) + 'aa'``.

===================  ====================================
        ABNF                     Compynator
===================  ====================================
Terminal             ``Terminal``
Rule                 ``Parser``
Concatenation        ``p1 + p2``
Alternative          ``p1 | p2``
Sequence group       Normal use of parentheses
Variable repetition  ``p.repeat(lower, upper, take_all)``
Specific repetition  ``p.repeat(x, x)``
Optional             ``p.repeat(0, 1, take_all)``
===================  ====================================

Translating Parsing Expression Grammar (PEG)
--------------------------------------------

Unlike ABNF, PEG operators are always greedy. When translating PEG, we do not
need to worry about backtracking the repetitions with ``take_all``.

==============  ===============================
     PEG                  Compynator
==============  ===============================
Terminal        ``Terminal``
Nonterminal     ``Parser``
Epsilon         ``Empty``
--------------  -------------------------------
Sequence        ``p1 + p2``
Ordered choice  ``p1 | p2``
Zero or more    ``p.repeat()``
One or more     ``p.repeat(1)``
Optional        ``p.repeat(0, 1)``
And predicate   ``Lookahead(p)``
Not predicate   ``Lookahead(p, take_if=False)``
==============  ===============================

Combinator vs Generator
=======================

Advantages
----------

Advantages of parser combinators versus parser generators are:

#. Readability. A grammar can be expressed in a very similar form as its BNF.
   The code can be considered an *executable specification* of the grammar.
#. Simple setup. The code is the grammar. There is no need to run a generator to
   regenerate code when the grammar changes.
#. Understandability. Each parser is generally short and simple that its
   correctness can be easily verified. There is no need to look into generated
   code, or the code of the parser generator.
#. Parser combinators support context-sensitive grammars. For example, to parse
   an XML body, assuming ``start`` parses a start tag, ``body`` parses the body,
   and ``end`` parses a specified end tag:

   .. code-block:: python

       xml_tag = start.then(lambda tag_name: body.skip(end(tag_name)))

#. Combination of lexing and parsing. Most parser generators perform their
   lexing and parsing phases separately. Parser combinators combine these phases
   together. Hence they are not limited to string inputs. The example (in
   `test_core.py <tests/test_core.py>`_) below takes a tokenized sequence.

   .. code-block:: python

        NUM, OP, TERMINAL = 0, 1, 2
        tokens = [(NUM, 2), (OP, operator.add), (NUM, 10),
                  (OP, operator.mul), (NUM, 4)]
        num = One.where(lambda c: c[0] == NUM)
        op = One.where(lambda c: c[0] == OP).value(lambda c: c[1])
        mult_div = op.where(lambda c: c in (operator.mul, operator.truediv))
        add_sub = op.where(lambda c: c in (operator.add, operator.sub))
        left_paren = One.where(lambda c: c[0] == TERMINAL and c[1] == '(')
        right_paren = One.where(lambda c: c[0] == TERMINAL and c[1] == ')')
        expr = Forward()
        factor = (
            num.value(lambda t: t[1]) |
            left_paren.then(expr).skip(right_paren)
        )
        def do_op(left, op, right):
            return op(left, right)
        term = Forward()
        term.is_((
            Collect(term, mult_div, factor).value(lambda v: do_op(*v)) ^
            factor
        ).memoize())
        expr.is_((
            Collect(expr, add_sub, term).value(lambda v: do_op(*v)) ^
            term
        ).memoize())
        calc = expr.filter(lambda r: not r.remain)
        self.assertEqual(
                set(expr(tokens)),
                {
                    Result(value=42, remain=[]),
                    Result(value=12, remain=tokens[3:]),
                    Result(value=2, remain=tokens[1:]),
                })
        self.assertEqual(calc(tokens), Succeed(42)([]))

Disadvantages
-------------

Disadvantages of parser combinators are:

#. Familiarity. Most textbooks write about parser generators and traditional
   parsing techniques such as LL, LR, etc. Parser combinators are more common
   in functional and logic programming communities, as popularized by
   [Wadler1985]_ and [Hutton1992]_.
#. Coupling of code and grammar. The downside of simple setup is a tight
   coupling of code and grammar, which might make it difficult to understand.
#. As it is implemented here, performance might be impacted due to composition
   overhead. See `test_benchmark.py <tests/test_benchmark.py>`_ for details. On
   the same machine, the result for URI parsing could be ~70 times slower::

     t.test_parse_uri() 903.5961110000001 usec per run
     t.test_urlparse() 13.704007000000074 usec per run

#. All the advantages and disadvantages of scannerless parsing apply too.

Limitations
===========

Currently, this library does not implement:

#. Source context such as line and column number of the token.
#. "Greedy" matching in the same sense as in regular expression (i.e. longest
   match). The greedy operation in this library is in the "greedy algorithm"
   sense, i.e. the first rule that matches will be taken.
#. Space treatments. Spaces have to be explicitly taken care of in grammars.

References
==========

.. [Wadler1985] Wadler, Philip. (1985). "How to replace failure by a list of
   successes". Proc. conference on functional programming and computer
   architecture. Springer–Verlag.

.. [Hutton1992] Hutton, Graham. (1992). "Higher-order functions for parsing".
   Journal of functional programming, 2(3), 323–343.

.. [Frost2007] Frost R.A., Hafiz R., Callaghan P. (2007) "Parser Combinators for
   Ambiguous Left-Recursive Grammars". In: Hudak P., Warren D.S. (eds)
   "Practical Aspects of Declarative Languages". PADL 2008. Lecture Notes in
   Computer Science, vol 4902. Springer, Berlin, Heidelberg

.. [RFC3986] Berners-Lee, T., Fielding, R., and L. Masinter, "Uniform Resource
   Identifier (URI): Generic Syntax", STD 66, RFC 3986, January 2005.
