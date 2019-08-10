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

# PYTHONPATH=. python3 tests/test_benchmark.py

import ast
import re
import sys
import unittest
from urllib.parse import urlparse, ParseResult
import timeit

try:
    import pyparsing
except ImportError:
    pass

from compynator import *


class PythonExprTest(unittest.TestCase):
    def setUp(self):
        # Translated from https://github.com/gvanrossum/pegen/blob/ec2b354f64f6dbfcb46133757fe4c0e07880f526/test/test_pegen.py#L232
        ident = Alpha + Alnum.repeat()
        atom = (
            ident.value(lambda v: ast.Name(id=v, ctx=ast.Load())) |
            Digit.repeat(1).value(lambda v: ast.Constant(
                    value=ast.literal_eval(v)))
        )
        expr = Forward('expr')
        factor = (
            Terminal('(').then(expr).skip(')') |
            atom
        )
        memotable = {}
        term = Forward('term')
        term = (
            term.skip('*').then(factor,
                    lambda l, r: ast.BinOp(l, ast.Mult(), r)) ^
            term.skip('/').then(factor,
                    lambda l, r: ast.BinOp(l, ast.Div(), r)) ^
            factor
        ).memoize()
        expr = (
            expr.skip('+').then(term, lambda l, r: ast.BinOp(l, ast.Add(), r)) ^
            expr.skip('-').then(term, lambda l, r: ast.BinOp(l, ast.Sub(), r)) ^
            term
        ).memoize()
        start = expr.skip(Terminal('\n').repeat(0, 1)).filter(
                lambda r: not r.remain).value(
                lambda v: ast.fix_missing_locations(
                        ast.Expression(v, lineno=1, col_offset=0)))
        self.parser = start

    def test_python_expr(self):
        parsed_results = self.parser('(1+2*3+5)/(6-2)\n')
        self.assertEqual(len(parsed_results), 1)
        tree = next(iter(parsed_results)).value
        code = compile(tree, "", "eval")
        val = eval(code)
        self.assertEqual(val, 3.0)


class ParseUriTest(unittest.TestCase):
    class Node:
        pass
    class CompositeNode(Node):
        def __init__(self, *children):
            self.children = []
            for child in children:
                self.add_child(child)
        def add_child(self, node):
            self.children.append(node)
        def pprint(self, level=0, indent=4):
            print(' ' * level * indent, self.__class__.__name__)
            for child in self.children:
                if child:
                    child.pprint(level + 1, indent)
        def __getitem__(self, index):
            return self.children[index]
    class LeafNode(Node):
        def __init__(self, value):
            self.value = value
        def pprint(self, level=0, indent=4):
            print(' ' * level * indent, self.__class__.__name__, self.value)
    class Scheme(LeafNode):
        def __init__(self, scheme):
            super().__init__(scheme)
    class Path(LeafNode):
        def __init__(self, path):
            super().__init__(path)
    class Query(LeafNode):
        def __init__(self, query):
            super().__init__(query)
    class Fragment(LeafNode):
        def __init__(self, fragment):
            super().__init__(fragment)
    class UserInfo(LeafNode):
        def __init__(self, userinfo):
            super().__init__(userinfo)
    class Host(LeafNode):
        def __init__(self, host):
            super().__init__(host)
    class Port(LeafNode):
        def __init__(self, port):
            super().__init__(port)
    class Authority(CompositeNode):
        def __init__(self, userinfo, host, port):
            if userinfo is None:
                userinfo = ParseUriTest.UserInfo(None)
            if host is None:
                host = ParseUriTest.Host(None)
            if port is None:
                port = ParseUriTest.Port(None)
            super().__init__(userinfo, host, port)
    class HierPart(CompositeNode):
        def __init__(self, authority, path):
            if authority is None:
                authority = ParseUriTest.Authority(None, None, None)
            if path is None:
                path = ParseUriTest.Path(None)
            super().__init__(authority, path)
    class Uri(CompositeNode):
        def __init__(self, scheme, hier_part, query, fragment):
            if scheme is None:
                scheme = ParseUriTest.Scheme(None)
            if hier_part is None:
                hier_part = ParseUriTest.HierPart(None, None)
            if query is None:
                query = ParseUriTest.Query(None)
            if fragment is None:
                fragment = ParseUriTest.Fragment(None)
            super().__init__(scheme, hier_part, query, fragment)

    def create_query(v):
        return Query(v)
    def create_fragment(v):
        return Fragment(v)
    def create_path(v):
        return Path(v)

    def setUp(self):
        # See Appendix A in https://tools.ietf.org/html/rfc3986.
        sub_delims = One.where(lambda c: c in "!$&'()*+,;=")
        gen_delims = One.where(lambda c: c in ':/?#[]@')
        reserved = gen_delims | sub_delims
        unreserved = Alpha | Digit | '-' | '.' | '_' | '~'
        pct_encoded = '%' + HexDigit + HexDigit
        pchar = unreserved | pct_encoded | sub_delims | ':' | '@'
        query = (pchar | '/' | '?').repeat().value(self.Query)
        fragment = (pchar | '/' | '?').repeat().value(self.Fragment)
        segment = pchar.repeat()
        segment_nz = pchar.repeat(1)
        segment_nz_nc = (unreserved | pct_encoded | sub_delims | '@').repeat(1)
        path_empty = Empty
        path_rootless = (segment_nz + ('/' + segment).repeat())
        path_noscheme = (segment_nz_nc + ('/' + segment).repeat())
        path_absolute = ('/' + ((segment_nz + ('/' + segment).repeat()) |
                                Empty))
        path_abempty = ('/' + segment).repeat()
        path = (path_abempty | path_absolute | path_noscheme | path_rootless
                | path_empty)
        reg_name = (unreserved | pct_encoded | sub_delims).repeat()
        dec_octet = (Digit |
                     (One.where(lambda c: '1' <= c <= '9') + Digit) |
                     ('1' + Digit + Digit) |
                     ('2' + One.where(lambda c: '0' <= c <= '4') + Digit) |
                     ('25' + One.where(lambda c: '0' <= c <= '5')))
        ipv4address = (dec_octet + '.' + dec_octet + '.'
                       + dec_octet + '.' + dec_octet)
        h16 = HexDigit.repeat(lower=1, upper=4)
        ls32 = (h16 + ':' + h16) | ipv4address
        h16c = h16 + ':'
        ipv6address = (
                (h16c.repeat(6, 6) + ls32) |
                ('::' + h16c.repeat(5, 5) + ls32) |
                (h16.repeat(0, 1) + '::' + h16c.repeat(4, 4) + ls32) |
                ((h16c.repeat(0, 1) + h16).repeat(0, 1) + '::' +
                 h16c.repeat(3, 3) + ls32) |
                ((h16c.repeat(0, 2, take_all=True) + h16).repeat(0, 1) + '::' +
                 h16c.repeat(2, 2) + ls32) |
                ((h16c.repeat(0, 3, take_all=True) + h16).repeat(0, 1) + '::' +
                 h16c + ls32) |
                ((h16c.repeat(0, 4, take_all=True) + h16).repeat(0, 1) + '::' +
                 ls32) |
                ((h16c.repeat(0, 5, take_all=True) + h16).repeat(0, 1) + '::' +
                 h16) |
                ((h16c.repeat(0, 6, take_all=True) + h16).repeat(0, 1) + '::')
        )
        ipvfuture = ('v' + HexDigit + '.' +
                     (unreserved | sub_delims | ':').repeat(lower=1))
        ip_literal = '[' + (ipv6address | ipvfuture) + ']'
        port = Digit.repeat().value(self.Port)
        port_or_empty = Terminal(':').then(port) | Empty.value(None)
        host = (ip_literal | ipv4address | reg_name).value(self.Host)
        userinfo = (unreserved | pct_encoded | sub_delims | ':').repeat().value(
                self.UserInfo)
        userinfo_or_empty = userinfo.skip('@') | Empty.value(None)
        authority = userinfo_or_empty.then(host, lambda u, h: (u, h)).then(
                port_or_empty, lambda uh, p: self.Authority(uh[0], uh[1], p))
        scheme = (Alpha + (Alpha | Digit | '+' | '-' | '.').repeat()).value(
                self.Scheme)
        relative_part = (
                Terminal('//').then(authority).then(path_abempty,
                        lambda a, p: self.HierPart(a, self.Path(p))) |
                path_absolute.value(
                        lambda v: self.HierPart(None, self.Path(v))) |
                path_noscheme.value(
                        lambda v: self.HierPart(None, self.Path(v))) |
                path_empty.value(
                        lambda v: self.HierPart(None, self.Path(v)))
        )
        query_or_empty = Terminal('?').then(query) | Empty.value(None)
        fragment_or_empty = Terminal('#').then(fragment) | Empty.value(None)
        relative_ref = \
                relative_part.then(query_or_empty, lambda h, q: (h, q)).then(
                fragment_or_empty, lambda hq, f: self.Uri(
                        None, hq[0], hq[1], f))
        hier_part = (
                Terminal('//').then(authority).then(path_abempty,
                        lambda a, p: self.HierPart(a, self.Path(p))) |
                path_absolute.value(
                        lambda v: self.HierPart(None, self.Path(v))) |
                path_rootless.value(
                        lambda v: self.HierPart(None, self.Path(v))) |
                path_empty.value(
                        lambda v: self.HierPart(None, self.Path(v)))
        )
        uri = scheme.skip(':').then(hier_part, lambda s, h: (s, h)).then(
                query_or_empty, lambda sh, q: (sh[0], sh[1], q)).then(
                fragment_or_empty, lambda shq, f: self.Uri(
                        shq[0], shq[1], shq[2], f))
        absolute_uri = scheme + ':' + hier_part + (('?' + query) | Empty)
        uri_reference = uri | relative_ref

        self.parser = uri_reference
        self.url = 'http://www.ics.uci.edu/pub/ietf/uri/?query#fragment'
        return self.parser

    def test_parse_relative(self):
        prs = self.parser(self.url.lstrip('http://'))
        self.assertEqual(len(prs), 1)
        pr = next(iter(prs))
        self.assertEqual(pr.remain, '')
        r = pr.value
        self.assertEqual(r.children[0].value, None)
        self.assertEqual(r.children[1][0][1].value, None)
        self.assertEqual(r.children[1][1].value,
                         'www.ics.uci.edu/pub/ietf/uri/')
        self.assertEqual(r.children[2].value, 'query')
        self.assertEqual(r.children[3].value, 'fragment')

    def test_parse_no_scheme(self):
        prs = self.parser(self.url.lstrip('http:'))
        self.assertEqual(len(prs), 1)
        pr = next(iter(prs))
        self.assertEqual(pr.remain, '')
        r = pr.value
        self.assertEqual(r.children[0].value, None)
        self.assertEqual(r.children[1][0][1].value, 'www.ics.uci.edu')
        self.assertEqual(r.children[1][1].value, '/pub/ietf/uri/')
        self.assertEqual(r.children[2].value, 'query')
        self.assertEqual(r.children[3].value, 'fragment')

    def test_parse_uri(self):
        prs = self.parser(self.url)
        self.assertEqual(len(prs), 1)
        pr = next(iter(prs))
        self.assertEqual(pr.remain, '')
        r = pr.value
        self.assertEqual(r.children[0].value, 'http')
        self.assertEqual(r.children[1][0][1].value, 'www.ics.uci.edu')
        self.assertEqual(r.children[1][1].value, '/pub/ietf/uri/')
        self.assertEqual(r.children[2].value, 'query')
        self.assertEqual(r.children[3].value, 'fragment')

    def test_urlparse(self):
        r = urlparse(self.url)
        self.assertEqual(r.scheme, 'http')
        self.assertEqual(r.netloc, 'www.ics.uci.edu')
        self.assertEqual(r.path, '/pub/ietf/uri/')
        self.assertEqual(r.query, 'query')
        self.assertEqual(r.fragment, 'fragment')

    def test_ipv6_okay(self):
        url = 'https://[2001:db8:85a3::8a2e:370:7334]:443/'
        prs = self.parser(url)
        self.assertEqual(len(prs), 1)
        pr = next(iter(prs))
        self.assertEqual(pr.remain, '')
        r = pr.value
        self.assertEqual(r.children[0].value, 'https')
        self.assertEqual(
            r.children[1][0][1].value, '[2001:db8:85a3::8a2e:370:7334]')
        self.assertEqual(r.children[1][0][2].value, '443')
        self.assertEqual(r.children[1][1].value, '/')
        self.assertEqual(r.children[2].value, None)
        self.assertEqual(r.children[3].value, None)

    def test_ipv6_invalid(self):
        url = 'https://[ghi::]:443/'
        r = urlparse(url)
        # urlparse *incorrectly* accepts this.
        self.assertEqual(r.netloc, '[ghi::]:443')

        prs = self.parser(url)
        self.assertEqual(len(prs), 1)
        pr = next(iter(prs))
        self.assertNotEqual(pr.remain, '')

    def test_ipfuture(self):
        url = 'https://[v8.2001:db8:85a3::8a2e:370:7334]:443/'
        r = urlparse(url)
        self.assertEqual(r.netloc, '[v8.2001:db8:85a3::8a2e:370:7334]:443')
        prs = self.parser(url)
        self.assertEqual(len(prs), 1)
        pr = next(iter(prs))
        self.assertEqual(pr.remain, '')
        r = pr.value
        self.assertEqual(r.children[0].value, 'https')
        self.assertEqual(
            r.children[1][0][1].value, '[v8.2001:db8:85a3::8a2e:370:7334]')
        self.assertEqual(r.children[1][0][2].value, '443')
        self.assertEqual(r.children[1][1].value, '/')
        self.assertEqual(r.children[2].value, None)
        self.assertEqual(r.children[3].value, None)


@unittest.skipUnless('pyparsing' in sys.modules, 'Need PyParsing')
class PyParsingTest(unittest.TestCase):
    def setUp(self):
        self._opn = {
            "+" : operator.add,
            "-" : operator.sub,
            "*" : operator.mul,
            "/" : operator.truediv,
        }
        self._expr_stack = []
        self._expr = '-(1+2*3+5)/(6-2)'
        self._create_parser()
        self._create_pyparser()

    def _create_parser(self):
        def xo_yo(fo1, fo2):
            if not fo1:
                return fo2
            return fo1[1](fo1[0], fo2[0]), fo2[1]
        def xo_x(fo1, fo2):
            if not fo1:
                return fo2
            return fo1[1](fo1[0], fo2)
        fnumber = Regex(
                re.compile(r"[+-]?\d+(?:\.\d*)?(?:[eE][+-]?\d+)?")).value(float)
        plus, minus, mult, div = map(Terminal, '+-*/')
        addop = (plus | minus).value(lambda o: self._opn[o])
        multop = (mult | div).value(lambda o: self._opn[o])
        expr = Forward('expr')
        sub_expr = Terminal('(').then(expr).skip(')')
        factor = Terminal('-').repeat(0, 1).then(
                fnumber | sub_expr, lambda x, y: -y if x else y)
        term = factor.then(multop, lambda f, o: (f, o)).repeat(0, None,
                xo_yo).then(factor, xo_x)
        expr = term.then(addop, lambda f, o: (f, o)).repeat(0, None,
                xo_yo).then(term, xo_x)
        self.parser = expr

    def _create_pyparser(self):
        # Copied from
        # https://github.com/pyparsing/pyparsing/blob/master/examples/fourFn.py.
        from pyparsing import (Literal, Group, Forward, Regex, ZeroOrMore,
                               Suppress)
        def pushFirst( strg, loc, toks ):
            self._expr_stack.append( toks[0] )
        def pushUMinus( strg, loc, toks ):
            for t in toks:
                if t == '-':
                    self._expr_stack.append( 'unary -' )
                else:
                    break
        fnumber = Regex(r"[+-]?\d+(?:\.\d*)?(?:[eE][+-]?\d+)?")
        plus, minus, mult, div = map(Literal, '+-*/')
        lpar, rpar = map(Suppress, '()')
        addop = plus | minus
        multop = mult | div
        expr = Forward()
        atom = ((0, None)*minus + (fnumber.setParseAction(pushFirst) |
                Group(lpar + expr + rpar))).setParseAction(pushUMinus)
        factor = Forward()
        factor << atom
        term = factor + ZeroOrMore((multop + factor).setParseAction(pushFirst))
        expr << term + ZeroOrMore((addop + term).setParseAction(pushFirst))
        self.pyparser = expr

    def _evaluate_stack(self):
        op = self._expr_stack.pop()
        if op == 'unary -':
            return -self._evaluate_stack()
        if op in "+-*/^":
            op2 = self._evaluate_stack()
            op1 = self._evaluate_stack()
            return self._opn[op]( op1, op2 )
        else:
            return float( op )

    def test_pyparser(self):
        self._expr_stack.clear()
        self.pyparser.parseString(self._expr)
        self.assertEqual(self._evaluate_stack(), -3.0)

    def test_parser(self):
        prs = self.parser(self._expr)
        self.assertEqual(next(iter(prs)).value, -3.0)


def bench_python_expr():
    benchmark('PythonExprTest("test_python_expr").setUp()')
    benchmark('t.test_python_expr()',
              setup='from __main__ import PythonExprTest;'
                    't = PythonExprTest("test_python_expr"); t.setUp()')


def bench_uri_parse():
    benchmark('ParseUriTest("test_parse_uri").setUp()')
    benchmark('t.test_parse_uri()',
              setup='from __main__ import ParseUriTest;'
                    't = ParseUriTest("test_parse_uri"); t.setUp()')


def bench_urlparse():
    benchmark('t.test_urlparse()',
              setup='from __main__ import ParseUriTest;'
                    't = ParseUriTest("test_urlparse"); t.setUp()')


def bench_pyparsing():
    benchmark('t.test_pyparser()',
              setup='from __main__ import PyParsingTest;'
                    't = PyParsingTest("test_pyparser"); t.setUp()')
    benchmark('t.test_parser()',
              setup='from __main__ import PyParsingTest;'
                    't = PyParsingTest("test_parser"); t.setUp()')


def benchmark(stmt, number=1000, setup=None):
    names = [
        'PythonExprTest',
        'ParseUriTest',
        'urlparse',
    ]
    if not setup:
        setup = 'from __main__ import ' + ','.join(names)

    total = timeit.timeit(stmt, number=number, setup=setup)
    unit = 'usec'
    usec = total * 1E6 / number
    if usec > 1000:
        msec = usec / 1000
        unit = 'msec'
        if msec > 1000:
            sec = total
            unit = 'sec'
    print(stmt, locals()[unit], unit, 'per run')


if __name__ == '__main__':
    bench_python_expr()
    bench_uri_parse()
    bench_urlparse()
    bench_pyparsing()
