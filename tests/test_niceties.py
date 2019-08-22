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

import re
import string
import unittest

from compynator import *


class NicetiesTest(unittest.TestCase):

    def _test_class(self, p, char_class):
        self.assertFalse(p(''))
        for c in (chr(x) for x in range(256)):
            if c in char_class:
                self.assertEqual(p(c), Succeed(c)(''))
                self.assertEqual(p(c + 'abc'), Succeed(c)('abc'))
            else:
                self.assertFalse(p(c))

    def test_digit(self):
        self._test_class(Digit, string.digits)

    def test_hexdigit(self):
        self._test_class(HexDigit, string.digits + 'abcdefABCDEF')

    def test_octdigit(self):
        self._test_class(OctDigit, string.octdigits)

    def test_lower(self):
        self._test_class(Lower, string.ascii_lowercase)

    def test_upper(self):
        self._test_class(Upper, string.ascii_uppercase)

    def test_alpha(self):
        self._test_class(Alpha, string.ascii_letters)

    def test_alnum(self):
        self._test_class(Alnum, string.ascii_letters + string.digits)

    def test_regex(self):
        r = Regex(re.compile('a+'))
        self.assertFalse(r('b'))
        self.assertEqual(r('ab'), Succeed('a')('b'))
        self.assertEqual(r('aab'), Succeed('aa')('b'))

    def test_iterminal(self):
        true = ITerminal('t')
        self.assertEqual(true('t'), Succeed('t')(''))
        self.assertEqual(true('T'), Succeed('T')(''))
        self.assertEqual(true('ta'), Succeed('t')('a'))
        self.assertEqual(true('Ta'), Succeed('T')('a'))

        ab = ITerminal('ab')
        self.assertFalse(ab(''))
        self.assertFalse(ab('a'))
        self.assertFalse(ab('Aa'))
        self.assertEqual(ab('ab'), Succeed('ab')(''))
        self.assertEqual(ab('aB'), Succeed('aB')(''))
        self.assertEqual(ab('Ab'), Succeed('Ab')(''))
        self.assertEqual(ab('AB'), Succeed('AB')(''))
        self.assertEqual(ab('aBc'), Succeed('aB')('c'))

    def test_collect_okay(self):
        zero = Terminal('0')
        one = Terminal('1')
        bit = zero | one
        p = Collect(bit, bit, bit)
        for a in '01':
            for b in '01':
                for c in '01':
                    bits = a + b + c
                    self.assertEqual(set(p(bits)), {Result((a, b, c), '')})

    def test_collect_nondet(self):
        bit = Terminal('a').repeat(0, 1, take_all=True)
        p = Collect(bit, bit, bit)
        self.assertEqual(len(p('')), 1)     # -/-/-
        self.assertEqual(len(p('a')), 4)    # a/-/-, -/a/-, -/-/a
        self.assertEqual(len(p('aa')), 7)   # a/a/-, a/-/a, -/a/a
        self.assertEqual(len(p('aaa')), 8)  # a/a/a

    def test_collect_fail(self):
        zero = Terminal('0').repeat(1, 2, take_all=True)
        p = Collect(zero, zero, zero)
        rs = p('00a')
        self.assertFalse(rs)

    def test_context_sensitive(self):
        start_tag = Terminal('<').then(Alpha.repeat(1)).skip('>')
        def end_tag(name):
            return Terminal('</').then(Terminal(name)).skip('>')
        tag = start_tag.then(lambda tag: body.skip(end_tag(tag)))
        body = tag | Alnum.repeat()
        self.assertEqual(tag('<b>bold</b>'), Succeed('bold')(''))
        self.assertEqual(tag('<b><i>bi</i></b>'), Succeed('bi')(''))
        self.assertFalse(tag('<b><i>fail</b></i>'))
        self.assertFalse(tag('<b><i>fail</i>'))
        self.assertFalse(tag('<b><i>fail</b>'))

    def test_lookahead_true(self):
        # Taken from https://en.wikipedia.org/wiki/Parsing_expression_grammar.
        B = Forward()
        B.is_(Terminal('b') + B.repeat(0, 1) + 'c')
        A = Forward()
        A.is_(Terminal('a') + A.repeat(0, 1) + 'b')
        S = (Lookahead(A + 'c') + Terminal('a').repeat(1) + B).filter(
                lambda rs: not rs.remain)
        self.assertFalse(S('aabc'))
        self.assertFalse(S('abbc'))
        self.assertFalse(S('abcc'))
        self.assertEqual(S('abc'), Succeed('abc')(''))
        self.assertEqual(S('aabbcc'), Succeed('aabbcc')(''))
        self.assertEqual(S('aaabbbccc'), Succeed('aaabbbccc')(''))
        self.assertFalse(S('aaabbccc'))
        self.assertFalse(S('aaabbbcc'))
        self.assertFalse(S('aabbbccc'))

    def test_lookahead_false(self):
        # Taken from https://en.wikipedia.org/wiki/Parsing_expression_grammar.
        a = Terminal('a')
        b = Terminal('b')
        a_but_not_ab = Lookahead(a + b, take_if=False) + a
        self.assertEqual(a_but_not_ab('a'), Succeed('a')(''))
        self.assertFalse(a_but_not_ab('ab'))
        self.assertEqual(a_but_not_ab('ac'), Succeed('a')('c'))
        self.assertFalse(a_but_not_ab('ba'))

if __name__ == '__main__':
    unittest.main()
