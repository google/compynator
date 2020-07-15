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

import unittest

from compynator import *


class CoreTest(unittest.TestCase):

    def test_terminal(self):
        true = Terminal('t')
        self.assertEqual(true('t'), Succeed('t')(''))
        self.assertEqual(true('ta'), Succeed('t')('a'))
        self.assertEqual(true('tab'), Succeed('t')('ab'))
        self.assertEqual(true('tba'), Succeed('t')('ba'))

        ab = Terminal('ab')
        self.assertFalse(ab(''))
        self.assertFalse(ab('a'))
        self.assertFalse(ab('aa'))
        self.assertEqual(ab('ab'), Succeed('ab')(''))
        self.assertEqual(ab('abc'), Succeed('ab')('c'))

    def test_name_class(self):
        a = Terminal('a')
        self.assertEqual(a.get_name(), 'Terminal')
        a.set_name('a_new_name')
        self.assertEqual(a.get_name(), 'a_new_name')

    def test_name_func(self):
        @Parser
        def a_parser_func(tokens):
            pass
        self.assertEqual(a_parser_func.get_name(), 'a_parser_func')

    def test_succeed(self):
        s = Succeed(42)
        self.assertEqual(s(''), Succeed(42)(''))
        self.assertEqual(s('a'), Succeed(42)('a'))
        self.assertEqual(s('b'), Succeed(42)('b'))

    def test_then_binder(self):
        true = Terminal('t').then(lambda r: Succeed(True))
        self.assertEqual(true('t'), Succeed(True)(''))
        self.assertEqual(true('ta'), Succeed(True)('a'))
        self.assertEqual(true('tb'), Succeed(True)('b'))
        self.assertFalse(true('a'))

    def test_then_parser(self):
        true = Terminal('t').then(Succeed(True))
        self.assertEqual(true('t'), Succeed(True)(''))
        self.assertEqual(true('ta'), Succeed(True)('a'))
        self.assertEqual(true('tb'), Succeed(True)('b'))
        self.assertFalse(true('a'))

    def test_then_other(self):
        true = Terminal('t').then('a')
        self.assertFalse(true('t'))
        self.assertEqual(true('ta'), Succeed('a')(''))
        self.assertFalse(true('tb'))
        self.assertFalse(true('a'))

    def test_add_parser(self):
        a = Terminal('a')
        b = Terminal('b')
        a_b = a + b
        self.assertFalse(a_b('a'))
        self.assertFalse(a_b('b'))
        self.assertFalse(a_b('ba'))
        self.assertEqual(a_b('ab'), Succeed('ab')(''))

    def test_add_binder(self):
        a = Terminal('a')
        b = Terminal('b')
        a_b = a + (lambda _: b)
        self.assertFalse(a_b('a'))
        self.assertFalse(a_b('b'))
        self.assertFalse(a_b('ba'))
        self.assertEqual(a_b('ab'), Succeed('ab')(''))

    def test_add_other(self):
        a = Terminal('a')
        a_8 = a + 8
        self.assertFalse(a_8('a'))
        self.assertFalse(a_8('b'))
        self.assertFalse(a_8('8a'))
        self.assertEqual(a_8('a8'), Succeed('a8')(''))

    def test_radd(self):
        b = Terminal('b')
        a_b = 'a' + b
        self.assertFalse(a_b('a'))
        self.assertFalse(a_b('b'))
        self.assertFalse(a_b('ba'))
        self.assertEqual(a_b('ab'), Succeed('ab')(''))

    def test_skip_parser(self):
        a = Terminal('a')
        b = Terminal('b')
        a_b = a.skip(b)
        self.assertFalse(a_b('a'))
        self.assertFalse(a_b('b'))
        self.assertFalse(a_b('ba'))
        self.assertEqual(a_b('ab'), Succeed('a')(''))

    def test_skip_lambda(self):
        a = Terminal('a')
        b = Terminal('b')
        a_b = a.skip(lambda _: b)
        self.assertFalse(a_b('a'))
        self.assertFalse(a_b('b'))
        self.assertFalse(a_b('ba'))
        self.assertEqual(a_b('ab'), Succeed('a')(''))

    def test_skip_other(self):
        a = Terminal('a')
        a_b = a.skip('b')
        self.assertFalse(a_b('a'))
        self.assertFalse(a_b('b'))
        self.assertFalse(a_b('ba'))
        self.assertEqual(a_b('ab'), Succeed('a')(''))

    def test_xor_parser(self):
        true = Terminal('t').then(lambda r: Succeed(True))
        false = Terminal('f').then(lambda r: Succeed(False))
        self.assertEqual((true ^ false)('t'), Succeed(True)(''))
        self.assertEqual((true ^ false)('f'), Succeed(False)(''))
        self.assertEqual((false ^ true)('t'), Succeed(True)(''))
        self.assertEqual((false ^ true)('f'), Succeed(False)(''))
        self.assertFalse((true ^ false)('c'))

    def test_xor_other(self):
        true = 't'
        false = Terminal('f')
        self.assertEqual((true ^ false)('t'), Succeed('t')(''))
        self.assertEqual((true ^ false)('f'), Succeed('f')(''))
        self.assertEqual((false ^ true)('t'), Succeed('t')(''))
        self.assertEqual((false ^ true)('f'), Succeed('f')(''))
        self.assertFalse((true ^ false)('c'))

    def test_xor_other_ambiguous(self):
        t = 't'
        tt = Terminal('tt')
        expected = {
            Result('t', 't'),
            Result('tt', ''),
        }
        self.assertEqual(set((t ^ tt)('tt')), expected)

    def test_or_parser(self):
        true = Terminal('t').then(lambda r: Succeed(True))
        false = Terminal('f').then(lambda r: Succeed(False))
        self.assertEqual((true | false)('t'), Succeed(True)(''))
        self.assertEqual((true | false)('f'), Succeed(False)(''))
        self.assertEqual((false | true)('t'), Succeed(True)(''))
        self.assertEqual((false | true)('f'), Succeed(False)(''))
        self.assertFalse((true | false)('c'))

    def test_or_other(self):
        true = 't'
        false = Terminal('f')
        self.assertEqual((true | false)('t'), Succeed('t')(''))
        self.assertEqual((true | false)('f'), Succeed('f')(''))
        self.assertEqual((false | true)('t'), Succeed('t')(''))
        self.assertEqual((false | true)('f'), Succeed('f')(''))
        self.assertFalse((true | false)('c'))

    def test_or_other_unique(self):
        t = 't'
        tt = Terminal('tt')
        self.assertEqual((t | tt)('tt'), Succeed('t')('t'))

    def test_ambiguous_with_xor(self):
        empty = Succeed('')
        s = (Terminal('s') + (lambda x: s) + (lambda x: s)) ^ empty
        expected = {
            Result('', 'ssss'),
            Result('s', 'sss'),
            Result('ss', 'ss'),
            Result('sss', 's'),
            Result('ssss', ''),
        }
        self.assertEqual(set(s('ssss')), expected)

    def test_ambiguous_with_or(self):
        empty = Succeed('')
        s = (Terminal('s') + (lambda x: s) + (lambda x: s)) | empty
        self.assertEqual(s('ssss'), Succeed('ssss')(''))

    def test_repeat(self):
        p = Terminal('a').repeat()
        self.assertEqual(p(''), Succeed('')(''))
        self.assertEqual(p('b'), Succeed('')('b'))
        self.assertEqual(p('a'), Succeed('a')(''))
        self.assertEqual(p('aa'), Succeed('aa')(''))
        self.assertEqual(p('aac'), Succeed('aa')('c'))

    def test_repeat_0(self):
        m0 = Terminal('a').repeat(0)
        self.assertEqual(m0(''), Succeed('')(''))
        self.assertEqual(m0('b'), Succeed('')('b'))
        self.assertEqual(m0('a'), Succeed('a')(''))
        self.assertEqual(m0('aa'), Succeed('aa')(''))
        self.assertEqual(m0('aab'), Succeed('aa')('b'))

    def test_repeat_0_0(self):
        m0_0 = Terminal('a').repeat(0, 0)
        self.assertEqual(m0_0(''), Succeed('')(''))
        self.assertEqual(m0_0('b'), Succeed('')('b'))
        self.assertEqual(m0_0('a'), Succeed('')('a'))
        self.assertEqual(m0_0('aa'), Succeed('')('aa'))

    def test_repeat_0_1(self):
        m0_1 = Terminal('a').repeat(0, 1)
        self.assertEqual(m0_1(''), Succeed('')(''))
        self.assertEqual(m0_1('b'), Succeed('')('b'))
        self.assertEqual(m0_1('a'), Succeed('a')(''))
        self.assertEqual(m0_1('aa'), Succeed('a')('a'))

    def test_repeat_1(self):
        m1 = Terminal('a').repeat(1)
        self.assertFalse(m1(''))
        self.assertFalse(m1('b'))
        self.assertEqual(m1('a'), Succeed('a')(''))
        self.assertEqual(m1('aa'), Succeed('aa')(''))
        self.assertEqual(m1('aab'), Succeed('aa')('b'))

    def test_repeat_1_1(self):
        m1_1 = Terminal('a').repeat(1, 1)
        self.assertFalse(m1_1(''))
        self.assertFalse(m1_1('b'))
        self.assertEqual(m1_1('a'), Succeed('a')(''))
        self.assertEqual(m1_1('aa'), Succeed('a')('a'))

    def test_repeat_0_all(self):
        m0 = Terminal('a').repeat(0, take_all=True)
        self.assertEqual(m0(''), Succeed('')(''))
        self.assertEqual(m0('b'), Succeed('')('b'))
        self.assertEqual(set(m0('a')), {Result('', 'a'), Result('a', '')})
        self.assertEqual(set(m0('aa')),
                         {Result('', 'aa'), Result('a', 'a'), Result('aa', '')})
        self.assertEqual(set(m0('aab')),
                         {Result('', 'aab'), Result('a', 'ab'),
                          Result('aa', 'b')})

    def test_repeat_0_0_all(self):
        m0_0 = Terminal('a').repeat(0, 0, take_all=True)
        self.assertEqual(m0_0(''), Succeed('')(''))
        self.assertEqual(m0_0('b'), Succeed('')('b'))
        self.assertEqual(m0_0('a'), Succeed('')('a'))
        self.assertEqual(m0_0('aa'), Succeed('')('aa'))

    def test_repeat_0_1_all(self):
        m0_1 = Terminal('a').repeat(0, 1, take_all=True)
        self.assertEqual(m0_1(''), Succeed('')(''))
        self.assertEqual(m0_1('b'), Succeed('')('b'))
        self.assertEqual(set(m0_1('a')), {Result('', 'a'), Result('a', '')})
        self.assertEqual(set(m0_1('aa')), {Result('', 'aa'), Result('a', 'a')})

    def test_repeat_1_all(self):
        m1 = Terminal('a').repeat(1, take_all=True)
        self.assertFalse(m1(''))
        self.assertFalse(m1('b'))
        self.assertEqual(m1('a'), Succeed('a')(''))
        self.assertEqual(set(m1('aa')), {Result('a', 'a'), Result('aa', '')})
        self.assertEqual(set(m1('aab')), {Result('a', 'ab'), Result('aa', 'b')})

    def test_repeat_1_1_all(self):
        m1_1 = Terminal('a').repeat(1, 1, take_all=True)
        self.assertFalse(m1_1(''))
        self.assertFalse(m1_1('b'))
        self.assertEqual(m1_1('a'), Succeed('a')(''))
        self.assertEqual(m1_1('aa'), Succeed('a')('a'))

    def test_one(self):
        o = One
        self.assertFalse(o(''))
        self.assertEqual(o('a'), Succeed('a')(''))
        self.assertEqual(o('ab'), Succeed('a')('b'))

    def test_where(self):
        a = One.where(lambda c: 'a' <= c <= 'z')
        self.assertFalse(a(''))
        self.assertEqual(a('a'), Succeed('a')(''))
        self.assertEqual(a('ab'), Succeed('a')('b'))
        self.assertFalse(a('8ab'))

    def test_value_callable(self):
        empty = Succeed('')
        digit = (One.where(lambda c: '0' <= c <= '9') + (
                 lambda x: digit)) | empty
        digit_as_int = digit.value(int)
        self.assertEqual(digit_as_int('1'), Succeed(1)(''))
        self.assertEqual(digit_as_int('1a'), Succeed(1)('a'))
        self.assertEqual(digit_as_int('10'), Succeed(10)(''))
        self.assertEqual(digit_as_int('10b'), Succeed(10)('b'))

    def test_value_object(self):
        t = Terminal('t').value(True)
        self.assertFalse(t('f'))
        self.assertEqual(t('t'), Succeed(True)(''))

    def test_forward_plus(self):
        f = Forward()
        empty = Succeed('')
        f.is_((Terminal('a') + f + f) | empty)
        self.assertEqual(f('aaa'), Succeed('aaa')(''))

    def test_forward_then(self):
        f = Forward()
        empty = Succeed('')
        f.is_(Terminal('a').then(f).then(f) | empty)
        self.assertEqual(f('aaa'), Succeed('')(''))

    def test_memoize(self):
        empty = Succeed('')
        s = ((Terminal('s') + (lambda _: s) + (lambda _: s))
             ^ empty).memoize()
        # Note that it'll take a very long time with 20-char input without
        # memoization.
        self.assertEqual(len(s('s' * 20)), 21)

    def test_auto_memoize(self):
        expr = Forward()
        factor = Alnum | Terminal('(').then(expr).skip(')')
        term = Forward()
        term.is_(
            term.skip('*').then(factor) ^
            term.skip('/').then(factor) ^
            factor
        )
        expr.is_(
            expr.skip('+').then(term) ^
            expr.skip('-').then(term) ^
            term
        )
        r = expr.filter(lambda r: not r.remain)('(1+2*3)/(4+5)')
        self.assertEqual(r, Succeed('5')(''))

    def test_auto_memoize_with_memoized_parsers(self):
        expr = Forward()
        factor = Alnum | Terminal('(').then(expr).skip(')')
        term = Forward()
        term.is_((
            term.skip('*').then(factor) ^
            term.skip('/').then(factor) ^
            factor
        ).memoize())
        expr.is_((
            expr.skip('+').then(term) ^
            expr.skip('-').then(term) ^
            term
        ).memoize())
        r = expr.filter(lambda r: not r.remain)('(1+2*3)/(4+5)')
        self.assertEqual(r, Succeed('5')(''))

    def test_direct_left_recursive(self):
        empty = Succeed('')
        s = Forward()
        s.is_(((s + (lambda _: Terminal('s'))) ^ empty).memoize())
        self.assertEqual(len(s('s' * 20)), 21)

    def test_indirect_left_recursive(self):
        empty = Succeed('')
        s1 = Forward()
        s2 = (s1 + (lambda _: Terminal('s'))) ^ empty
        s1.is_(((s2 + (lambda _: Terminal('s'))) ^ empty).memoize())
        self.assertEqual(len(s1('s' * 20)), 21)

    def test_max_recursion(self):
        empty = Succeed('')
        s = ((Terminal('s') + (lambda _: s) + (lambda _: s))
             ^ empty).memoize()
        context = ParseContext(ParseOptions(max_recursion=0))
        self.assertEqual(len(s.parse_with_context('s' * 20, context)), 0)
        context = ParseContext(ParseOptions(max_recursion=1))
        self.assertEqual(len(s.parse_with_context('s' * 20, context)), 21)

    def test_failure(self):
        a = Terminal('a')
        f2 = Failure(a, 'Level 2', 'bc')
        f1 = Failure(a, 'Level 1', 'abc', f2)
        f0 = Failure(a, 'Top level', 'abc', f1)
        self.assertTrue('Level 2' in f2.describe())
        self.assertFalse('Caused by' in f2.describe())
        self.assertTrue('Level 2' in f1.describe())
        self.assertTrue('Level 1' in f1.describe())
        self.assertTrue('Caused by' in f1.describe())
        # Top level is included because we are calling it.
        self.assertTrue('Top level' in f0.describe())
        # Level 1 is included if we set include_unnamed=True.
        self.assertTrue('Level 1' in f0.describe(include_unnamed=True))
        # But normally Level 1 is not included because it's not the root cause.
        self.assertFalse('Level 1' in f0.describe())
        # Level 2 is included because it is the root cause.
        self.assertTrue('Level 2' in f0.describe())

        a.set_name('Woot')
        # Level 1 should be included now because its parser is named.
        self.assertTrue('Level 1' in f0.describe())

    def test_boolean_expression(self):
        # See https://leetcode.com/problems/parsing-a-boolean-expression.
        t = Terminal('t').value(True)
        f = Terminal('f').value(False)
        e = Forward()
        n = Terminal('!(').then(e).value(lambda x: not x).skip(')')
        a_empty = Succeed(True)
        a_e_tail = Forward()
        a_e_tail.is_(
            Terminal(',').then(e).then(a_e_tail, lambda x, y: x and y)
            | a_empty)
        a = Terminal('&(').then(e).then(
            a_e_tail, lambda x, y: x and y).skip(')')
        o_empty = Succeed(False)
        o_e_tail = Forward()
        o_e_tail.is_(
            Terminal(',').then(e).then(o_e_tail, lambda x, y: x or y)
            | o_empty)
        o = Terminal('|(').then(e).then(
            o_e_tail, lambda x, y: x or y).skip(')')
        e.is_(t | f | n | a | o)
        self.assertEqual(e('t'), Succeed(True)(''))
        self.assertEqual(e('f'), Succeed(False)(''))
        self.assertEqual(e('!(t)'), Succeed(False)(''))
        self.assertEqual(e('!(f)'), Succeed(True)(''))
        self.assertEqual(e('!(!(t))'), Succeed(True)(''))
        self.assertEqual(e('!(!(f))'), Succeed(False)(''))
        self.assertEqual(e('&(t)'), Succeed(True)(''))
        self.assertEqual(e('&(f)'), Succeed(False)(''))
        self.assertEqual(e('&(t,t)'), Succeed(True)(''))
        self.assertEqual(e('&(t,t,t)'), Succeed(True)(''))
        self.assertEqual(e('&(f,t,t)'), Succeed(False)(''))
        self.assertEqual(e('&(t,f,t)'), Succeed(False)(''))
        self.assertEqual(e('&(t,t,f)'), Succeed(False)(''))
        self.assertEqual(e('&(t,t,!(t))'), Succeed(False)(''))
        self.assertEqual(e('|(f)'), Succeed(False)(''))
        self.assertEqual(e('|(t)'), Succeed(True)(''))
        self.assertEqual(e('|(t,f)'), Succeed(True)(''))
        self.assertEqual(e('|(t,f,f)'), Succeed(True)(''))
        self.assertEqual(e('|(f,f,f)'), Succeed(False)(''))
        self.assertEqual(e('|(t,f,t)'), Succeed(True)(''))
        self.assertEqual(e('|(f,f,t)'), Succeed(True)(''))
        self.assertEqual(e('|(f,f,!(t))'), Succeed(False)(''))
        # LeetCode test cases.
        self.assertEqual(e('!(f)'), Succeed(True)(''))
        self.assertEqual(e('|(f,t)'), Succeed(True)(''))
        self.assertEqual(e('&(t,f)'), Succeed(False)(''))
        self.assertEqual(e('|(&(t,f,t),!(t))'), Succeed(False)(''))

    def test_non_string(self):
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
        def do_op(left_and_op, right):
            left, op = left_and_op
            return op(left, right)
        term = Forward()
        term.is_((
            term.then(mult_div, lambda l, o: (l, o)).then(factor, do_op) ^
            factor
        ).memoize())
        expr.is_((
            expr.then(add_sub, lambda l, o: (l, o)).then(term, do_op) ^
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


if __name__ == '__main__':
    unittest.main()
