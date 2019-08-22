import doctest
import unittest

from compynator import core, niceties


class DocstringTest(unittest.TestCase):

    def test_docstring(self):
        doctest.testmod(core)
        doctest.testmod(niceties)


if __name__ == '__main__':
    unittest.main()
