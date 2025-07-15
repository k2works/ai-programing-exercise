import unittest
from fizzbuzz import FizzBuzz


class FizzBuzzTest(unittest.TestCase):
    def setUp(self):
        self.fizzbuzz = FizzBuzz

    def test_1を渡したら文字列1を返す(self):
        self.assertEqual('1', self.fizzbuzz.generate(1))

    def test_2を渡したら文字列2を返す(self):
        self.assertEqual('2', self.fizzbuzz.generate(2))


if __name__ == '__main__':
    unittest.main()
