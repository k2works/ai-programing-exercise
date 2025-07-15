import unittest
import io
import sys
from fizzbuzz import FizzBuzz


class FizzBuzzTest(unittest.TestCase):
    def setUp(self):
        self.fizzbuzz = FizzBuzz

    def test_1を渡したら文字列1を返す(self):
        self.assertEqual('1', self.fizzbuzz.generate(1))

    def test_2を渡したら文字列2を返す(self):
        self.assertEqual('2', self.fizzbuzz.generate(2))

    def test_3を渡したら文字列Fizzを返す(self):
        self.assertEqual('Fizz', self.fizzbuzz.generate(3))

    def test_5を渡したら文字列Buzzを返す(self):
        self.assertEqual('Buzz', self.fizzbuzz.generate(5))

    def test_15を渡したら文字列FizzBuzzを返す(self):
        self.assertEqual('FizzBuzz', self.fizzbuzz.generate(15))

    def test_配列の初めは文字列の1を返す(self):
        result = self.fizzbuzz.print_1_to_100()
        self.assertEqual('1', result[0])

    def test_配列の最後は文字列の100を返す(self):
        result = self.fizzbuzz.print_1_to_100()
        self.assertEqual('Buzz', result[-1])

    def test_プリントする(self):
        # 標準出力をキャプチャするためのセットアップ
        captured_output = io.StringIO()
        sys.stdout = captured_output
        
        # FizzBuzzのプリント機能を実行
        self.fizzbuzz.print_fizzbuzz()
        
        # 標準出力を元に戻す
        sys.stdout = sys.__stdout__
        
        # 出力された内容を取得
        output = captured_output.getvalue()
        
        # 最初の行と最後の行をチェック
        lines = output.strip().split('\n')
        self.assertEqual('1', lines[0])
        self.assertEqual('Buzz', lines[-1])
        self.assertEqual(100, len(lines))


if __name__ == '__main__':
    unittest.main()
