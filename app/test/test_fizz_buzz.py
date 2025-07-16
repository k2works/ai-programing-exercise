"""Test module for FizzBuzz class."""

import io
import sys

from lib.fizz_buzz import FizzBuzz


class TestFizzBuzz:
    """Test class for FizzBuzz."""

    def test_3を渡したら文字列Fizzを返す(self) -> None:
        """Test that 3 returns 'Fizz'."""
        assert FizzBuzz.generate(3, 1) == "Fizz"

    def test_5を渡したら文字列Buzzを返す(self) -> None:
        """Test that 5 returns 'Buzz'."""
        assert FizzBuzz.generate(5, 1) == "Buzz"

    def test_15を渡したら文字列FizzBuzzを返す(self) -> None:
        """Test that 15 returns 'FizzBuzz'."""
        assert FizzBuzz.generate(15, 1) == "FizzBuzz"

    def test_1を渡したら文字列1を返す(self) -> None:
        """Test that 1 returns '1'."""
        assert FizzBuzz.generate(1, 1) == "1"

    def test_三の倍数の場合にFizzを返す(self) -> None:
        """Test that multiples of 3 return 'Fizz'."""
        assert FizzBuzz.generate(6, 1) == "Fizz"
        assert FizzBuzz.generate(9, 1) == "Fizz"
        assert FizzBuzz.generate(12, 1) == "Fizz"

    def test_五の倍数の場合にBuzzを返す(self) -> None:
        """Test that multiples of 5 return 'Buzz'."""
        assert FizzBuzz.generate(10, 1) == "Buzz"
        assert FizzBuzz.generate(20, 1) == "Buzz"
        assert FizzBuzz.generate(25, 1) == "Buzz"

    def test_十五の倍数の場合にFizzBuzzを返す(self) -> None:
        """Test that multiples of 15 return 'FizzBuzz'."""
        assert FizzBuzz.generate(30, 1) == "FizzBuzz"
        assert FizzBuzz.generate(45, 1) == "FizzBuzz"
        assert FizzBuzz.generate(60, 1) == "FizzBuzz"

    def test_三と五の倍数ではない場合に数字の文字列を返す(self) -> None:
        """Test that non-multiples of 3 and 5 return the number as string."""
        assert FizzBuzz.generate(1, 1) == "1"
        assert FizzBuzz.generate(2, 1) == "2"
        assert FizzBuzz.generate(4, 1) == "4"
        assert FizzBuzz.generate(7, 1) == "7"
        assert FizzBuzz.generate(8, 1) == "8"

    def test_1から100までのFizzBuzz配列を返す(self) -> None:
        """Test that generate_list returns 100 elements."""
        result = FizzBuzz.generate_list(1)
        assert len(result) == 100
        assert result[0] == "1"
        assert result[2] == "Fizz"
        assert result[4] == "Buzz"
        assert result[14] == "FizzBuzz"

    def test_配列や繰り返し処理を理解する(self) -> None:
        """Test understanding of array and iteration processing."""
        # プリント出力をキャプチャ
        captured_output = io.StringIO()
        sys.stdout = captured_output

        # FizzBuzzリストを出力
        fizzbuzz_list = FizzBuzz.generate_list(1)
        for item in fizzbuzz_list:
            print(item)

        # 標準出力を復元
        sys.stdout = sys.__stdout__

        # 出力結果を取得
        output = captured_output.getvalue()

        # 最初の15個をチェック
        lines = output.strip().split("\n")
        expected = [
            "1",
            "2",
            "Fizz",
            "4",
            "Buzz",
            "Fizz",
            "7",
            "8",
            "Fizz",
            "Buzz",
            "11",
            "Fizz",
            "13",
            "14",
            "FizzBuzz",
        ]
        for i, expected_value in enumerate(expected):
            assert lines[i] == expected_value

    def test_タイプごとに出力を切り替えることができる(self) -> None:
        """Test that output can be switched by type."""
        # タイプ1の場合：数を文字列にして返す
        assert FizzBuzz.generate(1, 1) == "1"
        
        # タイプ2の場合：数を文字列にして返す（その他の場合）
        assert FizzBuzz.generate(1, 2) == "1"
        
    def test_タイプ2_三の倍数の場合(self) -> None:
        """Test type 2 for multiples of 3."""
        assert FizzBuzz.generate(3, 2) == "3"
        
    def test_タイプ2_五の倍数の場合(self) -> None:
        """Test type 2 for multiples of 5."""
        assert FizzBuzz.generate(5, 2) == "5"
        
    def test_タイプ2_三と五の倍数の場合(self) -> None:
        """Test type 2 for multiples of both 3 and 5."""
        assert FizzBuzz.generate(15, 2) == "15"
        
    def test_タイプ3_三の倍数の場合(self) -> None:
        """Test type 3 for multiples of 3."""
        assert FizzBuzz.generate(3, 3) == "3"
        
    def test_タイプ3_五の倍数の場合(self) -> None:
        """Test type 3 for multiples of 5."""
        assert FizzBuzz.generate(5, 3) == "5"
        
    def test_タイプ3_三と五の倍数の場合(self) -> None:
        """Test type 3 for multiples of both 3 and 5."""
        assert FizzBuzz.generate(15, 3) == "FizzBuzz"
        
    def test_タイプ3_その他の場合(self) -> None:
        """Test type 3 for other cases."""
        assert FizzBuzz.generate(1, 3) == "1"
        
    def test_それ以外のタイプの場合_例外を返す(self) -> None:
        """Test exception for unsupported types."""
        import pytest
        with pytest.raises(RuntimeError, match="該当するタイプは存在しません"):
            FizzBuzz.generate(1, 4)

    def test_collectメソッドで新しい要素の配列を返す(self) -> None:
        """Test mapping elements to new values using map."""
        words = ["apple", "orange", "pineapple", "strawberry"]
        result = list(map(len, words))
        assert result == [5, 6, 9, 10]

    def test_findメソッドで配列の中から条件に一致する要素を取得する(self) -> None:
        """Test finding first element that matches condition."""
        words = ["apple", "orange", "pineapple", "strawberry"]
        result = next((word for word in words if len(word) > 0), None)
        assert result == "apple"

    def test_detectメソッドで配列の中から条件に一致する要素を取得する(self) -> None:
        """Test detecting first element that matches condition."""
        words = ["apple", "orange", "pineapple", "strawberry"]
        result = next(filter(lambda word: len(word) > 0, words), None)
        assert result == "apple"

    def test_指定した評価式で並び変えた配列を返す(self) -> None:
        """Test sorting array with specified expressions."""
        numbers = ["2", "4", "13", "3", "1", "10"]

        result1 = sorted(numbers)
        result2 = sorted(numbers, key=int)
        result3 = sorted(numbers, key=int, reverse=True)

        assert result1 == ["1", "10", "13", "2", "3", "4"]
        assert result2 == ["1", "2", "3", "4", "10", "13"]
        assert result3 == ["13", "10", "4", "3", "2", "1"]

    def test_配列の中から条件に一致する要素を取得する(self) -> None:
        """Test getting elements that match pattern using regex-like functionality."""
        words = ["apple", "orange", "pineapple", "strawberry", "apricot"]
        result = [word for word in words if word.startswith("a")]
        assert result == ["apple", "apricot"]

    def test_ブロック内の条件式が真である間までの要素を返す(self) -> None:
        """Test taking elements while condition is true."""
        numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        result = []
        for item in numbers:
            if item < 6:
                result.append(item)
            else:
                break
        assert result == [1, 2, 3, 4, 5]

    def test_ブロック内の条件式が真である以降の要素を返す(self) -> None:
        """Test dropping elements while condition is true."""
        numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        found_index = 0
        for i, item in enumerate(numbers):
            if item >= 6:
                found_index = i
                break
        result = numbers[found_index:]
        assert result == [6, 7, 8, 9, 10]

    def test_injectメソッドで畳み込み演算を行う(self) -> None:
        """Test fold/reduce operation with initial value."""
        from functools import reduce

        numbers = [1, 2, 3, 4, 5]
        result = reduce(lambda total, n: total + n, numbers, 0)
        assert result == 15

    def test_reduceメソッドで畳み込み演算を行う(self) -> None:
        """Test reduce operation without initial value."""
        from functools import reduce

        numbers = [1, 2, 3, 4, 5]
        result = reduce(lambda total, n: total + n, numbers)
        assert result == 15
