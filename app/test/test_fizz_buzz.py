"""Test module for FizzBuzz class."""

import io
import sys

from lib.fizz_buzz import FizzBuzz


class TestFizzBuzz:
    """Test class for FizzBuzz."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.fizzbuzz = FizzBuzz(1)  # デフォルトでタイプ1

    def test_3を渡したら文字列Fizzを返す(self) -> None:
        """Test that 3 returns 'Fizz'."""
        assert self.fizzbuzz.generate(3) == "Fizz"

    def test_5を渡したら文字列Buzzを返す(self) -> None:
        """Test that 5 returns 'Buzz'."""
        assert self.fizzbuzz.generate(5) == "Buzz"

    def test_15を渡したら文字列FizzBuzzを返す(self) -> None:
        """Test that 15 returns 'FizzBuzz'."""
        assert self.fizzbuzz.generate(15) == "FizzBuzz"

    def test_1を渡したら文字列1を返す(self) -> None:
        """Test that 1 returns '1'."""
        assert self.fizzbuzz.generate(1) == "1"

    def test_三の倍数の場合にFizzを返す(self) -> None:
        """Test that multiples of 3 return 'Fizz'."""
        assert self.fizzbuzz.generate(6) == "Fizz"
        assert self.fizzbuzz.generate(9) == "Fizz"
        assert self.fizzbuzz.generate(12) == "Fizz"

    def test_五の倍数の場合にBuzzを返す(self) -> None:
        """Test that multiples of 5 return 'Buzz'."""
        assert self.fizzbuzz.generate(10) == "Buzz"
        assert self.fizzbuzz.generate(20) == "Buzz"
        assert self.fizzbuzz.generate(25) == "Buzz"

    def test_十五の倍数の場合にFizzBuzzを返す(self) -> None:
        """Test that multiples of 15 return 'FizzBuzz'."""
        assert self.fizzbuzz.generate(30) == "FizzBuzz"
        assert self.fizzbuzz.generate(45) == "FizzBuzz"
        assert self.fizzbuzz.generate(60) == "FizzBuzz"

    def test_三と五の倍数ではない場合に数字の文字列を返す(self) -> None:
        """Test that non-multiples of 3 and 5 return the number as string."""
        assert self.fizzbuzz.generate(1) == "1"
        assert self.fizzbuzz.generate(2) == "2"
        assert self.fizzbuzz.generate(4) == "4"
        assert self.fizzbuzz.generate(7) == "7"
        assert self.fizzbuzz.generate(8) == "8"

    def test_1から100までのFizzBuzz配列を返す(self) -> None:
        """Test that generate_list returns 100 elements."""
        result = self.fizzbuzz.generate_list()
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
        fizzbuzz_list = self.fizzbuzz.generate_list()
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
        fizzbuzz_type1 = FizzBuzz(1)
        assert fizzbuzz_type1.generate(1) == "1"
        
        # タイプ2の場合：数を文字列にして返す（その他の場合）
        fizzbuzz_type2 = FizzBuzz(2)
        assert fizzbuzz_type2.generate(1) == "1"
        
    def test_タイプ2_三の倍数の場合(self) -> None:
        """Test type 2 for multiples of 3."""
        fizzbuzz_type2 = FizzBuzz(2)
        assert fizzbuzz_type2.generate(3) == "3"
        
    def test_タイプ2_五の倍数の場合(self) -> None:
        """Test type 2 for multiples of 5."""
        fizzbuzz_type2 = FizzBuzz(2)
        assert fizzbuzz_type2.generate(5) == "5"
        
    def test_タイプ2_三と五の倍数の場合(self) -> None:
        """Test type 2 for multiples of both 3 and 5."""
        fizzbuzz_type2 = FizzBuzz(2)
        assert fizzbuzz_type2.generate(15) == "15"
        
    def test_タイプ3_三の倍数の場合(self) -> None:
        """Test type 3 for multiples of 3."""
        fizzbuzz_type3 = FizzBuzz(3)
        assert fizzbuzz_type3.generate(3) == "3"
        
    def test_タイプ3_五の倍数の場合(self) -> None:
        """Test type 3 for multiples of 5."""
        fizzbuzz_type3 = FizzBuzz(3)
        assert fizzbuzz_type3.generate(5) == "5"
        
    def test_タイプ3_三と五の倍数の場合(self) -> None:
        """Test type 3 for multiples of both 3 and 5."""
        fizzbuzz_type3 = FizzBuzz(3)
        assert fizzbuzz_type3.generate(15) == "FizzBuzz"
        
    def test_タイプ3_その他の場合(self) -> None:
        """Test type 3 for other cases."""
        fizzbuzz_type3 = FizzBuzz(3)
        assert fizzbuzz_type3.generate(1) == "1"
        
    def test_それ以外のタイプの場合_例外を返す(self) -> None:
        """Test exception for unsupported types."""
        import pytest
        with pytest.raises(RuntimeError, match="該当するタイプは存在しません"):
            FizzBuzz(4)


class TestArrayAndIteration:
    """Test class for learning array and iteration methods."""

    def test_繰り返し処理(self) -> None:
        """Test iteration processing."""
        # Capture stdout
        captured_output = io.StringIO()
        sys.stdout = captured_output

        for i in [1, 2, 3]:
            print(i * i)

        # Reset stdout
        sys.stdout = sys.__stdout__

        output = captured_output.getvalue()
        assert output == "1\n4\n9\n"

    def test_selectメソッドで特定の条件を満たす要素だけを配列に入れて返す(self) -> None:
        """Test filtering elements with specific condition using list comprehension."""
        numbers = [1.1, 2, 3.3, 4]
        result = [x for x in numbers if isinstance(x, int)]
        assert result == [2, 4]

    def test_find_allメソッドで特定の条件を満たす要素だけを配列に入れて返す(
        self,
    ) -> None:
        """Test filtering elements with specific condition using filter."""
        numbers = [1.1, 2, 3.3, 4]
        result = list(filter(lambda x: isinstance(x, int), numbers))
        assert result == [2, 4]

    def test_特定の条件を満たさない要素だけを配列に入れて返す(self) -> None:
        """Test filtering elements that don't meet condition."""
        numbers = [1.1, 2, 3.3, 4]
        result = [x for x in numbers if not isinstance(x, int)]
        assert result == [1.1, 3.3]

    def test_mapメソッドで新しい要素の配列を返す(self) -> None:
        """Test mapping elements to new values using list comprehension."""
        words = ["apple", "orange", "pineapple", "strawberry"]
        result = [len(word) for word in words]
        assert result == [5, 6, 9, 10]

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
