"""Test module for FizzBuzz class."""

import io
import sys

from lib.fizz_buzz import FizzBuzz


class TestFizzBuzz:
    """Test class for FizzBuzz."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.fizzbuzz = FizzBuzz(1)
        self.fizzbuzz = FizzBuzz(1)

    def test_3を渡したら文字列Fizzを返す(self) -> None:
        """Test that 3 returns 'Fizz'."""
        assert self.fizzbuzz.generate(3).value == "Fizz"

    def test_5を渡したら文字列Buzzを返す(self) -> None:
        """Test that 5 returns 'Buzz'."""
        assert self.fizzbuzz.generate(5).value == "Buzz"

    def test_15を渡したら文字列FizzBuzzを返す(self) -> None:
        """Test that 15 returns 'FizzBuzz'."""
        assert self.fizzbuzz.generate(15).value == "FizzBuzz"

    def test_1を渡したら文字列1を返す(self) -> None:
        """Test that 1 returns '1'."""
        assert self.fizzbuzz.generate(1).value == "1"

    def test_三の倍数の場合にFizzを返す(self) -> None:
        """Test that multiples of 3 return 'Fizz'."""
        assert self.fizzbuzz.generate(6).value == "Fizz"
        assert self.fizzbuzz.generate(9).value == "Fizz"
        assert self.fizzbuzz.generate(12).value == "Fizz"

    def test_五の倍数の場合にBuzzを返す(self) -> None:
        """Test that multiples of 5 return 'Buzz'."""
        assert self.fizzbuzz.generate(10).value == "Buzz"
        assert self.fizzbuzz.generate(20).value == "Buzz"
        assert self.fizzbuzz.generate(25).value == "Buzz"

    def test_十五の倍数の場合にFizzBuzzを返す(self) -> None:
        """Test that multiples of 15 return 'FizzBuzz'."""
        assert self.fizzbuzz.generate(30).value == "FizzBuzz"
        assert self.fizzbuzz.generate(45).value == "FizzBuzz"
        assert self.fizzbuzz.generate(60).value == "FizzBuzz"

    def test_三と五の倍数ではない場合に数字の文字列を返す(self) -> None:
        """Test that non-multiples of 3 and 5 return the number as string."""
        assert self.fizzbuzz.generate(1).value == "1"
        assert self.fizzbuzz.generate(2).value == "2"
        assert self.fizzbuzz.generate(4).value == "4"
        assert self.fizzbuzz.generate(7).value == "7"
        assert self.fizzbuzz.generate(8).value == "8"

    def test_1から100までのFizzBuzz配列を返す(self) -> None:
        """Test that generate_list returns 100 elements."""
        fizzbuzz = FizzBuzz()
        fizzbuzz.generate_list()
        result = fizzbuzz.fizz_buzz_list
        assert len(result) == 100
        assert result[0].value == "1"
        assert result[2].value == "Fizz"
        assert result[4].value == "Buzz"
        assert result[14].value == "FizzBuzz"

    def test_配列や繰り返し処理を理解する(self) -> None:
        """Test understanding of array and iteration processing."""
        # プリント出力をキャプチャ
        captured_output = io.StringIO()
        sys.stdout = captured_output

        # FizzBuzzリストを出力
        fizzbuzz = FizzBuzz()
        fizzbuzz_list = fizzbuzz.generate_list()
        for item in fizzbuzz_list:
            print(item.value)

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
        assert self.fizzbuzz.generate(1).value == "1"

        # タイプ2の場合：数を文字列にして返す（その他の場合）
        fizzbuzz_type2 = FizzBuzz(2)
        assert fizzbuzz_type2.generate(1).value == "1"

    def test_タイプ2_三の倍数の場合(self) -> None:
        """Test type 2 for multiples of 3."""
        fizzbuzz_type2 = FizzBuzz(2)
        assert fizzbuzz_type2.generate(3).value == "3"

    def test_タイプ2_五の倍数の場合(self) -> None:
        """Test type 2 for multiples of 5."""
        fizzbuzz_type2 = FizzBuzz(2)
        assert fizzbuzz_type2.generate(5).value == "5"

    def test_タイプ2_三と五の倍数の場合(self) -> None:
        """Test type 2 for multiples of both 3 and 5."""
        fizzbuzz_type2 = FizzBuzz(2)
        assert fizzbuzz_type2.generate(15).value == "15"

    def test_タイプ3_三の倍数の場合(self) -> None:
        """Test type 3 for multiples of 3."""
        fizzbuzz_type3 = FizzBuzz(3)
        assert fizzbuzz_type3.generate(3).value == "3"

    def test_タイプ3_五の倍数の場合(self) -> None:
        """Test type 3 for multiples of 5."""
        fizzbuzz_type3 = FizzBuzz(3)
        assert fizzbuzz_type3.generate(5).value == "5"

    def test_タイプ3_三と五の倍数の場合(self) -> None:
        """Test type 3 for multiples of both 3 and 5."""
        fizzbuzz_type3 = FizzBuzz(3)
        assert fizzbuzz_type3.generate(15).value == "FizzBuzz"

    def test_タイプ3_その他の場合(self) -> None:
        """Test type 3 for other cases."""
        fizzbuzz_type3 = FizzBuzz(3)
        assert fizzbuzz_type3.generate(1).value == "1"

    def test_それ以外のタイプの場合_未定義のタイプを返す(self) -> None:
        """Test that other types return an undefined type."""
        from lib.fizz_buzz import FizzBuzzType

        fizzbuzz_type = FizzBuzzType.create(4)
        assert str(fizzbuzz_type) == "未定義"

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
        """Test reduce method for folding operations."""
        result = [fizzbuzz_value.value for fizzbuzz_value in FizzBuzz().generate_list()]

        # Pythonでreduceを使った例（合計）
        from functools import reduce

        # 数値に変換可能な文字列のみ取得して合計
        numbers = [
            int(val) for val in result[:10] if val not in ["Fizz", "Buzz", "FizzBuzz"]
        ]
        sum_result = reduce(lambda acc, x: acc + x, numbers, 0)
        assert sum_result == 22  # 1+2+4+7+8 = 22

    def test_FizzBuzzValue_同じ値である(self) -> None:
        """Test that FizzBuzzValue equality works correctly."""
        fizzbuzz = FizzBuzz(1)
        value1 = fizzbuzz.generate(1)
        value2 = fizzbuzz.generate(1)

        assert value1 == value2

    def test_FizzBuzzValue_to_stringメソッド(self) -> None:
        """Test FizzBuzzValue string representation."""
        fizzbuzz = FizzBuzz(1)
        value = fizzbuzz.generate(3)

        assert str(value) == "3:Fizz"

    def test_FizzBuzzList_新しいインスタンスが作られる(self) -> None:
        """Test that FizzBuzzList creates new instances when adding."""
        from lib.fizz_buzz import FizzBuzzList, FizzBuzzValue

        # 空のリストを作成
        list1 = FizzBuzzList([])

        # 値を追加して新しいリストを作成
        values = [FizzBuzzValue(1, "1"), FizzBuzzValue(2, "2")]
        list2 = list1.add(values)

        # 異なるインスタンスであることを確認
        assert list1 is not list2
        assert len(list1.value) == 0
        assert len(list2.value) == 2

    def test_FizzBuzzList_イミュータブルである(self) -> None:
        """Test that FizzBuzzList is immutable."""
        from lib.fizz_buzz import FizzBuzzList, FizzBuzzValue

        values = [FizzBuzzValue(1, "1"), FizzBuzzValue(2, "2")]
        fizz_buzz_list = FizzBuzzList(values)

        # 元の配列を変更しても影響を受けない
        values.append(FizzBuzzValue(3, "Fizz"))
        assert len(fizz_buzz_list.value) == 2

        # valueプロパティから取得した配列を変更しても影響を受けない
        retrieved_list = fizz_buzz_list.value
        retrieved_list.append(FizzBuzzValue(4, "4"))
        assert len(fizz_buzz_list.value) == 2

    def test_FizzBuzzValueCommand_実行(self) -> None:
        """Test FizzBuzzValueCommand execution."""
        from lib.fizz_buzz import FizzBuzzType01, FizzBuzzValueCommand

        command = FizzBuzzValueCommand(FizzBuzzType01())
        assert command.execute(3) == "Fizz"
        assert command.execute(5) == "Buzz"
        assert command.execute(15) == "FizzBuzz"
        assert command.execute(1) == "1"

    def test_FizzBuzzListCommand_実行(self) -> None:
        """Test FizzBuzzListCommand execution."""
        from lib.fizz_buzz import FizzBuzzListCommand, FizzBuzzType01

        command = FizzBuzzListCommand(FizzBuzzType01())
        result = command.execute(15)
        assert len(result) == 15
        assert result[0].value == "1"
        assert result[2].value == "Fizz"
        assert result[4].value == "Buzz"
        assert result[14].value == "FizzBuzz"


class TestFizzBuzzExceptionCases:
    """Test class for FizzBuzz exception cases."""

    def test_値は正の値のみ許可する(self) -> None:
        """Test that only positive values are allowed."""
        import pytest

        from lib.fizz_buzz import FizzBuzzType01, FizzBuzzValueCommand

        command = FizzBuzzValueCommand(FizzBuzzType01())
        with pytest.raises(ValueError, match="正の値のみ有効です"):
            command.execute(-1)

    def test_上限は100件までです(self) -> None:
        """Test that the upper limit is 100 items."""
        import pytest

        from lib.fizz_buzz import FizzBuzzList, FizzBuzzValue

        values = [FizzBuzzValue(i, str(i)) for i in range(1, 102)]  # 101個の値
        with pytest.raises(ValueError, match="上限は100件までです"):
            FizzBuzzList(values)
