"""Test cases for FizzBuzz implementation."""

import sys
from io import StringIO

from src.fizzbuzz import FizzBuzz


class TestFizzBuzz:
    """Test class for FizzBuzz functionality."""

    def setup_method(self) -> None:
        """Set up test fixtures before each test method."""
        self.fizzbuzz = FizzBuzz

    class TestMultiplesOfThree:
        """Test cases for multiples of three."""

        def setup_method(self) -> None:
            """Set up test fixtures."""
            self.fizzbuzz = FizzBuzz

        def test_3を渡したら文字列fizzを返す(self) -> None:
            """Test that 3 returns 'Fizz'."""
            assert self.fizzbuzz.generate(3) == "Fizz"

    class TestMultiplesOfFive:
        """Test cases for multiples of five."""

        def setup_method(self) -> None:
            """Set up test fixtures."""
            self.fizzbuzz = FizzBuzz

        def test_5を渡したら文字列buzzを返す(self) -> None:
            """Test that 5 returns 'Buzz'."""
            assert self.fizzbuzz.generate(5) == "Buzz"

    class TestMultiplesOfThreeAndFive:
        """Test cases for multiples of both three and five."""

        def setup_method(self) -> None:
            """Set up test fixtures."""
            self.fizzbuzz = FizzBuzz

        def test_15を渡したら文字列fizzbuzzを返す(self) -> None:
            """Test that 15 returns 'FizzBuzz'."""
            assert self.fizzbuzz.generate(15) == "FizzBuzz"

    class TestOtherNumbers:
        """Test cases for numbers that are not multiples of 3 or 5."""

        def setup_method(self) -> None:
            """Set up test fixtures."""
            self.fizzbuzz = FizzBuzz

        def test_1を渡したら文字列1を返す(self) -> None:
            """Test that 1 returns '1'."""
            assert self.fizzbuzz.generate(1) == "1"

    class TestGenerateList:
        """Test cases for generate_list method."""

        def setup_method(self) -> None:
            """Set up test fixtures."""
            self.fizzbuzz = FizzBuzz

        def test_1から100までのfizzbuzz配列を返す(self) -> None:
            """Test that generate_list returns correct FizzBuzz array."""
            result = self.fizzbuzz.generate_list()
            assert len(result) == 100
            assert result[0] == "1"  # 1
            assert result[2] == "Fizz"  # 3
            assert result[4] == "Buzz"  # 5
            assert result[14] == "FizzBuzz"  # 15


class TestArrayAndIteration:
    """Test cases for understanding arrays and iteration processing."""

    def test_繰り返し処理(self) -> None:
        """Test iteration processing."""
        # Capture stdout
        old_stdout = sys.stdout
        sys.stdout = captured_output = StringIO()

        # Execute the loop
        for i in [1, 2, 3]:
            print(i * i)

        # Restore stdout
        sys.stdout = old_stdout
        output = captured_output.getvalue()

        expected = "1\n4\n9\n"
        assert output == expected

    def test_selectで特定の条件を満たす要素だけを配列に入れて返す(self) -> None:
        """Test filtering elements that meet specific conditions."""
        numbers = [1.1, 2, 3.3, 4]
        result = [x for x in numbers if isinstance(x, int)]
        assert result == [2, 4]

    def test_特定の条件を満たさない要素だけを配列に入れて返す(self) -> None:
        """Test filtering elements that don't meet specific conditions."""
        numbers = [1.1, 2, 3.3, 4]
        result = [x for x in numbers if not isinstance(x, int)]
        assert result == [1.1, 3.3]

    def test_mapで新しい要素の配列を返す(self) -> None:
        """Test creating new array with map operation."""
        words = ["apple", "orange", "pineapple", "strawberry"]
        result = [len(word) for word in words]
        assert result == [5, 6, 9, 10]

    def test_findで配列の中から条件に一致する要素を取得する(self) -> None:
        """Test finding first element that matches condition."""
        words = ["apple", "orange", "pineapple", "strawberry"]
        result = next((word for word in words if len(word) > 0), None)
        assert result == "apple"

    def test_指定した評価式で並び変えた配列を返す(self) -> None:
        """Test sorting array with specified evaluation expression."""
        numbers = ["2", "4", "13", "3", "1", "10"]

        # Default string sort
        result1 = sorted(numbers)
        # Sort by integer value
        result2 = sorted(numbers, key=int)
        # Sort by integer value in reverse
        result3 = sorted(numbers, key=int, reverse=True)

        assert result1 == ["1", "10", "13", "2", "3", "4"]
        assert result2 == ["1", "2", "3", "4", "10", "13"]
        assert result3 == ["13", "10", "4", "3", "2", "1"]

    def test_grepで配列の中から条件に一致する要素を取得する(self) -> None:
        """Test finding elements that match regex pattern."""
        import re

        words = ["apple", "orange", "pineapple", "strawberry", "apricot"]
        pattern = re.compile(r"^a")
        result = [word for word in words if pattern.match(word)]
        assert result == ["apple", "apricot"]

    def test_take_whileでブロック内の条件式が真である間までの要素を返す(self) -> None:
        """Test taking elements while condition is true."""
        from itertools import takewhile

        numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        result = list(takewhile(lambda x: x < 6, numbers))
        assert result == [1, 2, 3, 4, 5]

    def test_drop_whileでブロック内の条件式が真である以降の要素を返す(self) -> None:
        """Test dropping elements while condition is true."""
        from itertools import dropwhile

        numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        result = list(dropwhile(lambda x: x < 6, numbers))
        assert result == [6, 7, 8, 9, 10]

    def test_reduceで畳み込み演算を行う(self) -> None:
        """Test fold operation using reduce."""
        from functools import reduce

        # Using reduce with initial value
        result1 = reduce(lambda total, n: total + n, [1, 2, 3, 4, 5], 0)
        # Using reduce without initial value
        result2 = reduce(lambda total, n: total + n, [1, 2, 3, 4, 5])

        assert result1 == 15
        assert result2 == 15
