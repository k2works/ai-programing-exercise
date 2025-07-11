"""Tests for FizzBuzz class."""

from src.fizzbuzz import FizzBuzz


class TestFizzBuzz:
    """Test cases for FizzBuzz class."""

    def setup_method(self):
        """Setup method for each test."""
        self.fizzbuzz = FizzBuzz

    class TestBasicNumbers:
        """Test basic number conversion."""

        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzz

        def test_1を渡したら文字列1を返す(self):
            """Test that 1 returns '1'."""
            assert self.fizzbuzz.generate(1) == "1"

        def test_2を渡したら文字列2を返す(self):
            """Test that 2 returns '2'."""
            assert self.fizzbuzz.generate(2) == "2"

    class TestMultiplesOfThree:
        """Test multiples of three."""

        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzz

        def test_3を渡したら文字列fizzを返す(self):
            """Test that 3 returns 'Fizz'."""
            assert self.fizzbuzz.generate(3) == "Fizz"

        def test_6を渡したら文字列fizzを返す(self):
            """Test that 6 returns 'Fizz'."""
            assert self.fizzbuzz.generate(6) == "Fizz"

        def test_9を渡したら文字列fizzを返す(self):
            """Test that 9 returns 'Fizz'."""
            assert self.fizzbuzz.generate(9) == "Fizz"

        def test_12を渡したら文字列fizzを返す(self):
            """Test that 12 returns 'Fizz'."""
            assert self.fizzbuzz.generate(12) == "Fizz"

    class TestMultiplesOfFive:
        """Test multiples of five."""

        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzz

        def test_5を渡したら文字列buzzを返す(self):
            """Test that 5 returns 'Buzz'."""
            assert self.fizzbuzz.generate(5) == "Buzz"

        def test_10を渡したら文字列buzzを返す(self):
            """Test that 10 returns 'Buzz'."""
            assert self.fizzbuzz.generate(10) == "Buzz"

        def test_20を渡したら文字列buzzを返す(self):
            """Test that 20 returns 'Buzz'."""
            assert self.fizzbuzz.generate(20) == "Buzz"

        def test_25を渡したら文字列buzzを返す(self):
            """Test that 25 returns 'Buzz'."""
            assert self.fizzbuzz.generate(25) == "Buzz"

    class TestMultiplesOfThreeAndFive:
        """Test multiples of both three and five."""

        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzz

        def test_15を渡したら文字列fizzbuzzを返す(self):
            """Test that 15 returns 'FizzBuzz'."""
            assert self.fizzbuzz.generate(15) == "FizzBuzz"

        def test_30を渡したら文字列fizzbuzzを返す(self):
            """Test that 30 returns 'FizzBuzz'."""
            assert self.fizzbuzz.generate(30) == "FizzBuzz"

        def test_45を渡したら文字列fizzbuzzを返す(self):
            """Test that 45 returns 'FizzBuzz'."""
            assert self.fizzbuzz.generate(45) == "FizzBuzz"

        def test_60を渡したら文字列fizzbuzzを返す(self):
            """Test that 60 returns 'FizzBuzz'."""
            assert self.fizzbuzz.generate(60) == "FizzBuzz"

    class TestGenerateList:
        """Test list generation functionality."""

        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzz

        def test_1から100までの数を生成する(self):
            """Test that generate_list returns 100 items with correct values."""
            result = self.fizzbuzz.generate_list()
            assert len(result) == 100
            assert result[0] == "1"  # 1st item (1)
            assert result[2] == "Fizz"  # 3rd item (3)
            assert result[4] == "Buzz"  # 5th item (5)
            assert result[14] == "FizzBuzz"  # 15th item (15)
