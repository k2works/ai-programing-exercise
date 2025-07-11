"""Tests for FizzBuzzValueCommand class."""
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from lib.application.fizz_buzz_value_command import FizzBuzzValueCommand
from lib.domain.type.fizz_buzz_type_01 import FizzBuzzType01
from lib.domain.type.fizz_buzz_type_02 import FizzBuzzType02
from lib.domain.type.fizz_buzz_type_03 import FizzBuzzType03
from lib.domain.type.fizz_buzz_type import FizzBuzzType


class TestFizzBuzzValueCommand:
    """Test cases for FizzBuzzValueCommand class."""
    
    class TestType1:
        """Test type 1 case."""
        
        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzzValueCommand(FizzBuzzType01())
        
        def test_3を渡したら文字列fizzを返す(self):
            """Test that 3 returns 'Fizz'."""
            assert self.fizzbuzz.execute(3) == 'Fizz'
        
        def test_5を渡したら文字列buzzを返す(self):
            """Test that 5 returns 'Buzz'."""
            assert self.fizzbuzz.execute(5) == 'Buzz'
        
        def test_15を渡したら文字列fizzbuzzを返す(self):
            """Test that 15 returns 'FizzBuzz'."""
            assert self.fizzbuzz.execute(15) == 'FizzBuzz'
        
        def test_1を渡したら文字列1を返す(self):
            """Test that 1 returns '1'."""
            assert self.fizzbuzz.execute(1) == '1'
    
    class TestType2:
        """Test type 2 case."""
        
        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzzValueCommand(FizzBuzzType02())
        
        def test_3を渡したら文字列3を返す(self):
            """Test that 3 returns '3' for type 2."""
            assert self.fizzbuzz.execute(3) == '3'
        
        def test_5を渡したら文字列5を返す(self):
            """Test that 5 returns '5' for type 2."""
            assert self.fizzbuzz.execute(5) == '5'
        
        def test_15を渡したら文字列15を返す(self):
            """Test that 15 returns '15' for type 2."""
            assert self.fizzbuzz.execute(15) == '15'
        
        def test_1を渡したら文字列1を返す(self):
            """Test that 1 returns '1' for type 2."""
            assert self.fizzbuzz.execute(1) == '1'
    
    class TestType3:
        """Test type 3 case."""
        
        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzzValueCommand(FizzBuzzType03())
        
        def test_3を渡したら文字列3を返す(self):
            """Test that 3 returns '3' for type 3."""
            assert self.fizzbuzz.execute(3) == '3'
        
        def test_5を渡したら文字列5を返す(self):
            """Test that 5 returns '5' for type 3."""
            assert self.fizzbuzz.execute(5) == '5'
        
        def test_15を渡したら文字列fizzbuzzを返す(self):
            """Test that 15 returns 'FizzBuzz' for type 3."""
            assert self.fizzbuzz.execute(15) == 'FizzBuzz'
        
        def test_1を渡したら文字列1を返す(self):
            """Test that 1 returns '1' for type 3."""
            assert self.fizzbuzz.execute(1) == '1'
    
    class TestUndefinedType:
        """Test undefined type case."""
        
        def test_未定義のタイプを返す(self):
            """Test that invalid type returns undefined type."""
            fizzbuzz_type = FizzBuzzType.create(4)
            assert str(fizzbuzz_type) == '未定義'
        
        def test_空の文字列を返す(self):
            """Test that undefined type returns empty string."""
            type_obj = FizzBuzzType.create(4)
            command = FizzBuzzValueCommand(type_obj)
            assert command.execute(3) == ''
    
    class TestExceptionCase:
        """Test exception cases."""
        
        def test_値は正の値のみ許可する(self):
            """Test that negative values raise error."""
            try:
                command = FizzBuzzValueCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
                command.execute(-1)
                assert False, "Expected AssertionError"
            except Exception as e:
                assert str(e) == '正の値のみ有効です'
