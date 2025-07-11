"""Tests for FizzBuzzListCommand class."""
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from lib.application.fizz_buzz_list_command import FizzBuzzListCommand
from lib.domain.type.fizz_buzz_type_01 import FizzBuzzType01
from lib.domain.type.fizz_buzz_type import FizzBuzzType
from lib.domain.model.fizz_buzz_list import FizzBuzzList


class TestFizzBuzzListCommand:
    """Test cases for FizzBuzzListCommand class."""
    
    class TestType1:
        """Test type 1 case."""
        
        def setup_method(self):
            """Setup method for each test."""
            fizzbuzz = FizzBuzzListCommand(FizzBuzzType01())
            self.result = fizzbuzz.execute(100)
        
        def test_配列の初めは文字列の1を返す(self):
            """Test that generate_list returns correct first item."""
            assert self.result[0].value == '1'
        
        def test_配列の最後は文字列のbuzzを返す(self):
            """Test that generate_list returns correct last item."""
            assert self.result[-1].value == 'Buzz'
        
        def test_配列の2番目は文字列のfizzを返す(self):
            """Test that generate_list returns correct third item."""
            assert self.result[2].value == 'Fizz'
        
        def test_配列の4番目は文字列のbuzzを返す(self):
            """Test that generate_list returns correct fifth item."""
            assert self.result[4].value == 'Buzz'
        
        def test_配列の14番目は文字列のfizzbuzzを返す(self):
            """Test that generate_list returns correct fifteenth item."""
            assert self.result[14].value == 'FizzBuzz'
    
    class TestExceptionCase:
        """Test exception cases."""
        
        def test_100より多い数を許可しない(self):
            """Test that list count exceeds limit throws error."""
            try:
                command = FizzBuzzListCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
                command.execute(101)
                assert False, "Expected RuntimeError"
            except RuntimeError as e:
                assert str(e) == '上限は100件までです'
