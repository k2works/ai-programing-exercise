"""Tests for FizzBuzzValue class."""
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from lib.domain.model.fizz_buzz_value import FizzBuzzValue


class TestFizzBuzzValue:
    """Test cases for FizzBuzzValue class."""
    
    def test_同じ値である(self):
        """Test that same values are equal."""
        value1 = FizzBuzzValue(1, '1')
        value2 = FizzBuzzValue(1, '1')
        
        assert value1 == value2
    
    def test_to_stringメソッド(self):
        """Test string representation."""
        value = FizzBuzzValue(3, 'Fizz')
        
        assert str(value) == "3:Fizz"
