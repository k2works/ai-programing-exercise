"""Tests for FizzBuzzList class."""
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from lib.domain.model.fizz_buzz_list import FizzBuzzList
from lib.domain.model.fizz_buzz_value import FizzBuzzValue
from lib.application.fizz_buzz_list_command import FizzBuzzListCommand
from lib.domain.type.fizz_buzz_type import FizzBuzzType


class TestFizzBuzzList:
    """Test cases for FizzBuzzList class."""
    
    def test_新しいインスタンスが作られる(self):
        """Test that add method creates new instance."""
        command = FizzBuzzListCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
        array = command.execute(50)
        list1 = FizzBuzzList(array.value)
        list2 = list1.add(array.value)
        
        assert len(list1.value) == 50
        assert len(list2.value) == 100
