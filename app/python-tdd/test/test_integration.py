"""Integration tests for the modularized FizzBuzz application."""
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from lib.application.fizz_buzz_value_command import FizzBuzzValueCommand
from lib.application.fizz_buzz_list_command import FizzBuzzListCommand
from lib.domain.type.fizz_buzz_type import FizzBuzzType
from lib.domain.model.fizz_buzz_value import FizzBuzzValue
from lib.domain.model.fizz_buzz_list import FizzBuzzList


class TestModularizedFizzBuzz:
    """Integration tests for modularized FizzBuzz application."""
    
    def test_value_command_integration(self):
        """Test value command integration with all types."""
        # Type 1
        command1 = FizzBuzzValueCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
        assert command1.execute(3) == 'Fizz'
        assert command1.execute(5) == 'Buzz'
        assert command1.execute(15) == 'FizzBuzz'
        assert command1.execute(1) == '1'
        
        # Type 2  
        command2 = FizzBuzzValueCommand(FizzBuzzType.create(FizzBuzzType.TYPE_02))
        assert command2.execute(3) == '3'
        assert command2.execute(5) == '5'
        assert command2.execute(15) == '15'
        
        # Type 3
        command3 = FizzBuzzValueCommand(FizzBuzzType.create(FizzBuzzType.TYPE_03))
        assert command3.execute(3) == '3'
        assert command3.execute(5) == '5'
        assert command3.execute(15) == 'FizzBuzz'
        
        # Undefined type
        command4 = FizzBuzzValueCommand(FizzBuzzType.create(4))
        assert command4.execute(3) == ''
    
    def test_list_command_integration(self):
        """Test list command integration."""
        command = FizzBuzzListCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
        result = command.execute(100)
        
        assert isinstance(result, FizzBuzzList)
        assert len(result) == 100
        assert result[0].value == '1'
        assert result[2].value == 'Fizz'
        assert result[4].value == 'Buzz'
        assert result[14].value == 'FizzBuzz'
        assert result[-1].value == 'Buzz'
    
    def test_value_object_immutability(self):
        """Test value object immutability."""
        value1 = FizzBuzzValue(3, 'Fizz')
        value2 = FizzBuzzValue(3, 'Fizz')
        
        assert value1 == value2
        assert str(value1) == '3:Fizz'
    
    def test_first_class_collection(self):
        """Test first-class collection behavior."""
        values = [FizzBuzzValue(1, '1'), FizzBuzzValue(2, '2')]
        list1 = FizzBuzzList(values)
        
        more_values = [FizzBuzzValue(3, 'Fizz')]
        list2 = list1.add(more_values)
        
        # Original list unchanged
        assert len(list1) == 2
        # New list has combined values
        assert len(list2) == 3
        assert list2[2].value == 'Fizz'
    
    def test_polymorphism_through_factory(self):
        """Test polymorphism through factory method."""
        types = [
            FizzBuzzType.create(FizzBuzzType.TYPE_01),
            FizzBuzzType.create(FizzBuzzType.TYPE_02),
            FizzBuzzType.create(FizzBuzzType.TYPE_03),
            FizzBuzzType.create(4)  # undefined
        ]
        
        expected_15_results = ['FizzBuzz', '15', 'FizzBuzz', '']
        
        for i, fizz_buzz_type in enumerate(types):
            result = fizz_buzz_type.generate(15)
            assert result.value == expected_15_results[i]
    
    def test_exception_handling(self):
        """Test exception handling."""
        # Negative value
        try:
            FizzBuzzValue(-1, '-1')
            assert False, "Expected AssertionError"
        except Exception as e:
            assert str(e) == '正の値のみ有効です'
        
        # List too long
        try:
            values = [FizzBuzzValue(i, str(i)) for i in range(101)]
            FizzBuzzList(values)
            assert False, "Expected RuntimeError"
        except RuntimeError as e:
            assert str(e) == '上限は100件までです'
