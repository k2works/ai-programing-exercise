#!/usr/bin/env python3
"""Manual integration test for modular FizzBuzz."""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))

from lib.application.fizz_buzz_value_command import FizzBuzzValueCommand
from lib.application.fizz_buzz_list_command import FizzBuzzListCommand  
from lib.domain.type.fizz_buzz_type import FizzBuzzType
from lib.domain.model.fizz_buzz_value import FizzBuzzValue
from lib.domain.model.fizz_buzz_list import FizzBuzzList

def test_value_command():
    """Test value command integration."""
    print("Testing value command integration...")
    
    # Type 1
    command1 = FizzBuzzValueCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
    assert command1.execute(3) == 'Fizz'
    assert command1.execute(5) == 'Buzz'
    assert command1.execute(15) == 'FizzBuzz'
    assert command1.execute(1) == '1'
    print("✓ Type 1 tests passed")
    
    # Type 2
    command2 = FizzBuzzValueCommand(FizzBuzzType.create(FizzBuzzType.TYPE_02))
    assert command2.execute(3) == '3'
    assert command2.execute(5) == '5'
    assert command2.execute(15) == '15'
    print("✓ Type 2 tests passed")
    
    # Type 3
    command3 = FizzBuzzValueCommand(FizzBuzzType.create(FizzBuzzType.TYPE_03))
    assert command3.execute(3) == '3'
    assert command3.execute(5) == '5'
    assert command3.execute(15) == 'FizzBuzz'
    print("✓ Type 3 tests passed")
    
    # Undefined type
    command4 = FizzBuzzValueCommand(FizzBuzzType.create(4))
    assert command4.execute(3) == ''
    print("✓ Undefined type tests passed")

def test_list_command():
    """Test list command integration."""
    print("Testing list command integration...")
    
    command = FizzBuzzListCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
    result = command.execute(100)
    
    assert isinstance(result, FizzBuzzList)
    assert len(result) == 100
    assert result[0].value == '1'
    assert result[2].value == 'Fizz'
    assert result[4].value == 'Buzz'
    assert result[14].value == 'FizzBuzz'
    assert result[-1].value == 'Buzz'
    print("✓ List command tests passed")

def test_value_object():
    """Test value object immutability."""
    print("Testing value object immutability...")
    
    value1 = FizzBuzzValue(3, 'Fizz')
    value2 = FizzBuzzValue(3, 'Fizz')
    
    assert value1 == value2
    assert str(value1) == '3:Fizz'
    print("✓ Value object tests passed")

def test_first_class_collection():
    """Test first-class collection behavior."""
    print("Testing first-class collection behavior...")
    
    values = [FizzBuzzValue(1, '1'), FizzBuzzValue(2, '2')]
    list1 = FizzBuzzList(values)
    
    more_values = [FizzBuzzValue(3, 'Fizz')]
    list2 = list1.add(more_values)
    
    # Original list unchanged
    assert len(list1) == 2
    # New list has combined values
    assert len(list2) == 3
    assert list2[2].value == 'Fizz'
    print("✓ First-class collection tests passed")

def test_polymorphism():
    """Test polymorphism through factory method."""
    print("Testing polymorphism through factory method...")
    
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
    print("✓ Polymorphism tests passed")

def test_exception_handling():
    """Test exception handling."""
    print("Testing exception handling...")
    
    # Negative value
    try:
        FizzBuzzValue(-1, '-1')
        assert False, "Expected AssertionError"
    except Exception as e:
        assert str(e) == '正の値のみ有効です'
    print("✓ Negative value exception test passed")
    
    # List too long
    try:
        values = [FizzBuzzValue(i, str(i)) for i in range(101)]
        FizzBuzzList(values)
        assert False, "Expected RuntimeError"
    except RuntimeError as e:
        assert str(e) == '上限は100件までです'
    print("✓ List too long exception test passed")

if __name__ == '__main__':
    print("Running manual integration tests...")
    print("=" * 50)
    
    test_value_command()
    test_list_command()
    test_value_object()
    test_first_class_collection()
    test_polymorphism()
    test_exception_handling()
    
    print("=" * 50)
    print("All integration tests passed successfully!")
