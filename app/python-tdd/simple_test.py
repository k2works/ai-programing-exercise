#!/usr/bin/env python3
"""Simple test to verify modular structure works."""

import sys
import os

# Add current directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))

try:
    print("Testing modular FizzBuzz implementation...")
    
    # Import Value Object
    from lib.domain.model.fizz_buzz_value import FizzBuzzValue
    value = FizzBuzzValue(3, 'Fizz')
    print(f"FizzBuzzValue test: {value} == 3:Fizz")
    assert str(value) == '3:Fizz'
    
    # Import Type01
    from lib.domain.type.fizz_buzz_type_01 import FizzBuzzType01
    type01 = FizzBuzzType01()
    result = type01.generate(3)
    print(f"FizzBuzzType01 test: {result.value} == Fizz")
    assert result.value == 'Fizz'
    
    # Import Factory
    from lib.domain.type.fizz_buzz_type import FizzBuzzType
    factory_type = FizzBuzzType.create(FizzBuzzType.TYPE_01)
    factory_result = factory_type.generate(5) 
    print(f"Factory test: {factory_result.value} == Buzz")
    assert factory_result.value == 'Buzz'
    
    # Import Commands
    from lib.application.fizz_buzz_value_command import FizzBuzzValueCommand
    command = FizzBuzzValueCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
    command_result = command.execute(15)
    print(f"Command test: {command_result} == FizzBuzz")
    assert command_result == 'FizzBuzz'
    
    print("All modular tests passed!")
    
except Exception as e:
    print(f"Error: {e}")
    import traceback
    traceback.print_exc()
