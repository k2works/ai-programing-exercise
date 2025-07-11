#!/usr/bin/env python3
"""FizzBuzz Application main entry point."""

import sys
import os

# Add current directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))

from lib.application.fizz_buzz_list_command import FizzBuzzListCommand
from lib.domain.type.fizz_buzz_type import FizzBuzzType


def main():
    """Main function to execute FizzBuzz application."""
    try:
        command = FizzBuzzListCommand(FizzBuzzType.create(FizzBuzzType.TYPE_01))
        result = command.execute(100)
        
        for value in result.value:
            print(value.value)
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == '__main__':
    main()
