"""FizzBuzz Value Command implementation module."""
from .fizz_buzz_command import FizzBuzzCommand
from ..domain.type.fizz_buzz_type import FizzBuzzType


class FizzBuzzValueCommand(FizzBuzzCommand):
    """Command for generating single FizzBuzz value."""
    
    def __init__(self, fizz_buzz_type: FizzBuzzType) -> None:
        """Initialize with FizzBuzz type.
        
        Args:
            fizz_buzz_type: FizzBuzz type strategy
        """
        self._type = fizz_buzz_type
    
    def execute(self, number: int) -> str:
        """Execute command to generate FizzBuzz value.
        
        Args:
            number: Input number
            
        Returns:
            Generated FizzBuzz value as string
        """
        return self._type.generate(number).value
