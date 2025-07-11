"""FizzBuzz List Command implementation module."""
from .fizz_buzz_command import FizzBuzzCommand
from ..domain.type.fizz_buzz_type import FizzBuzzType
from ..domain.model.fizz_buzz_list import FizzBuzzList


class FizzBuzzListCommand(FizzBuzzCommand):
    """Command for generating FizzBuzz list."""
    
    def __init__(self, fizz_buzz_type: FizzBuzzType) -> None:
        """Initialize with FizzBuzz type.
        
        Args:
            fizz_buzz_type: FizzBuzz type strategy
        """
        self._type = fizz_buzz_type
    
    def execute(self, number: int) -> FizzBuzzList:
        """Execute command to generate FizzBuzz list.
        
        Args:
            number: Maximum number to generate
            
        Returns:
            Generated FizzBuzz list
        """
        values = [self._type.generate(i) for i in range(1, number + 1)]
        return FizzBuzzList(values)
