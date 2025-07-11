"""FizzBuzz List First-Class Collection module."""
from typing import List

from .fizz_buzz_value import FizzBuzzValue


class FizzBuzzList:
    """First-class collection for FizzBuzz values."""
    
    MAX_COUNT = 100
    
    def __init__(self, value: List[FizzBuzzValue]) -> None:
        """Initialize FizzBuzzList.
        
        Args:
            value: List of FizzBuzzValue objects
            
        Raises:
            RuntimeError: If list count exceeds MAX_COUNT
        """
        if len(value) > self.MAX_COUNT:
            raise RuntimeError(f'上限は{self.MAX_COUNT}件までです')
        
        self._value = value
    
    @property
    def value(self) -> List[FizzBuzzValue]:
        """Get the list of values."""
        return self._value
    
    def __str__(self) -> str:
        """String representation."""
        return str(self._value)
    
    def add(self, other_value: List[FizzBuzzValue]) -> 'FizzBuzzList':
        """Add values and return new FizzBuzzList.
        
        Args:
            other_value: List of values to add
            
        Returns:
            New FizzBuzzList with combined values
        """
        return FizzBuzzList(self._value + other_value)
    
    def __len__(self) -> int:
        """Return length of the list."""
        return len(self._value)
    
    def __getitem__(self, index: int) -> FizzBuzzValue:
        """Get item by index."""
        return self._value[index]
    
    def __iter__(self):
        """Return iterator."""
        return iter(self._value)
