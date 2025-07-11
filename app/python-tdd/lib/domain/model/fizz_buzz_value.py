"""FizzBuzz Value Object module."""


class AssertionError(Exception):
    """Custom assertion error for FizzBuzz application."""
    pass


class FizzBuzzValue:
    """Value object for FizzBuzz results."""
    
    def __init__(self, number: int, value: str) -> None:
        """Initialize FizzBuzzValue.
        
        Args:
            number: The input number
            value: The FizzBuzz result value
            
        Raises:
            AssertionError: If number is negative
        """
        if number < 0:
            raise AssertionError('正の値のみ有効です')
        
        self._number = number
        self._value = value
    
    @property
    def number(self) -> int:
        """Get the number."""
        return self._number
    
    @property
    def value(self) -> str:
        """Get the value."""
        return self._value
    
    def __str__(self) -> str:
        """String representation."""
        return f"{self._number}:{self._value}"
    
    def __eq__(self, other) -> bool:
        """Check equality."""
        if not isinstance(other, FizzBuzzValue):
            return False
        return self._number == other._number and self._value == other._value
    
    def __hash__(self) -> int:
        """Hash value."""
        return hash((self._number, self._value))
