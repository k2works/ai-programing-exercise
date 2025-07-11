"""FizzBuzz implementation module."""
from typing import List, Optional


class FizzBuzz:
    """FizzBuzz generator class."""
    
    MAX_NUMBER: int = 100

    def __init__(self, type: int = 1) -> None:
        """Initialize FizzBuzz instance.
        
        Args:
            type: The type of FizzBuzz (1: normal, 2: numbers only, 3: FizzBuzz only)
        """
        self._type = self.create(type)
        self._list: Optional[List[str]] = None

    @classmethod
    def create(cls, type: int) -> 'FizzBuzz':
        """Create FizzBuzz instance with appropriate type.
        
        Args:
            type: The type of FizzBuzz
            
        Returns:
            FizzBuzz instance with the specified type
            
        Raises:
            RuntimeError: If type is not 1, 2, or 3
        """
        if type == 1:
            return FizzBuzzType01()
        elif type == 2:
            return FizzBuzzType02()
        elif type == 3:
            return FizzBuzzType03()
        else:
            raise RuntimeError('該当するタイプは存在しません')

    @property
    def list(self) -> Optional[List[str]]:
        """Get the generated FizzBuzz list."""
        return self._list

    def generate(self, number: int, type: int = None) -> str:
        """Generate FizzBuzz result for a given number.

        Args:
            number: The input number
            type: The type of FizzBuzz (deprecated - use instance type instead)

        Returns:
            'FizzBuzz' if divisible by both 3 and 5,
            'Fizz' if divisible by 3,
            'Buzz' if divisible by 5,
            string representation of the number otherwise
        """
        return self._type.generate(number)
    
    def generate_list(self) -> List[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER."""
        self._list = [self.generate(i) for i in range(1, self.MAX_NUMBER + 1)]
        return self._list


class FizzBuzzType:
    """Base class for FizzBuzz type implementations."""
    
    def generate(self, number: int) -> str:
        """Generate FizzBuzz result for a given number.
        
        This is a base implementation that should be overridden by subclasses.
        """
        raise NotImplementedError("Subclasses must implement generate method")
    
    def is_fizz(self, number: int) -> bool:
        """Check if number is divisible by 3."""
        return number % 3 == 0
    
    def is_buzz(self, number: int) -> bool:
        """Check if number is divisible by 5."""
        return number % 5 == 0


class FizzBuzzType01(FizzBuzzType):
    """Type 1 FizzBuzz implementation."""
    
    def generate(self, number: int) -> str:
        """Generate Type 1 FizzBuzz result for a given number."""
        if self.is_fizz(number) and self.is_buzz(number):
            return "FizzBuzz"
        if self.is_fizz(number):
            return 'Fizz'
        if self.is_buzz(number):
            return 'Buzz'
        return str(number)


class FizzBuzzType02(FizzBuzzType):
    """Type 2 FizzBuzz implementation."""
    
    def generate(self, number: int) -> str:
        """Generate Type 2 FizzBuzz result for a given number."""
        return str(number)


class FizzBuzzType03(FizzBuzzType):
    """Type 3 FizzBuzz implementation."""
    
    def generate(self, number: int) -> str:
        """Generate Type 3 FizzBuzz result for a given number."""
        if self.is_fizz(number) and self.is_buzz(number):
            return "FizzBuzz"
        return str(number)
