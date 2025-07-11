"""FizzBuzz implementation module."""
from typing import List, Optional


class FizzBuzzValue:
    """Value object for FizzBuzz results."""
    
    def __init__(self, number: int, value: str) -> None:
        """Initialize FizzBuzzValue.
        
        Args:
            number: The input number
            value: The FizzBuzz result value
        """
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


class FizzBuzzList:
    """First-class collection for FizzBuzzValue objects."""
    def __init__(self, values: List[FizzBuzzValue]) -> None:
        self._values = values.copy()  # 防御的コピー
    
    @property
    def value(self) -> List[FizzBuzzValue]:
        return self._values.copy()
    
    def __getitem__(self, index: int) -> FizzBuzzValue:
        return self._values[index]
    
    def __len__(self) -> int:
        return len(self._values)
    
    def __iter__(self):
        return iter(self._values)
    
    def add(self, values: List[FizzBuzzValue]) -> 'FizzBuzzList':
        return FizzBuzzList(self._values + values)
    
    def __str__(self) -> str:
        return str([str(v) for v in self._values])


class FizzBuzz:
    """FizzBuzz generator class."""
    
    MAX_NUMBER: int = 100

    def __init__(self, type: int = 1) -> None:
        """Initialize FizzBuzz instance.
        
        Args:
            type: The type of FizzBuzz (1: normal, 2: numbers only, 3: FizzBuzz only)
        """
        self._type = self.create(type)
        self._list: Optional[FizzBuzzList] = None

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
    def list(self) -> Optional[FizzBuzzList]:
        """Get the generated FizzBuzz list."""
        return self._list

    def generate(self, number: int, type: int = None) -> FizzBuzzValue:
        """Generate FizzBuzz result for a given number.

        Args:
            number: The input number
            type: The type of FizzBuzz (deprecated - use instance type instead)

        Returns:
            FizzBuzzValue object containing the number and result
        """
        return self._type.generate(number)
    
    def generate_list(self) -> FizzBuzzList:
        """Generate FizzBuzz list from 1 to MAX_NUMBER."""
        values = [self.generate(i) for i in range(1, self.MAX_NUMBER + 1)]
        self._list = FizzBuzzList(values)
        return self._list


class FizzBuzzType:
    """Base class for FizzBuzz type implementations."""
    
    @classmethod
    def create(cls, type: int) -> 'FizzBuzzType':
        """Create appropriate FizzBuzz type instance.
        
        Args:
            type: The type of FizzBuzz
            
        Returns:
            FizzBuzzType instance with the specified type
            
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
    
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz result for a given number.
        
        This is a base implementation that should be overridden by subclasses.
        """
        raise NotImplementedError("Subclasses must implement generate method")
    
    def is_fizz(self, number: int) -> bool:
        """Check if number is divisible by 3 (Fizz condition)."""
        return number % 3 == 0
    
    def is_buzz(self, number: int) -> bool:
        """Check if number is divisible by 5 (Buzz condition)."""
        return number % 5 == 0


class FizzBuzzType01(FizzBuzzType):
    """Type 1 FizzBuzz implementation."""
    
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate Type 1 FizzBuzz result for a given number."""
        if self.is_fizz(number) and self.is_buzz(number):
            return FizzBuzzValue(number, "FizzBuzz")
        if self.is_fizz(number):
            return FizzBuzzValue(number, 'Fizz')
        if self.is_buzz(number):
            return FizzBuzzValue(number, 'Buzz')
        return FizzBuzzValue(number, str(number))


class FizzBuzzType02(FizzBuzzType):
    """Type 2 FizzBuzz implementation."""
    
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate Type 2 FizzBuzz result for a given number."""
        return FizzBuzzValue(number, str(number))


class FizzBuzzType03(FizzBuzzType):
    """Type 3 FizzBuzz implementation."""
    
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate Type 3 FizzBuzz result for a given number."""
        if self.is_fizz(number) and self.is_buzz(number):
            return FizzBuzzValue(number, "FizzBuzz")
        return FizzBuzzValue(number, str(number))
