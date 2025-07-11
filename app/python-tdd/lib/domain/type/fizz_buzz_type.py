"""FizzBuzz Type base class module."""
from abc import ABC, abstractmethod

from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzType(ABC):
    """Base class for FizzBuzz type strategy."""
    
    TYPE_01 = 1
    TYPE_02 = 2
    TYPE_03 = 3
    
    @classmethod
    def create(cls, type_num: int) -> 'FizzBuzzType':
        """Factory method to create FizzBuzzType instance.
        
        Args:
            type_num: Type number
            
        Returns:
            FizzBuzzType instance
        """
        from .fizz_buzz_type_01 import FizzBuzzType01
        from .fizz_buzz_type_02 import FizzBuzzType02  
        from .fizz_buzz_type_03 import FizzBuzzType03
        from .fizz_buzz_type_not_defined import FizzBuzzTypeNotDefined
        
        if type_num == cls.TYPE_01:
            return FizzBuzzType01()
        elif type_num == cls.TYPE_02:
            return FizzBuzzType02()
        elif type_num == cls.TYPE_03:
            return FizzBuzzType03()
        else:
            return FizzBuzzTypeNotDefined()
    
    @abstractmethod
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for given number.
        
        Args:
            number: Input number
            
        Returns:
            FizzBuzzValue object
        """
        pass
    
    def _is_fizz(self, number: int) -> bool:
        """Check if number is fizz (divisible by 3).
        
        Args:
            number: Input number
            
        Returns:
            True if fizz, False otherwise
        """
        return number % 3 == 0
    
    def _is_buzz(self, number: int) -> bool:
        """Check if number is buzz (divisible by 5).
        
        Args:
            number: Input number
            
        Returns:
            True if buzz, False otherwise
        """
        return number % 5 == 0
