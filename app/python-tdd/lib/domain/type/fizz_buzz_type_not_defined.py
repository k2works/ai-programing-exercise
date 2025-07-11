"""FizzBuzz Type Not Defined implementation module."""
from .fizz_buzz_type import FizzBuzzType
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzTypeNotDefined(FizzBuzzType):
    """Null object for undefined FizzBuzz types."""
    
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for undefined type.
        
        Args:
            number: Input number
            
        Returns:
            FizzBuzzValue with empty string
        """
        return FizzBuzzValue(number, '')
    
    def __str__(self) -> str:
        """String representation for undefined type.
        
        Returns:
            Japanese text for 'undefined'
        """
        return '未定義'
