"""FizzBuzz Type 02 implementation module."""
from .fizz_buzz_type import FizzBuzzType
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzType02(FizzBuzzType):
    """Type 2: Numbers only behavior."""
    
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for Type 2 (numbers only).
        
        Args:
            number: Input number
            
        Returns:
            FizzBuzzValue with number as string
        """
        return FizzBuzzValue(number, str(number))
