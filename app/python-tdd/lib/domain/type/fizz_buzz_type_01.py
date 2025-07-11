"""FizzBuzz Type 01 implementation module."""
from .fizz_buzz_type import FizzBuzzType
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzType01(FizzBuzzType):
    """Type 1: Normal FizzBuzz behavior."""
    
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for Type 1.
        
        Args:
            number: Input number
            
        Returns:
            FizzBuzzValue with appropriate value
        """
        if self._is_fizz(number) and self._is_buzz(number):
            return FizzBuzzValue(number, 'FizzBuzz')
        elif self._is_fizz(number):
            return FizzBuzzValue(number, 'Fizz')
        elif self._is_buzz(number):
            return FizzBuzzValue(number, 'Buzz')
        else:
            return FizzBuzzValue(number, str(number))
