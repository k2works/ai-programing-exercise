"""FizzBuzz Type 03 implementation module."""
from .fizz_buzz_type import FizzBuzzType
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzType03(FizzBuzzType):
    """Type 3: FizzBuzz only for multiples of 15."""
    
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for Type 3.
        
        Args:
            number: Input number
            
        Returns:
            FizzBuzzValue with FizzBuzz only for multiples of 15
        """
        if self._is_fizz(number) and self._is_buzz(number):
            return FizzBuzzValue(number, 'FizzBuzz')
        else:
            return FizzBuzzValue(number, str(number))
