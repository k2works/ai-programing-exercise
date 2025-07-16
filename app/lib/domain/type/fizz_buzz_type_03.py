"""FizzBuzz Type 3 implementation module."""

from .fizz_buzz_type import FizzBuzzType
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzType03(FizzBuzzType):
    """FizzBuzz Type 3 implementation."""

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object
        """
        if self.is_fizz(number) and self.is_buzz(number):
            return FizzBuzzValue(number, "FizzBuzz")
        return FizzBuzzValue(number, str(number))
