"""FizzBuzz Type 1 implementation module."""

from .fizz_buzz_type import FizzBuzzType
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzType01(FizzBuzzType):
    """FizzBuzz Type 1 implementation."""

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object
        """
        if self.is_fizz(number) and self.is_buzz(number):
            return FizzBuzzValue(number, "FizzBuzz")
        if self.is_fizz(number):
            return FizzBuzzValue(number, "Fizz")
        if self.is_buzz(number):
            return FizzBuzzValue(number, "Buzz")
        return FizzBuzzValue(number, str(number))
