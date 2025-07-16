"""FizzBuzz Type 2 implementation module."""

from .fizz_buzz_type import FizzBuzzType
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzType02(FizzBuzzType):
    """FizzBuzz Type 2 implementation."""

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object
        """
        return FizzBuzzValue(number, str(number))
