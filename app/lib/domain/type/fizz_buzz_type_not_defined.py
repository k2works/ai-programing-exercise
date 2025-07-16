"""FizzBuzz Type Not Defined implementation module."""

from .fizz_buzz_type import FizzBuzzType
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzTypeNotDefined(FizzBuzzType):
    """FizzBuzz Type Not Defined implementation."""

    def __str__(self) -> str:
        """Return string representation.

        Returns:
            String representation of undefined type
        """
        return "未定義"

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object with undefined value
        """
        return FizzBuzzValue(number, "未定義")
