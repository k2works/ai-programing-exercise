"""FizzBuzz value command implementation module."""

from .fizz_buzz_command import FizzBuzzCommand
from ..domain.type.fizz_buzz_type import FizzBuzzType


class FizzBuzzValueCommand(FizzBuzzCommand):
    """Command for generating FizzBuzz values."""

    def __init__(self, type_instance: FizzBuzzType) -> None:
        """Initialize FizzBuzzValueCommand instance.

        Args:
            type_instance: The FizzBuzzType instance to use
        """
        self._type = type_instance

    def execute(self, number: int) -> str:
        """Execute the command to generate FizzBuzz value.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz string value
        """
        return self._type.generate(number).value
