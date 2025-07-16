"""FizzBuzz list command implementation module."""

from .fizz_buzz_command import FizzBuzzCommand
from ..domain.type.fizz_buzz_type import FizzBuzzType
from ..domain.model.fizz_buzz_value import FizzBuzzValue
from ..domain.model.fizz_buzz_list import FizzBuzzList


class FizzBuzzListCommand(FizzBuzzCommand):
    """Command for generating FizzBuzz lists."""

    def __init__(self, type_instance: FizzBuzzType) -> None:
        """Initialize FizzBuzzListCommand instance.

        Args:
            type_instance: The FizzBuzzType instance to use
        """
        self._type = type_instance

    def execute(self, number: int) -> list[FizzBuzzValue]:
        """Execute the command to generate FizzBuzz list.

        Args:
            number: The maximum number for the range (1 to number)

        Returns:
            List of FizzBuzz values
        """
        values = [self._type.generate(i) for i in range(1, number + 1)]
        return FizzBuzzList(values).value
