"""FizzBuzz implementation module."""

from .domain.model.fizz_buzz_value import FizzBuzzValue
from .domain.model.fizz_buzz_list import FizzBuzzList
from .domain.type.fizz_buzz_type import FizzBuzzType
from .application.fizz_buzz_command import FizzBuzzCommand
from .application.fizz_buzz_value_command import FizzBuzzValueCommand
from .application.fizz_buzz_list_command import FizzBuzzListCommand


class FizzBuzz:
    """FizzBuzz class for generating FizzBuzz sequences."""

    MAX_NUMBER: int = 100

    def __init__(self, type_: int = 1) -> None:
        """Initialize FizzBuzz instance.

        Args:
            type_: The type of FizzBuzz conversion (default: 1)
        """
        self._list: FizzBuzzList = FizzBuzzList([])
        self._type_instance = FizzBuzzType.create(type_)

    @property
    def fizz_buzz_list(self) -> list[FizzBuzzValue]:
        """Get the FizzBuzz list.

        Returns:
            The FizzBuzz list
        """
        return self._list.value

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object
        """
        return self._type_instance.generate(number)

    def generate_list(self) -> list[FizzBuzzValue]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz value objects
        """
        values = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        self._list = self._list.add(values)
        return self._list.value

    @classmethod
    def create(cls, type_: int) -> FizzBuzzType:
        """Factory method to create FizzBuzz type instances.

        Args:
            type_: The type of FizzBuzz conversion

        Returns:
            Instance of the appropriate FizzBuzz type class
        """
        return FizzBuzzType.create(type_)
