"""FizzBuzz type base class module."""

from abc import ABC, abstractmethod
from ..model.fizz_buzz_value import FizzBuzzValue


class FizzBuzzType(ABC):
    """Abstract base class for FizzBuzz type implementations."""

    MAX_NUMBER: int = 100

    def __init__(self) -> None:
        """Initialize FizzBuzzType instance."""
        self._list: list[FizzBuzzValue] = []

    @classmethod
    def create(cls, type_: int) -> "FizzBuzzType":
        """Factory method to create FizzBuzz type instances.

        Args:
            type_: The type of FizzBuzz conversion

        Returns:
            Instance of the appropriate FizzBuzz type class
        """
        from .fizz_buzz_type_01 import FizzBuzzType01
        from .fizz_buzz_type_02 import FizzBuzzType02
        from .fizz_buzz_type_03 import FizzBuzzType03
        from .fizz_buzz_type_not_defined import FizzBuzzTypeNotDefined

        if type_ == 1:
            return FizzBuzzType01()
        elif type_ == 2:
            return FizzBuzzType02()
        elif type_ == 3:
            return FizzBuzzType03()
        else:
            return FizzBuzzTypeNotDefined()

    @property
    def fizz_buzz_list(self) -> list[FizzBuzzValue]:
        """Get the FizzBuzz list.

        Returns:
            The FizzBuzz list
        """
        return self._list

    def is_fizz(self, number: int) -> bool:
        """Check if number is divisible by 3.

        Args:
            number: The number to check

        Returns:
            True if number is divisible by 3
        """
        return number % 3 == 0

    def is_buzz(self, number: int) -> bool:
        """Check if number is divisible by 5.

        Args:
            number: The number to check

        Returns:
            True if number is divisible by 5
        """
        return number % 5 == 0

    @abstractmethod
    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object
        """
        pass

    def generate_list(self) -> list[FizzBuzzValue]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz value objects
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list
