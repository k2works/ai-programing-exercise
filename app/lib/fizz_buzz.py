"""FizzBuzz implementation module."""

from abc import ABC, abstractmethod


class FizzBuzz:
    """FizzBuzz class for generating FizzBuzz sequences."""

    MAX_NUMBER: int = 100

    def __init__(self, type_: int = 1) -> None:
        """Initialize FizzBuzz instance.

        Args:
            type_: The type of FizzBuzz conversion (default: 1)
        """
        self._list: list[str] = []
        self._type_instance = self._create_type_instance(type_)

    def _create_type_instance(
        self, type_: int
    ) -> "FizzBuzzType":
        """Create appropriate type instance.

        Args:
            type_: The type of FizzBuzz conversion

        Returns:
            Instance of the appropriate FizzBuzz type class
        """
        if type_ == 1:
            return FizzBuzzType01()
        elif type_ == 2:
            return FizzBuzzType02()
        elif type_ == 3:
            return FizzBuzzType03()
        else:
            raise RuntimeError("該当するタイプは存在しません")

    @property
    def fizz_buzz_list(self) -> list[str]:
        """Get the FizzBuzz list.

        Returns:
            The FizzBuzz list
        """
        return self._list

    def generate(self, number: int) -> str:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz string value
        """
        return self._type_instance.generate(number)

    def generate_list(self) -> list[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz string values
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list

    @classmethod
    def create(
        cls, type_: int
    ) -> "FizzBuzzType":
        """Factory method to create FizzBuzz type instances.

        Args:
            type_: The type of FizzBuzz conversion

        Returns:
            Instance of the appropriate FizzBuzz type class
        """
        if type_ == 1:
            return FizzBuzzType01()
        elif type_ == 2:
            return FizzBuzzType02()
        elif type_ == 3:
            return FizzBuzzType03()
        else:
            raise RuntimeError("該当するタイプは存在しません")


class FizzBuzzType(ABC):
    """Abstract base class for FizzBuzz type implementations."""

    MAX_NUMBER: int = 100

    def __init__(self) -> None:
        """Initialize FizzBuzzType instance."""
        self._list: list[str] = []

    @property
    def fizz_buzz_list(self) -> list[str]:
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
    def generate(self, number: int) -> str:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz string value
        """
        pass

    def generate_list(self) -> list[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz string values
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list


class FizzBuzzType01(FizzBuzzType):
    """FizzBuzz Type 1 implementation."""

    def generate(self, number: int) -> str:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz string value
        """
        if self.is_fizz(number) and self.is_buzz(number):
            return "FizzBuzz"
        if self.is_fizz(number):
            return "Fizz"
        if self.is_buzz(number):
            return "Buzz"
        return str(number)


class FizzBuzzType02(FizzBuzzType):
    """FizzBuzz Type 2 implementation."""

    def generate(self, number: int) -> str:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz string value
        """
        return str(number)


class FizzBuzzType03(FizzBuzzType):
    """FizzBuzz Type 3 implementation."""

    def generate(self, number: int) -> str:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz string value
        """
        if self.is_fizz(number) and self.is_buzz(number):
            return "FizzBuzz"
        return str(number)
