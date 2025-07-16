"""FizzBuzz implementation module."""

from typing import Union


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
    ) -> Union["FizzBuzzType01", "FizzBuzzType02", "FizzBuzzType03"]:
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
    ) -> Union["FizzBuzzType01", "FizzBuzzType02", "FizzBuzzType03"]:
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


class FizzBuzzType01:
    """FizzBuzz Type 1 implementation."""

    MAX_NUMBER: int = 100

    def __init__(self) -> None:
        """Initialize FizzBuzzType01 instance."""
        self._list: list[str] = []

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
        is_fizz = number % 3 == 0
        is_buzz = number % 5 == 0

        if is_fizz and is_buzz:
            return "FizzBuzz"
        if is_fizz:
            return "Fizz"
        if is_buzz:
            return "Buzz"
        return str(number)

    def generate_list(self) -> list[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz string values
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list


class FizzBuzzType02:
    """FizzBuzz Type 2 implementation."""

    MAX_NUMBER: int = 100

    def __init__(self) -> None:
        """Initialize FizzBuzzType02 instance."""
        self._list: list[str] = []

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
        return str(number)

    def generate_list(self) -> list[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz string values
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list


class FizzBuzzType03:
    """FizzBuzz Type 3 implementation."""

    MAX_NUMBER: int = 100

    def __init__(self) -> None:
        """Initialize FizzBuzzType03 instance."""
        self._list: list[str] = []

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
        is_fizz = number % 3 == 0
        is_buzz = number % 5 == 0

        if is_fizz and is_buzz:
            return "FizzBuzz"
        return str(number)

    def generate_list(self) -> list[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz string values
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list
