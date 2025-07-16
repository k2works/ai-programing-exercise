"""FizzBuzz implementation module."""


class FizzBuzzValue:
    """Value object for FizzBuzz values."""

    def __init__(self, number: int, value: str) -> None:
        """Initialize FizzBuzzValue instance.

        Args:
            number: The number
            value: The FizzBuzz string value
        """
        self._number = number
        self._value = value

    @property
    def number(self) -> int:
        """Get the number.

        Returns:
            The number
        """
        return self._number

    @property
    def value(self) -> str:
        """Get the value.

        Returns:
            The FizzBuzz string value
        """
        return self._value

    def __str__(self) -> str:
        """String representation.

        Returns:
            String representation in format 'number:value'
        """
        return f"{self._number}:{self._value}"

    def __eq__(self, other) -> bool:
        """Equality comparison.

        Args:
            other: Other FizzBuzzValue to compare

        Returns:
            True if equal, False otherwise
        """
        if not isinstance(other, FizzBuzzValue):
            return False
        return self._number == other._number and self._value == other._value

    def __hash__(self) -> int:
        """Hash method for use in sets and dictionaries.

        Returns:
            Hash value
        """
        return hash((self._number, self._value))


class FizzBuzz:
    """FizzBuzz class for generating FizzBuzz sequences."""

    MAX_NUMBER: int = 100

    def __init__(self, type_: int = 1) -> None:
        """Initialize FizzBuzz instance.

        Args:
            type_: The type of FizzBuzz conversion (default: 1)
        """
        self._list: list[FizzBuzzValue] = []
        self._type = FizzBuzzType.create(type_)

    @property
    def items(self) -> list[FizzBuzzValue]:
        """Get the FizzBuzz list.

        Returns:
            The FizzBuzz list
        """
        return self._list

    @property
    def type(self):
        """Get the FizzBuzz type.

        Returns:
            The FizzBuzz type instance
        """
        return self._type

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object
        """
        return self._type.generate(number)

    def generate_list(self) -> list[FizzBuzzValue]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz value objects
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list


class FizzBuzzType:
    """Base class for FizzBuzz type implementations."""

    @classmethod
    def create(cls, type_: int):
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

    def fizz(self, number: int) -> bool:
        """Check if number is divisible by 3.

        Args:
            number: The number to check

        Returns:
            True if number is divisible by 3, False otherwise
        """
        return number % 3 == 0

    def buzz(self, number: int) -> bool:
        """Check if number is divisible by 5.

        Args:
            number: The number to check

        Returns:
            True if number is divisible by 5, False otherwise
        """
        return number % 5 == 0


class FizzBuzzType01(FizzBuzzType):
    """FizzBuzz Type 1 implementation."""

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for Type 1.

        Args:
            number: The number to convert

        Returns:
            The FizzBuzz value object
        """
        is_fizz = self.fizz(number)
        is_buzz = self.buzz(number)

        if is_fizz and is_buzz:
            return FizzBuzzValue(number, "FizzBuzz")
        if is_fizz:
            return FizzBuzzValue(number, "Fizz")
        if is_buzz:
            return FizzBuzzValue(number, "Buzz")

        return FizzBuzzValue(number, str(number))


class FizzBuzzType02(FizzBuzzType):
    """FizzBuzz Type 2 implementation."""

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for Type 2.

        Args:
            number: The number to convert

        Returns:
            The FizzBuzz value object
        """
        return FizzBuzzValue(number, str(number))


class FizzBuzzType03(FizzBuzzType):
    """FizzBuzz Type 3 implementation."""

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for Type 3.

        Args:
            number: The number to convert

        Returns:
            The FizzBuzz value object
        """
        is_fizz = self.fizz(number)
        is_buzz = self.buzz(number)

        if is_fizz and is_buzz:
            return FizzBuzzValue(number, "FizzBuzz")

        return FizzBuzzValue(number, str(number))
