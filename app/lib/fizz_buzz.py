"""FizzBuzz implementation module."""


class FizzBuzz:
    """FizzBuzz class for generating FizzBuzz sequences."""

    MAX_NUMBER: int = 100

    def __init__(self, type_: int = 1) -> None:
        """Initialize FizzBuzz instance.

        Args:
            type_: The type of FizzBuzz conversion (default: 1)
        """
        self._list: list[str] = []
        self._type = FizzBuzz.create(type_)

    @property
    def items(self) -> list[str]:
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

    def generate(self, number: int) -> str:
        """Generate FizzBuzz string for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz string representation
        """
        return self._type.generate(number)

    def generate_list(self) -> list[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz strings
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list


class FizzBuzzType:
    """Base class for FizzBuzz type implementations."""

    def is_fizz(self, number: int) -> bool:
        """Check if number is divisible by 3.

        Args:
            number: The number to check

        Returns:
            True if number is divisible by 3, False otherwise
        """
        return number % 3 == 0

    def is_buzz(self, number: int) -> bool:
        """Check if number is divisible by 5.

        Args:
            number: The number to check

        Returns:
            True if number is divisible by 5, False otherwise
        """
        return number % 5 == 0


class FizzBuzzType01(FizzBuzzType):
    """FizzBuzz Type 1 implementation."""

    def generate(self, number: int) -> str:
        """Generate FizzBuzz string for Type 1.

        Args:
            number: The number to convert

        Returns:
            The FizzBuzz string representation
        """
        is_fizz = self.is_fizz(number)
        is_buzz = self.is_buzz(number)

        if is_fizz and is_buzz:
            return "FizzBuzz"
        if is_fizz:
            return "Fizz"
        if is_buzz:
            return "Buzz"

        return str(number)


class FizzBuzzType02(FizzBuzzType):
    """FizzBuzz Type 2 implementation."""

    def generate(self, number: int) -> str:
        """Generate FizzBuzz string for Type 2.

        Args:
            number: The number to convert

        Returns:
            The number as string
        """
        return str(number)


class FizzBuzzType03(FizzBuzzType):
    """FizzBuzz Type 3 implementation."""

    def generate(self, number: int) -> str:
        """Generate FizzBuzz string for Type 3.

        Args:
            number: The number to convert

        Returns:
            The FizzBuzz string representation
        """
        is_fizz = self.is_fizz(number)
        is_buzz = self.is_buzz(number)

        if is_fizz and is_buzz:
            return "FizzBuzz"

        return str(number)
