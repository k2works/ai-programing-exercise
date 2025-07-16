"""FizzBuzz implementation module."""

from abc import ABC, abstractmethod


class FizzBuzzValue:
    """Value object for FizzBuzz values."""

    def __init__(self, number: int, value: str) -> None:
        """Initialize FizzBuzzValue instance.

        Args:
            number: The original number
            value: The FizzBuzz converted value

        Raises:
            ValueError: If number is negative
        """
        if number < 0:
            raise ValueError("正の値のみ有効です")

        self._number = number
        self._value = value

    @property
    def number(self) -> int:
        """Get the original number.

        Returns:
            The original number
        """
        return self._number

    @property
    def value(self) -> str:
        """Get the FizzBuzz value.

        Returns:
            The FizzBuzz value
        """
        return self._value

    def __str__(self) -> str:
        """Return string representation.

        Returns:
            String representation in format "number:value"
        """
        return f"{self._number}:{self._value}"

    def __eq__(self, other: object) -> bool:
        """Check equality with another FizzBuzzValue.

        Args:
            other: Object to compare with

        Returns:
            True if equal, False otherwise
        """
        if isinstance(other, FizzBuzzValue):
            return self._number == other._number and self._value == other._value
        return False

    def __hash__(self) -> int:
        """Return hash value.

        Returns:
            Hash value based on number and value
        """
        return hash((self._number, self._value))


class FizzBuzzList:
    """First-class collection for FizzBuzz values."""

    def __init__(self, list_: list[FizzBuzzValue]) -> None:
        """Initialize FizzBuzzList instance.

        Args:
            list_: List of FizzBuzzValue objects

        Raises:
            ValueError: If list has more than 100 items
        """
        if len(list_) > 100:
            raise ValueError("上限は100件までです")

        self._value = list_.copy()  # イミュータブルにするため、コピーを作成

    @property
    def value(self) -> list[FizzBuzzValue]:
        """Get the FizzBuzz value list.

        Returns:
            List of FizzBuzz values
        """
        return self._value.copy()  # イミュータブルにするため、コピーを返す

    def __str__(self) -> str:
        """Return string representation.

        Returns:
            String representation of the list
        """
        return str([str(item) for item in self._value])

    def add(self, values: list[FizzBuzzValue]) -> "FizzBuzzList":
        """Add values and return a new FizzBuzzList instance.

        Args:
            values: List of FizzBuzzValue objects to add

        Returns:
            New FizzBuzzList instance with added values
        """
        return FizzBuzzList(self._value + values)


class FizzBuzzCommand(ABC):
    """Abstract base class for FizzBuzz commands."""

    @abstractmethod
    def execute(self, number: int) -> object:
        """Execute the command.

        Args:
            number: The number to process

        Returns:
            Result of the command execution
        """
        pass


class FizzBuzzValueCommand(FizzBuzzCommand):
    """Command for generating FizzBuzz values."""

    def __init__(self, type_instance: "FizzBuzzType") -> None:
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


class FizzBuzzListCommand(FizzBuzzCommand):
    """Command for generating FizzBuzz lists."""

    def __init__(self, type_instance: "FizzBuzzType") -> None:
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
    def create(cls, type_: int) -> "FizzBuzzType":
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
            return FizzBuzzTypeNotDefined()


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


class FizzBuzzType01(FizzBuzzType):
    """FizzBuzz Type 1 implementation."""

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object
        """
        if self.is_fizz(number) and self.is_buzz(number):
            return FizzBuzzValue(number, "FizzBuzz")
        if self.is_fizz(number):
            return FizzBuzzValue(number, "Fizz")
        if self.is_buzz(number):
            return FizzBuzzValue(number, "Buzz")
        return FizzBuzzValue(number, str(number))


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


class FizzBuzzType03(FizzBuzzType):
    """FizzBuzz Type 3 implementation."""

    def generate(self, number: int) -> FizzBuzzValue:
        """Generate FizzBuzz value for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz value object
        """
        if self.is_fizz(number) and self.is_buzz(number):
            return FizzBuzzValue(number, "FizzBuzz")
        return FizzBuzzValue(number, str(number))


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
