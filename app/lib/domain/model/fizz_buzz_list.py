"""FizzBuzz list collection module."""

from .fizz_buzz_value import FizzBuzzValue


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
