"""FizzBuzz value object module."""


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
