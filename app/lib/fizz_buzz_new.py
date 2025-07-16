"""FizzBuzz implementation module."""


class FizzBuzz:
    """FizzBuzz class for generating FizzBuzz sequences."""

    MAX_NUMBER: int = 100

    def __init__(self) -> None:
        """Initialize FizzBuzz instance."""
        self._list: list[str] = []

    @property
    def list(self) -> list[str]:
        """Get the FizzBuzz list.
        
        Returns:
            The FizzBuzz list
        """
        return self._list

    def generate(self, number: int, type_: int = 1) -> str:
        """Generate FizzBuzz string for a given number.

        Args:
            number: The number to convert to FizzBuzz
            type_: The type of conversion (default: 1)

        Returns:
            The FizzBuzz string representation
        """
        if type_ == 1:
            is_fizz = number % 3 == 0
            is_buzz = number % 5 == 0

            if is_fizz and is_buzz:
                return "FizzBuzz"
            if is_fizz:
                return "Fizz"
            if is_buzz:
                return "Buzz"

            return str(number)
        elif type_ == 2:
            return str(number)
        elif type_ == 3:
            is_fizz = number % 3 == 0
            is_buzz = number % 5 == 0

            if is_fizz and is_buzz:
                return "FizzBuzz"

            return str(number)
        else:
            raise RuntimeError("該当するタイプは存在しません")

    def generate_list(self) -> list[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz strings
        """
        self._list = [self.generate(n) for n in range(1, self.MAX_NUMBER + 1)]
        return self._list
