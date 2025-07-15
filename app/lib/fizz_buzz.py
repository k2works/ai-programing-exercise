"""FizzBuzz implementation module."""


class FizzBuzz:
    """FizzBuzz class for generating FizzBuzz sequences."""

    MAX_NUMBER: int = 100

    @classmethod
    def generate(cls, number: int) -> str:
        """Generate FizzBuzz string for a given number.

        Args:
            number: The number to convert to FizzBuzz

        Returns:
            The FizzBuzz string representation
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

    @classmethod
    def generate_list(cls) -> list[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz strings
        """
        return [cls.generate(n) for n in range(1, cls.MAX_NUMBER + 1)]
