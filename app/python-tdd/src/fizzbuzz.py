"""FizzBuzz implementation for Test Driven Development learning."""

from typing import List


class FizzBuzz:
    """FizzBuzz class implementing the classic FizzBuzz game."""

    MAX_NUMBER: int = 100

    @classmethod
    def generate(cls, number: int) -> str:
        """Generate FizzBuzz result for a given number.

        Args:
            number: The input number

        Returns:
            'FizzBuzz' if divisible by both 3 and 5,
            'Fizz' if divisible by 3,
            'Buzz' if divisible by 5,
            string representation of the number otherwise
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
    def generate_list(cls) -> List[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER.

        Returns:
            List of FizzBuzz results
        """
        return [cls.generate(n) for n in range(1, cls.MAX_NUMBER + 1)]
