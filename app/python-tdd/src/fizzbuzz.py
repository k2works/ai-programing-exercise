"""FizzBuzz implementation module."""

from typing import List


class FizzBuzz:
    """FizzBuzz generator class."""

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
        if number % 15 == 0:
            return "FizzBuzz"
        if number % 3 == 0:
            return "Fizz"
        if number % 5 == 0:
            return "Buzz"
        return str(number)

    @classmethod
    def generate_list(cls) -> List[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER."""
        return [cls.generate(i) for i in range(1, cls.MAX_NUMBER + 1)]

    @classmethod
    def print_fizzbuzz(cls) -> None:
        """Print FizzBuzz results to stdout."""
        result = cls.generate_list()
        for item in result:
            print(item)
