"""FizzBuzz implementation module."""
from typing import List, Optional


class FizzBuzz:
    """FizzBuzz generator class."""
    
    MAX_NUMBER: int = 100

    def __init__(self) -> None:
        """Initialize FizzBuzz instance."""
        self._list: Optional[List[str]] = None

    @property
    def list(self) -> Optional[List[str]]:
        """Get the generated FizzBuzz list."""
        return self._list

    def generate(self, number: int, type: int = 1) -> str:
        """Generate FizzBuzz result for a given number.

        Args:
            number: The input number
            type: The type of FizzBuzz (1: normal, 2: numbers only, 3: FizzBuzz only)

        Returns:
            'FizzBuzz' if divisible by both 3 and 5,
            'Fizz' if divisible by 3,
            'Buzz' if divisible by 5,
            string representation of the number otherwise
        """
        if type == 1:
            if number % 15 == 0:
                return "FizzBuzz"
            if number % 3 == 0:
                return 'Fizz'
            if number % 5 == 0:
                return 'Buzz'
            return str(number)
        elif type == 2:
            return str(number)
        elif type == 3:
            if number % 15 == 0:
                return "FizzBuzz"
            return str(number)
    
    def generate_list(self) -> List[str]:
        """Generate FizzBuzz list from 1 to MAX_NUMBER."""
        self._list = [self.generate(i) for i in range(1, self.MAX_NUMBER + 1)]
        return self._list
