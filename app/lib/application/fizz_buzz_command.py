"""FizzBuzz command interface module."""

from abc import ABC, abstractmethod


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
