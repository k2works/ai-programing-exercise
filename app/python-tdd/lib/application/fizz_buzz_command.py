"""FizzBuzz Command interface module."""
from abc import ABC, abstractmethod


class FizzBuzzCommand(ABC):
    """Command interface for FizzBuzz operations."""
    
    @abstractmethod
    def execute(self, *args) -> any:
        """Execute the command.
        
        Args:
            *args: Variable arguments
            
        Returns:
            Command result
        """
        pass
