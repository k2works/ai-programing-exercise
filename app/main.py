import pytest

class TestFizzBuzz:
    def setup_method(self):
        self.fizzbuzz = FizzBuzz
    
    def test_1を渡したら文字列1を返す(self):
        assert self.fizzbuzz.generate(1) == '1'

    def test_2を渡したら文字列2を返す(self):
        assert self.fizzbuzz.generate(2) == '2'
    
    def test_3を渡したら文字列Fizzを返す(self):
        assert self.fizzbuzz.generate(3) == 'Fizz'
    
    def test_5を渡したら文字列Buzzを返す(self):
        assert self.fizzbuzz.generate(5) == 'Buzz'
    
    def test_15を渡したら文字列FizzBuzzを返す(self):
        assert self.fizzbuzz.generate(15) == 'FizzBuzz'
    
    def test_1から100までの数を文字列で返す(self):
        result = self.fizzbuzz.generate_list()
        assert len(result) == 100
        assert result[0] == '1'
        assert result[1] == '2'
        assert result[2] == 'Fizz'
        assert result[4] == 'Buzz'
        assert result[14] == 'FizzBuzz'

class FizzBuzz:
    @staticmethod
    def generate(number):
        if number % 15 == 0:
            return 'FizzBuzz'
        if number % 3 == 0:
            return 'Fizz'
        if number % 5 == 0:
            return 'Buzz'
        return str(number)
    
    @staticmethod
    def generate_list():
        return [FizzBuzz.generate(i) for i in range(1, 101)]
    
    @staticmethod
    def print_fizzbuzz():
        result = FizzBuzz.generate_list()
        for item in result:
            print(item)

if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == 'test':
        pytest.main([__file__])
    else:
        FizzBuzz.print_fizzbuzz()
