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
