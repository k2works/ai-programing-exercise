class FizzBuzz:
    @staticmethod
    def generate(number):
        return str(number)

class TestFizzBuzz:
    def setup_method(self):
        self.fizzbuzz = FizzBuzz
    
    def test_1を渡したら文字列1を返す(self):
        assert self.fizzbuzz.generate(1) == '1'
    
    def test_2を渡したら文字列2を返す(self):
        assert self.fizzbuzz.generate(2) == '2'
