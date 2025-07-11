class FizzBuzz:
    @staticmethod
    def generate(number):
        return str(number)

def test_1を渡したら文字列1を返す():
    assert FizzBuzz.generate(1) == '1'

def test_2を渡したら文字列2を返す():
    assert FizzBuzz.generate(2) == '2'
