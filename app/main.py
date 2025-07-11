class FizzBuzz:
    @staticmethod
    def generate(number):
        return '1'

def test_1を渡したら文字列1を返す():
    assert FizzBuzz.generate(1) == '1'
