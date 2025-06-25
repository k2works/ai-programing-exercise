import pytest

def test_1を渡したら文字列1を返す():
    assert FizzBuzz.generate(1) == '1'

def test_2を渡したら文字列2を返す():
    assert FizzBuzz.generate(2) == '2'

class FizzBuzz:
    @staticmethod
    def generate(number):
        return str(number)

if __name__ == '__main__':
    pytest.main([__file__])
