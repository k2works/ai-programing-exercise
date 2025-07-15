class FizzBuzz:
    @staticmethod
    def generate(number):
        result = str(number)
        if number % 3 == 0:
            result = 'Fizz'
        return result
