class FizzBuzz:
    @staticmethod
    def generate(number):
        result = str(number)
        if number % 3 == 0 and number % 5 == 0:
            result = 'FizzBuzz'
        elif number % 3 == 0:
            result = 'Fizz'
        elif number % 5 == 0:
            result = 'Buzz'
        return result

    @staticmethod
    def print_1_to_100():
        result = []
        for i in range(1, 101):
            result.append(FizzBuzz.generate(i))
        return result

    @staticmethod
    def print_fizzbuzz():
        for i in range(1, 101):
            print(FizzBuzz.generate(i))
