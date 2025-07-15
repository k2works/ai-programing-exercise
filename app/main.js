class FizzBuzz {
  static generate(number) {
    let result = number.toString();
    if (number % 3 === 0) {
      result = 'Fizz';
    }
    return result;
  }
}

module.exports = FizzBuzz;
