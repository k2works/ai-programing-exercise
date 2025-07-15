class FizzBuzz {
  static generate(number: number): string {
    let result = number.toString();
    if (number % 3 === 0) {
      result = 'Fizz';
    }
    return result;
  }
}

export { FizzBuzz };
