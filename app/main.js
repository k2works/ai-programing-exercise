class FizzBuzz {
  static generate(number, type = 1) {
    switch (type) {
      case 1:
        let result = number.toString();
        if (number % 3 === 0 && number % 5 === 0) {
          result = 'FizzBuzz';
        } else if (number % 3 === 0) {
          result = 'Fizz';
        } else if (number % 5 === 0) {
          result = 'Buzz';
        }
        return result;
      case 2:
        return number.toString();
      case 3:
        if (number % 3 === 0 && number % 5 === 0) {
          return 'FizzBuzz';
        } else if (number % 3 === 0) {
          return 'Fizz';
        } else if (number % 5 === 0) {
          return 'Buzz';
        }
        return number.toString();
      default:
        throw new Error(`Unknown type: ${type}`);
    }
  }

  static printRange(min, max) {
    const results = [];
    for (let i = min; i <= max; i++) {
      results.push(this.generate(i));
    }
    return results.join('\n');
  }
}

module.exports = FizzBuzz;
