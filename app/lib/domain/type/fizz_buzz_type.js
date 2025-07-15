class FizzBuzzType {
  isFizz(number) {
    return number % 3 === 0;
  }

  isBuzz(number) {
    return number % 5 === 0;
  }

  static create(type) {
    switch (type) {
      case 1:
        return new (require('./fizz_buzz_type_01'))();
      case 2:
        return new (require('./fizz_buzz_type_02'))();
      case 3:
        return new (require('./fizz_buzz_type_03'))();
      default:
        return new (require('./fizz_buzz_type_not_defined'))();
    }
  }
}

module.exports = FizzBuzzType;
