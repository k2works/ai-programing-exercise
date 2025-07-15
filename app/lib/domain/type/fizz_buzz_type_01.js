const FizzBuzzType = require('./fizz_buzz_type');
const FizzBuzzValue = require('../model/fizz_buzz_value');

class FizzBuzzType01 extends FizzBuzzType {
  generate(number) {
    if (this.isFizz(number) && this.isBuzz(number)) {
      return new FizzBuzzValue(number, 'FizzBuzz');
    } else if (this.isFizz(number)) {
      return new FizzBuzzValue(number, 'Fizz');
    } else if (this.isBuzz(number)) {
      return new FizzBuzzValue(number, 'Buzz');
    }
    return new FizzBuzzValue(number, number.toString());
  }
}

module.exports = FizzBuzzType01;
