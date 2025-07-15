const FizzBuzzType = require('./fizz_buzz_type');
const FizzBuzzValue = require('../model/fizz_buzz_value');

class FizzBuzzType03 extends FizzBuzzType {
  generate(number) {
    if (this.isFizz(number) && this.isBuzz(number)) {
      return new FizzBuzzValue(number, 'FizzBuzz');
    }
    return new FizzBuzzValue(number, number.toString());
  }
}

module.exports = FizzBuzzType03;
