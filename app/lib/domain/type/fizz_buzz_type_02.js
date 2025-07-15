const FizzBuzzType = require('./fizz_buzz_type');
const FizzBuzzValue = require('../model/fizz_buzz_value');

class FizzBuzzType02 extends FizzBuzzType {
  generate(number) {
    return new FizzBuzzValue(number, number.toString());
  }
}

module.exports = FizzBuzzType02;
