const FizzBuzzType = require('./fizz_buzz_type');
const FizzBuzzValue = require('../model/fizz_buzz_value');

class FizzBuzzTypeNotDefined extends FizzBuzzType {
  generate(number) {
    return new FizzBuzzValue(number, '未定義');
  }

  toString() {
    return '未定義';
  }
}

module.exports = FizzBuzzTypeNotDefined;
