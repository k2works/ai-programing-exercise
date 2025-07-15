const FizzBuzzCommand = require('./fizz_buzz_command');
const FizzBuzzList = require('../domain/model/fizz_buzz_list');

class FizzBuzzListCommand extends FizzBuzzCommand {
  constructor(type) {
    super();
    this._type = type;
  }

  execute(number) {
    const values = [];
    for (let i = 1; i <= number; i++) {
      values.push(this._type.generate(i));
    }
    return new FizzBuzzList(values).value;
  }
}

module.exports = FizzBuzzListCommand;
