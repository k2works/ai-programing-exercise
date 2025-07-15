const FizzBuzzCommand = require('./fizz_buzz_command');

class FizzBuzzValueCommand extends FizzBuzzCommand {
  constructor(type) {
    super();
    this._type = type;
  }

  execute(number) {
    return this._type.generate(number).value;
  }
}

module.exports = FizzBuzzValueCommand;
