const FizzBuzzType = require('./lib/domain/type/fizz_buzz_type');
const FizzBuzzList = require('./lib/domain/model/fizz_buzz_list');

class FizzBuzz {
  constructor(type = 1) {
    this.MAX_NUMBER = 100;
    this._type = FizzBuzzType.create(type);
    this._list = new FizzBuzzList([]);
  }

  get list() {
    return this._list.value;
  }

  generate(number) {
    return this._type.generate(number);
  }

  generateList() {
    const newList = [];
    for (let i = 1; i <= this.MAX_NUMBER; i++) {
      newList.push(this._type.generate(i));
    }
    this._list = this._list.add(newList);
  }

  printRange(min, max) {
    const results = [];
    for (let i = min; i <= max; i++) {
      results.push(this.generate(i).value);
    }
    return results.join('\n');
  }
}

module.exports = FizzBuzz;
