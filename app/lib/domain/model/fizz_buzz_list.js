class FizzBuzzList {
  constructor(list) {
    if (list.length > 100) {
      throw new Error('上限は100件までです');
    }
    this._value = list;
  }

  get value() {
    return this._value;
  }

  toString() {
    return this._value.toString();
  }

  add(values) {
    return new FizzBuzzList(this._value.concat(values));
  }
}

module.exports = FizzBuzzList;
