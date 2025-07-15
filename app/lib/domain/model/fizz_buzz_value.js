class FizzBuzzValue {
  constructor(number, value) {
    if (number < 0) {
      throw new Error('正の値のみ有効です');
    }
    this._number = number;
    this._value = value;
  }

  get number() {
    return this._number;
  }

  get value() {
    return this._value;
  }

  toString() {
    return `${this._number}:${this._value}`;
  }

  equals(other) {
    return this._number === other.number && this._value === other.value;
  }
}

module.exports = FizzBuzzValue;
