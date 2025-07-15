class Assertions {
  static assert(condition, message = 'Assertion Failed') {
    if (!condition) {
      throw new Error(message);
    }
  }
}

class FizzBuzz {
  constructor(type = 1) {
    this.MAX_NUMBER = 100;
    this._type = FizzBuzz.create(type);
    this._list = new FizzBuzzList([]);
  }

  get list() {
    return this._list.value;
  }

  static create(type) {
    switch (type) {
      case 1:
        return new FizzBuzzType01();
      case 2:
        return new FizzBuzzType02();
      case 3:
        return new FizzBuzzType03();
      default:
        return new FizzBuzzTypeNotDefined();
    }
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

class FizzBuzzType {
  isFizz(number) {
    return number % 3 === 0;
  }

  isBuzz(number) {
    return number % 5 === 0;
  }
}

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

class FizzBuzzType02 extends FizzBuzzType {
  generate(number) {
    return new FizzBuzzValue(number, number.toString());
  }
}

class FizzBuzzType03 extends FizzBuzzType {
  generate(number) {
    if (this.isFizz(number) && this.isBuzz(number)) {
      return new FizzBuzzValue(number, 'FizzBuzz');
    }
    return new FizzBuzzValue(number, number.toString());
  }
}

class FizzBuzzTypeNotDefined extends FizzBuzzType {
  generate(number) {
    return new FizzBuzzValue(number, '未定義');
  }

  toString() {
    return '未定義';
  }
}

class FizzBuzzCommand {
  execute() {
    throw new Error('execute method must be implemented');
  }
}

class FizzBuzzValueCommand extends FizzBuzzCommand {
  constructor(type) {
    super();
    this._type = type;
  }

  execute(number) {
    return this._type.generate(number).value;
  }
}

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

module.exports = { 
  FizzBuzz, 
  FizzBuzzValue, 
  FizzBuzzList, 
  FizzBuzzCommand, 
  FizzBuzzValueCommand,
  FizzBuzzType01,
  FizzBuzzType02, 
  FizzBuzzType03,
  FizzBuzzListCommand
};
