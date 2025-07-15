class FizzBuzz {
  constructor(type = 1) {
    this.MAX_NUMBER = 100;
    this._type = FizzBuzz.create(type);
    this._list = [];
  }

  get list() {
    return this._list;
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
        throw new Error('該当するタイプは存在しません');
    }
  }

  generate(number) {
    return this._type.generate(number);
  }

  generateList() {
    this._list = [];
    for (let i = 1; i <= this.MAX_NUMBER; i++) {
      this._list.push(this._type.generate(i));
    }
  }

  printRange(min, max) {
    const results = [];
    for (let i = min; i <= max; i++) {
      results.push(this.generate(i));
    }
    return results.join('\n');
  }
}

class FizzBuzzType01 {
  generate(number) {
    let result = number.toString();
    if (number % 3 === 0 && number % 5 === 0) {
      result = 'FizzBuzz';
    } else if (number % 3 === 0) {
      result = 'Fizz';
    } else if (number % 5 === 0) {
      result = 'Buzz';
    }
    return result;
  }
}

class FizzBuzzType02 {
  generate(number) {
    return number.toString();
  }
}

class FizzBuzzType03 {
  generate(number) {
    if (number % 3 === 0 && number % 5 === 0) {
      return 'FizzBuzz';
    } else if (number % 3 === 0) {
      return 'Fizz';
    } else if (number % 5 === 0) {
      return 'Buzz';
    }
    return number.toString();
  }
}

module.exports = FizzBuzz;
