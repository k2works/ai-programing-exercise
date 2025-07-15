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
    let result = number.toString();
    if (this.isFizz(number) && this.isBuzz(number)) {
      result = 'FizzBuzz';
    } else if (this.isFizz(number)) {
      result = 'Fizz';
    } else if (this.isBuzz(number)) {
      result = 'Buzz';
    }
    return result;
  }
}

class FizzBuzzType02 extends FizzBuzzType {
  generate(number) {
    return number.toString();
  }
}

class FizzBuzzType03 extends FizzBuzzType {
  generate(number) {
    if (this.isFizz(number) && this.isBuzz(number)) {
      return 'FizzBuzz';
    } else if (this.isFizz(number)) {
      return 'Fizz';
    } else if (this.isBuzz(number)) {
      return 'Buzz';
    }
    return number.toString();
  }
}

module.exports = FizzBuzz;
