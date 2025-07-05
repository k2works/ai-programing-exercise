import { FizzBuzzValue } from '../model/FizzBuzzValue'

export abstract class FizzBuzzType {
  static readonly TYPE_01 = 1
  static readonly TYPE_02 = 2
  static readonly TYPE_03 = 3

  static create(type: number): FizzBuzzType {
    switch (type) {
      case FizzBuzzType.TYPE_01:
        return new FizzBuzzType01()
      case FizzBuzzType.TYPE_02:
        return new FizzBuzzType02()
      case FizzBuzzType.TYPE_03:
        return new FizzBuzzType03()
      default:
        return new FizzBuzzTypeNotDefined()
    }
  }

  abstract generate(number: number): FizzBuzzValue

  protected fizz(number: number): boolean {
    return number % 3 === 0
  }

  protected buzz(number: number): boolean {
    return number % 5 === 0
  }
}

export class FizzBuzzType01 extends FizzBuzzType {
  generate(number: number): FizzBuzzValue {
    if (this.fizz(number) && this.buzz(number)) {
      return new FizzBuzzValue('FizzBuzz')
    } else if (this.fizz(number)) {
      return new FizzBuzzValue('Fizz')
    } else if (this.buzz(number)) {
      return new FizzBuzzValue('Buzz')
    }

    return new FizzBuzzValue(number.toString())
  }
}

export class FizzBuzzType02 extends FizzBuzzType {
  generate(number: number): FizzBuzzValue {
    return new FizzBuzzValue(number.toString())
  }
}

export class FizzBuzzType03 extends FizzBuzzType {
  generate(number: number): FizzBuzzValue {
    if (this.fizz(number) && this.buzz(number)) {
      return new FizzBuzzValue('FizzBuzz')
    } else {
      return new FizzBuzzValue(number.toString())
    }
  }
}

export class FizzBuzzTypeNotDefined extends FizzBuzzType {
  generate(_number: number): FizzBuzzValue {
    throw new Error('該当するタイプは存在しません')
  }
}
