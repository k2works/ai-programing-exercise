export abstract class FizzBuzzType {
  abstract generate(number: number): string

  protected fizz(number: number): boolean {
    return number % 3 === 0
  }

  protected buzz(number: number): boolean {
    return number % 5 === 0
  }
}

export class FizzBuzzType01 extends FizzBuzzType {
  generate(number: number): string {
    if (this.fizz(number) && this.buzz(number)) {
      return 'FizzBuzz'
    } else if (this.fizz(number)) {
      return 'Fizz'
    } else if (this.buzz(number)) {
      return 'Buzz'
    }

    return number.toString()
  }
}

export class FizzBuzzType02 extends FizzBuzzType {
  generate(number: number): string {
    return number.toString()
  }
}

export class FizzBuzzType03 extends FizzBuzzType {
  generate(number: number): string {
    if (this.fizz(number) && this.buzz(number)) {
      return 'FizzBuzz'
    } else {
      return number.toString()
    }
  }
}
