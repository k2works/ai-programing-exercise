export abstract class FizzBuzzType {
  abstract generate(number: number): string
}

export class FizzBuzzType01 extends FizzBuzzType {
  generate(number: number): string {
    let result = number.toString()

    if (number % 3 === 0 && number % 5 === 0) {
      result = 'FizzBuzz'
    } else if (number % 3 === 0) {
      result = 'Fizz'
    } else if (number % 5 === 0) {
      result = 'Buzz'
    }

    return result
  }
}

export class FizzBuzzType02 extends FizzBuzzType {
  generate(number: number): string {
    return number.toString()
  }
}

export class FizzBuzzType03 extends FizzBuzzType {
  generate(number: number): string {
    if (number % 3 === 0 && number % 5 === 0) {
      return 'FizzBuzz'
    } else {
      return number.toString()
    }
  }
}
