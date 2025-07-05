export class FizzBuzz {
  generate(number: number, type: number = 1): string {
    switch (type) {
      case 1:
        let result = number.toString()

        if (number % 3 === 0 && number % 5 === 0) {
          result = 'FizzBuzz'
        } else if (number % 3 === 0) {
          result = 'Fizz'
        } else if (number % 5 === 0) {
          result = 'Buzz'
        }

        return result
      default:
        return number.toString()
    }
  }

  generateList(start: number, end: number): string[] {
    const result: string[] = []
    for (let i = start; i <= end; i++) {
      result.push(this.generate(i))
    }
    return result
  }
}
