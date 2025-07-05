export class FizzBuzz {
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

  generateList(start: number, end: number): string[] {
    const result: string[] = []
    for (let i = start; i <= end; i++) {
      result.push(this.generate(i))
    }
    return result
  }
}
