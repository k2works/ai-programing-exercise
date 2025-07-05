export class FizzBuzz {
  generate(number: number): string {
    let result = number.toString()
    
    if (number % 3 === 0) {
      result = 'Fizz'
    } else if (number % 5 === 0) {
      result = 'Buzz'
    }
    
    return result
  }
}
