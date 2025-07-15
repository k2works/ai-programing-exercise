export class FizzBuzz {
  static generate(n: number): string {
    if (n % 3 === 0) {
      return 'Fizz'
    } else if (n % 5 === 0) {
      return 'Buzz'
    }
    return n.toString()
  }
}
