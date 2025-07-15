export class FizzBuzz {
  static generate(n: number): string {
    if (n % 3 === 0) {
      return 'Fizz'
    }
    return n.toString()
  }
}
