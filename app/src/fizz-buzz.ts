export class FizzBuzz {
  private static readonly MAX_NUMBER = 100

  public static generate(number: number): string {
    const isFizz = number % 3 === 0
    const isBuzz = number % 5 === 0

    if (isFizz && isBuzz) return 'FizzBuzz'
    if (isFizz) return 'Fizz'
    if (isBuzz) return 'Buzz'

    return number.toString()
  }

  public static generateList(): string[] {
    // 1から最大値までのFizzBuzz配列を1発で作る
    return Array.from({ length: this.MAX_NUMBER }, (_, i) => this.generate(i + 1))
  }
}
