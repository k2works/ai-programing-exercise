/**
 * FizzBuzzクラス
 * 数値を受け取ってFizzBuzzの結果を返すクラス
 */
export class FizzBuzz {
  static readonly MAX_NUMBER = 100

  /**
   * 数値を受け取ってFizzBuzzの結果を返す
   * @param number - 変換する数値
   * @returns FizzBuzz結果の文字列
   */
  static generate(number: number): string {
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

  /**
   * 指定した範囲のFizzBuzz配列を生成する
   * @param start - 開始数値
   * @param end - 終了数値
   * @returns FizzBuzz結果の配列
   */
  static generateList(start: number, end: number): string[] {
    const result: string[] = []
    for (let i = start; i <= end; i++) {
      result.push(this.generate(i))
    }
    return result
  }

  /**
   * 1から最大値までのFizzBuzz配列を生成する
   * @returns FizzBuzz結果の配列
   */
  static generateListToMax(): string[] {
    return Array.from({ length: this.MAX_NUMBER }, (_, i) => this.generate(i + 1))
  }
}
