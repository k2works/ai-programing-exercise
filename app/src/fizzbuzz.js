/**
 * FizzBuzz問題の実装
 * TDD（Test Driven Development）の実践例
 */

export class FizzBuzz {
  /**
   * 数値を受け取り、FizzBuzzのルールに従って文字列を返す
   * - 3の倍数の場合は "Fizz"
   * - 5の倍数の場合は "Buzz" 
   * - 3と5の倍数の場合は "FizzBuzz"
   * - それ以外の場合は数値をそのまま文字列として返す
   * 
   * @param {number} number 判定する数値
   * @returns {string} FizzBuzzの結果
   */
  convert(number) {
    if (number % 15 === 0) {
      return 'FizzBuzz';
    }
    if (number % 3 === 0) {
      return 'Fizz';
    }
    if (number % 5 === 0) {
      return 'Buzz';
    }
    return number.toString();
  }

  /**
   * 1から指定した数までのFizzBuzz結果を配列で返す
   * @param {number} max 最大値
   * @returns {string[]} FizzBuzz結果の配列
   */
  generateSequence(max) {
    const result = [];
    for (let i = 1; i <= max; i++) {
      result.push(this.convert(i));
    }
    return result;
  }
}
