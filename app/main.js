// テスト関数のための簡単な例から始める
function greeting() {
  return 'hello world';
}

// FizzBuzzクラス
class FizzBuzz {
  static generate(number) {
    if (number % 3 === 0) {
      return 'Fizz';
    }
    return number.toString();
  }
}

module.exports = { greeting, FizzBuzz };
