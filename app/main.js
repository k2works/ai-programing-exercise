// テスト関数のための簡単な例から始める
function greeting() {
  return 'hello world';
}

// FizzBuzzクラス
class FizzBuzz {
  static generate(number) {
    if (number % 3 === 0 && number % 5 === 0) {
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
}

module.exports = { greeting, FizzBuzz };
