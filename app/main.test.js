const FizzBuzz = require('./main');

describe('FizzBuzz', () => {
  let fizzbuzz;

  beforeEach(() => {
    fizzbuzz = FizzBuzz;
  });

  describe('三の倍数の場合', () => {
    test('3を渡したら文字列Fizzを返す', () => {
      expect(fizzbuzz.generate(3)).toBe('Fizz');
    });
  });

  describe('五の倍数の場合', () => {
    test('5を渡したら文字列Buzzを返す', () => {
      expect(fizzbuzz.generate(5)).toBe('Buzz');
    });
  });

  describe('三と五の倍数の場合', () => {
    test('15を渡したら文字列FizzBuzzを返す', () => {
      expect(fizzbuzz.generate(15)).toBe('FizzBuzz');
    });
  });

  describe('その他の場合', () => {
    test('1を渡したら文字列1を返す', () => {
      expect(fizzbuzz.generate(1)).toBe('1');
    });

    test('2を渡したら文字列2を返す', () => {
      expect(fizzbuzz.generate(2)).toBe('2');
    });
  });
});
