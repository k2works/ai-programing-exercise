const FizzBuzz = require('./main');

describe('FizzBuzz', () => {
  let fizzbuzz;

  beforeEach(() => {
    fizzbuzz = new FizzBuzz();
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

  describe('1から100までの数', () => {
    test('1から100までのFizzBuzz数列を生成する', () => {
      const result = fizzbuzz.printRange(1, 100);
      expect(result).toContain('1');
      expect(result).toContain('2');
      expect(result).toContain('Fizz');
      expect(result).toContain('4');
      expect(result).toContain('Buzz');
      expect(result).toContain('Fizz');
      expect(result).toContain('7');
      expect(result).toContain('8');
      expect(result).toContain('Fizz');
      expect(result).toContain('Buzz');
    });

    test('1から100までのFizzBuzz配列を返す', () => {
      fizzbuzz.generateList();
      const result = fizzbuzz.list;
      expect(result[0]).toBe('1');
      expect(result[1]).toBe('2');
      expect(result[2]).toBe('Fizz');
      expect(result[14]).toBe('FizzBuzz');
      expect(result.length).toBe(100);
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

  describe('タイプごとに出力を切り替えることができる', () => {
    describe('タイプ1の場合', () => {
      test('1を渡したら文字列1を返す', () => {
        expect(fizzbuzz.generate(1, 1)).toBe('1');
      });
    });

    describe('タイプ2の場合', () => {
      test('1を渡したら文字列1を返す', () => {
        expect(fizzbuzz.generate(1, 2)).toBe('1');
      });

      test('3を渡したら文字列3を返す', () => {
        expect(fizzbuzz.generate(3, 2)).toBe('3');
      });

      test('5を渡したら文字列5を返す', () => {
        expect(fizzbuzz.generate(5, 2)).toBe('5');
      });
    });

    describe('タイプ3の場合', () => {
      test('1を渡したら文字列1を返す', () => {
        expect(fizzbuzz.generate(1, 3)).toBe('1');
      });

      test('3を渡したら文字列Fizzを返す', () => {
        expect(fizzbuzz.generate(3, 3)).toBe('Fizz');
      });

      test('5を渡したら文字列Buzzを返す', () => {
        expect(fizzbuzz.generate(5, 3)).toBe('Buzz');
      });

      test('15を渡したら文字列FizzBuzzを返す', () => {
        expect(fizzbuzz.generate(15, 3)).toBe('FizzBuzz');
      });
    });

    describe('それ以外のタイプの場合', () => {
      test('存在しないタイプを指定した場合はエラーを返す', () => {
        expect(() => fizzbuzz.generate(1, 4)).toThrow();
      });
    });
  });
});
