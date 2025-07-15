const FizzBuzz = require('./main');

describe('FizzBuzz', () => {
  describe('タイプ1の場合', () => {
    let fizzbuzz;

    beforeEach(() => {
      fizzbuzz = new FizzBuzz(1);
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
  });

  describe('タイプ2の場合', () => {
    let fizzbuzz;

    beforeEach(() => {
      fizzbuzz = new FizzBuzz(2);
    });

    describe('三の倍数の場合', () => {
      test('3を渡したら文字列3を返す', () => {
        expect(fizzbuzz.generate(3)).toBe('3');
      });
    });

    describe('五の倍数の場合', () => {
      test('5を渡したら文字列5を返す', () => {
        expect(fizzbuzz.generate(5)).toBe('5');
      });
    });

    describe('三と五の倍数の場合', () => {
      test('15を渡したら文字列15を返す', () => {
        expect(fizzbuzz.generate(15)).toBe('15');
      });
    });

    describe('その他の場合', () => {
      test('1を渡したら文字列1を返す', () => {
        expect(fizzbuzz.generate(1)).toBe('1');
      });
    });
  });

  describe('タイプ3の場合', () => {
    let fizzbuzz;

    beforeEach(() => {
      fizzbuzz = new FizzBuzz(3);
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
    });
  });

  describe('それ以外のタイプの場合', () => {
    test('存在しないタイプを指定した場合はエラーを返す', () => {
      expect(() => {
        const fizzbuzz = new FizzBuzz(4);
        fizzbuzz.generate(1);
      }).toThrow('該当するタイプは存在しません');
    });
  });
});
