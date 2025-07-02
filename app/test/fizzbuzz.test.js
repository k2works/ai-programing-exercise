/**
 * FizzBuzz クラスのテスト
 * TDD（Red-Green-Refactor）サイクルの実践例
 */

import { FizzBuzz } from '../src/fizzbuzz.js';

describe('FizzBuzz', () => {
  let fizzBuzz;

  beforeEach(() => {
    fizzBuzz = new FizzBuzz();
  });

  describe('convert method', () => {
    describe('通常の数値の場合', () => {
      test('1を渡すと"1"を返す', () => {
        expect(fizzBuzz.convert(1)).toBe('1');
      });

      test('2を渡すと"2"を返す', () => {
        expect(fizzBuzz.convert(2)).toBe('2');
      });

      test('4を渡すと"4"を返す', () => {
        expect(fizzBuzz.convert(4)).toBe('4');
      });
    });

    describe('3の倍数の場合', () => {
      test('3を渡すと"Fizz"を返す', () => {
        expect(fizzBuzz.convert(3)).toBe('Fizz');
      });

      test('6を渡すと"Fizz"を返す', () => {
        expect(fizzBuzz.convert(6)).toBe('Fizz');
      });

      test('9を渡すと"Fizz"を返す', () => {
        expect(fizzBuzz.convert(9)).toBe('Fizz');
      });

      test('12を渡すと"Fizz"を返す', () => {
        expect(fizzBuzz.convert(12)).toBe('Fizz');
      });
    });

    describe('5の倍数の場合', () => {
      test('5を渡すと"Buzz"を返す', () => {
        expect(fizzBuzz.convert(5)).toBe('Buzz');
      });

      test('10を渡すと"Buzz"を返す', () => {
        expect(fizzBuzz.convert(10)).toBe('Buzz');
      });

      test('20を渡すと"Buzz"を返す', () => {
        expect(fizzBuzz.convert(20)).toBe('Buzz');
      });
    });

    describe('3と5の倍数（15の倍数）の場合', () => {
      test('15を渡すと"FizzBuzz"を返す', () => {
        expect(fizzBuzz.convert(15)).toBe('FizzBuzz');
      });

      test('30を渡すと"FizzBuzz"を返す', () => {
        expect(fizzBuzz.convert(30)).toBe('FizzBuzz');
      });

      test('45を渡すと"FizzBuzz"を返す', () => {
        expect(fizzBuzz.convert(45)).toBe('FizzBuzz');
      });
    });

    describe('境界値テスト', () => {
      test('0を渡すと"FizzBuzz"を返す', () => {
        expect(fizzBuzz.convert(0)).toBe('FizzBuzz');
      });

      test('負の数-3を渡すと"Fizz"を返す', () => {
        expect(fizzBuzz.convert(-3)).toBe('Fizz');
      });

      test('負の数-5を渡すと"Buzz"を返す', () => {
        expect(fizzBuzz.convert(-5)).toBe('Buzz');
      });

      test('負の数-15を渡すと"FizzBuzz"を返す', () => {
        expect(fizzBuzz.convert(-15)).toBe('FizzBuzz');
      });
    });
  });

  describe('generateSequence method', () => {
    test('1から5までのシーケンスを生成できる', () => {
      const expected = ['1', '2', 'Fizz', '4', 'Buzz'];
      expect(fizzBuzz.generateSequence(5)).toEqual(expected);
    });

    test('1から15までのシーケンスを生成できる', () => {
      const expected = [
        '1', '2', 'Fizz', '4', 'Buzz',
        'Fizz', '7', '8', 'Fizz', 'Buzz',
        '11', 'Fizz', '13', '14', 'FizzBuzz'
      ];
      expect(fizzBuzz.generateSequence(15)).toEqual(expected);
    });

    test('1から1までのシーケンスを生成できる', () => {
      expect(fizzBuzz.generateSequence(1)).toEqual(['1']);
    });

    test('0を指定すると空の配列を返す', () => {
      expect(fizzBuzz.generateSequence(0)).toEqual([]);
    });
  });

  describe('TDDサイクルの確認', () => {
    test('Red-Green-Refactorサイクルが正しく動作している', () => {
      // このテストは、TDDサイクルが正常に機能していることを確認する
      // 実際の開発では、以下の順序で実装する：
      // 1. Red: 失敗するテストを書く
      // 2. Green: テストを通すための最小限のコードを書く  
      // 3. Refactor: コードを改善する
      
      const testCases = [
        { input: 1, expected: '1' },
        { input: 3, expected: 'Fizz' },
        { input: 5, expected: 'Buzz' },
        { input: 15, expected: 'FizzBuzz' },
        { input: 7, expected: '7' }
      ];

      testCases.forEach(({ input, expected }) => {
        expect(fizzBuzz.convert(input)).toBe(expected);
      });
    });
  });
});
