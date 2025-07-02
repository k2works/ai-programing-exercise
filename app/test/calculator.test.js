/**
 * Calculator クラスのテスト
 * テスト駆動開発の実践例
 */

import { Calculator } from '../src/index.js';

describe('Calculator', () => {
  let calculator;

  // 各テストの前にCalculatorインスタンスを作成
  beforeEach(() => {
    calculator = new Calculator();
  });

  describe('add method', () => {
    test('二つの正の数を足し算できる', () => {
      // Arrange（準備）
      const a = 2;
      const b = 3;
      const expected = 5;

      // Act（実行）
      const result = calculator.add(a, b);

      // Assert（検証）
      expect(result).toBe(expected);
    });

    test('負の数を含む足し算ができる', () => {
      expect(calculator.add(-1, 1)).toBe(0);
      expect(calculator.add(-2, -3)).toBe(-5);
    });

    test('ゼロを含む足し算ができる', () => {
      expect(calculator.add(0, 5)).toBe(5);
      expect(calculator.add(5, 0)).toBe(5);
    });

    test('小数点を含む足し算ができる', () => {
      expect(calculator.add(0.1, 0.2)).toBeCloseTo(0.3);
    });
  });

  describe('subtract method', () => {
    test('二つの数を引き算できる', () => {
      expect(calculator.subtract(5, 3)).toBe(2);
    });

    test('負の数を含む引き算ができる', () => {
      expect(calculator.subtract(-1, 1)).toBe(-2);
      expect(calculator.subtract(1, -1)).toBe(2);
    });

    test('同じ数の引き算はゼロになる', () => {
      expect(calculator.subtract(5, 5)).toBe(0);
    });
  });

  describe('multiply method', () => {
    test('二つの数を掛け算できる', () => {
      expect(calculator.multiply(3, 4)).toBe(12);
    });

    test('ゼロとの掛け算はゼロになる', () => {
      expect(calculator.multiply(5, 0)).toBe(0);
      expect(calculator.multiply(0, 5)).toBe(0);
    });

    test('負の数を含む掛け算ができる', () => {
      expect(calculator.multiply(-2, 3)).toBe(-6);
      expect(calculator.multiply(-2, -3)).toBe(6);
    });
  });

  describe('divide method', () => {
    test('二つの数を割り算できる', () => {
      expect(calculator.divide(10, 2)).toBe(5);
    });

    test('小数点の結果になる割り算ができる', () => {
      expect(calculator.divide(10, 3)).toBeCloseTo(3.333333);
    });

    test('負の数を含む割り算ができる', () => {
      expect(calculator.divide(-10, 2)).toBe(-5);
      expect(calculator.divide(10, -2)).toBe(-5);
      expect(calculator.divide(-10, -2)).toBe(5);
    });

    test('ゼロで割ろうとするとエラーが発生する', () => {
      expect(() => {
        calculator.divide(10, 0);
      }).toThrow('ゼロで割ることはできません');
    });
  });

  describe('TDDの実践例', () => {
    test('複数の計算を組み合わせた処理', () => {
      // より複雑な計算例: (2 + 3) * 4 / 2 = 10
      const step1 = calculator.add(2, 3); // 5
      const step2 = calculator.multiply(step1, 4); // 20
      const result = calculator.divide(step2, 2); // 10
      
      expect(result).toBe(10);
    });
  });
});
