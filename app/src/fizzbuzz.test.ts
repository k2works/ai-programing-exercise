import { describe, it, expect, beforeEach, vi } from 'vitest';
import { FizzBuzz } from './fizzbuzz';

describe('FizzBuzz', () => {
  let fizzbuzz: typeof FizzBuzz;

  beforeEach(() => {
    fizzbuzz = FizzBuzz;
  });

  describe('数を文字列にして返す', () => {
    describe('タイプ1の場合', () => {
      describe('その他の場合', () => {
        it('1を渡したら文字列"1"を返す', () => {
          expect(fizzbuzz.generate(1, 1)).toBe('1');
        });

        it('2を渡したら文字列"2"を返す', () => {
          expect(fizzbuzz.generate(2, 1)).toBe('2');
        });
      });

      describe('3の倍数の場合', () => {
        it('3を渡したら文字列"Fizz"を返す', () => {
          expect(fizzbuzz.generate(3, 1)).toBe('Fizz');
        });

        it('6を渡したら文字列"Fizz"を返す', () => {
          expect(fizzbuzz.generate(6, 1)).toBe('Fizz');
        });
      });

      describe('5の倍数の場合', () => {
        it('5を渡したら文字列"Buzz"を返す', () => {
          expect(fizzbuzz.generate(5, 1)).toBe('Buzz');
        });
      });

      describe('3と5両方の倍数の場合', () => {
        it('15を渡したら文字列"FizzBuzz"を返す', () => {
          expect(fizzbuzz.generate(15, 1)).toBe('FizzBuzz');
        });
      });

      describe('1から100までのFizzBuzzの配列を返す', () => {
        it('配列の初めは文字列の1を返す', () => {
          const result = fizzbuzz.generateRange(1, 100);
          expect(result[0]).toBe('1');
        });

        it('配列の最後は文字列のBuzzを返す', () => {
          const result = fizzbuzz.generateRange(1, 100);
          expect(result[99]).toBe('Buzz'); // 100番目の要素
        });

        it('配列の3番目は文字列のFizzを返す', () => {
          const result = fizzbuzz.generateRange(1, 100);
          expect(result[2]).toBe('Fizz'); // 3番目の要素
        });

        it('配列の5番目は文字列のBuzzを返す', () => {
          const result = fizzbuzz.generateRange(1, 100);
          expect(result[4]).toBe('Buzz'); // 5番目の要素
        });

        it('配列の15番目は文字列のFizzBuzzを返す', () => {
          const result = fizzbuzz.generateRange(1, 100);
          expect(result[14]).toBe('FizzBuzz'); // 15番目の要素
        });
      });
    });
  });

  describe('タイプごとに出力を切り替えることができる', () => {
    describe('タイプ1の場合', () => {
      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzbuzz.generate(1, 1)).toBe('1');
      });
    });

    describe('タイプ2の場合', () => {
      describe('3の倍数の場合', () => {
        it('3を渡したら文字列"3"を返す', () => {
          expect(fizzbuzz.generate(3, 2)).toBe('3');
        });
      });

      describe('5の倍数の場合', () => {
        it('5を渡したら文字列"5"を返す', () => {
          expect(fizzbuzz.generate(5, 2)).toBe('5');
        });
      });

      describe('3と5両方の倍数の場合', () => {
        it('15を渡したら文字列"15"を返す', () => {
          expect(fizzbuzz.generate(15, 2)).toBe('15');
        });
      });

      describe('その他の場合', () => {
        it('1を渡したら文字列"1"を返す', () => {
          expect(fizzbuzz.generate(1, 2)).toBe('1');
        });
      });
    });

    describe('タイプ3の場合', () => {
      describe('3の倍数の場合', () => {
        it('3を渡したら文字列"3"を返す', () => {
          expect(fizzbuzz.generate(3, 3)).toBe('3');
        });
      });

      describe('5の倍数の場合', () => {
        it('5を渡したら文字列"5"を返す', () => {
          expect(fizzbuzz.generate(5, 3)).toBe('5');
        });
      });

      describe('3と5両方の倍数の場合', () => {
        it('15を渡したら文字列"FizzBuzz"を返す', () => {
          expect(fizzbuzz.generate(15, 3)).toBe('FizzBuzz');
        });
      });

      describe('その他の場合', () => {
        it('1を渡したら文字列"1"を返す', () => {
          expect(fizzbuzz.generate(1, 3)).toBe('1');
        });
      });
    });
  });
});
