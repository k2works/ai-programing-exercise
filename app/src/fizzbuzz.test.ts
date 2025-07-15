import { describe, it, expect, beforeEach } from 'vitest';
import { FizzBuzz, FizzBuzzValue, FizzBuzzList } from './fizzbuzz';

describe('FizzBuzz', () => {
  let fizzbuzz: FizzBuzz;

  beforeEach(() => {
    fizzbuzz = new FizzBuzz();
  });

  describe('数を文字列にして返す', () => {
    describe('タイプ1の場合', () => {
      beforeEach(() => {
        fizzbuzz = new FizzBuzz(1);
      });

      describe('その他の場合', () => {
        it('1を渡したら文字列"1"を返す', () => {
          expect(fizzbuzz.generate(1)).toBe('1');
        });

        it('2を渡したら文字列"2"を返す', () => {
          expect(fizzbuzz.generate(2)).toBe('2');
        });
      });

      describe('3の倍数の場合', () => {
        it('3を渡したら文字列"Fizz"を返す', () => {
          expect(fizzbuzz.generate(3)).toBe('Fizz');
        });

        it('6を渡したら文字列"Fizz"を返す', () => {
          expect(fizzbuzz.generate(6)).toBe('Fizz');
        });
      });

      describe('5の倍数の場合', () => {
        it('5を渡したら文字列"Buzz"を返す', () => {
          expect(fizzbuzz.generate(5)).toBe('Buzz');
        });
      });

      describe('3と5両方の倍数の場合', () => {
        it('15を渡したら文字列"FizzBuzz"を返す', () => {
          expect(fizzbuzz.generate(15)).toBe('FizzBuzz');
        });
      });

      describe('1から100までのFizzBuzzの配列を返す', () => {
        let result: string[];

        beforeEach(() => {
          const fb = new FizzBuzz(1);
          fb.generateList();
          result = fb.list;
        });

        it('配列の初めは文字列の1を返す', () => {
          expect(result[0]).toBe('1');
        });

        it('配列の最後は文字列のBuzzを返す', () => {
          expect(result[99]).toBe('Buzz'); // 100番目の要素
        });

        it('配列の3番目は文字列のFizzを返す', () => {
          expect(result[2]).toBe('Fizz'); // 3番目の要素
        });

        it('配列の5番目は文字列のBuzzを返す', () => {
          expect(result[4]).toBe('Buzz'); // 5番目の要素
        });

        it('配列の15番目は文字列のFizzBuzzを返す', () => {
          expect(result[14]).toBe('FizzBuzz'); // 15番目の要素
        });
      });
    });
  });

  describe('タイプごとに出力を切り替えることができる', () => {
    describe('タイプ1の場合', () => {
      beforeEach(() => {
        fizzbuzz = new FizzBuzz(1);
      });

      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzbuzz.generate(1)).toBe('1');
      });
    });

    describe('タイプ2の場合', () => {
      beforeEach(() => {
        fizzbuzz = new FizzBuzz(2);
      });

      describe('3の倍数の場合', () => {
        it('3を渡したら文字列"3"を返す', () => {
          expect(fizzbuzz.generate(3)).toBe('3');
        });
      });

      describe('5の倍数の場合', () => {
        it('5を渡したら文字列"5"を返す', () => {
          expect(fizzbuzz.generate(5)).toBe('5');
        });
      });

      describe('3と5両方の倍数の場合', () => {
        it('15を渡したら文字列"15"を返す', () => {
          expect(fizzbuzz.generate(15)).toBe('15');
        });
      });

      describe('その他の場合', () => {
        it('1を渡したら文字列"1"を返す', () => {
          expect(fizzbuzz.generate(1)).toBe('1');
        });
      });
    });

    describe('タイプ3の場合', () => {
      beforeEach(() => {
        fizzbuzz = new FizzBuzz(3);
      });

      describe('3の倍数の場合', () => {
        it('3を渡したら文字列"3"を返す', () => {
          expect(fizzbuzz.generate(3)).toBe('3');
        });
      });

      describe('5の倍数の場合', () => {
        it('5を渡したら文字列"5"を返す', () => {
          expect(fizzbuzz.generate(5)).toBe('5');
        });
      });

      describe('3と5両方の倍数の場合', () => {
        it('15を渡したら文字列"FizzBuzz"を返す', () => {
          expect(fizzbuzz.generate(15)).toBe('FizzBuzz');
        });
      });

      describe('その他の場合', () => {
        it('1を渡したら文字列"1"を返す', () => {
          expect(fizzbuzz.generate(1)).toBe('1');
        });
      });
    });

    describe('それ以外のタイプの場合', () => {
      it('1を渡したら例外を返す', () => {
        expect(() => FizzBuzz.create(4)).toThrow('該当するタイプは存在しません');
      });
    });
  });
});

describe('FizzBuzzValue', () => {
  describe('ユーティリティメソッド', () => {
    it('isFizz() - Fizzの場合にtrueを返す', () => {
      const value = new FizzBuzzValue('Fizz');
      expect(value.isFizz()).toBe(true);
      expect(value.isBuzz()).toBe(false);
      expect(value.isFizzBuzz()).toBe(false);
      expect(value.isNumber()).toBe(false);
    });

    it('isBuzz() - Buzzの場合にtrueを返す', () => {
      const value = new FizzBuzzValue('Buzz');
      expect(value.isFizz()).toBe(false);
      expect(value.isBuzz()).toBe(true);
      expect(value.isFizzBuzz()).toBe(false);
      expect(value.isNumber()).toBe(false);
    });

    it('isFizzBuzz() - FizzBuzzの場合にtrueを返す', () => {
      const value = new FizzBuzzValue('FizzBuzz');
      expect(value.isFizz()).toBe(false);
      expect(value.isBuzz()).toBe(false);
      expect(value.isFizzBuzz()).toBe(true);
      expect(value.isNumber()).toBe(false);
    });

    it('isNumber() - 数値の場合にtrueを返す', () => {
      const value = new FizzBuzzValue('1');
      expect(value.isFizz()).toBe(false);
      expect(value.isBuzz()).toBe(false);
      expect(value.isFizzBuzz()).toBe(false);
      expect(value.isNumber()).toBe(true);
    });
  });
});

describe('FizzBuzzList', () => {
  describe('統計情報', () => {
    it('getStatistics() - 統計情報を正しく取得する', () => {
      const fizzbuzz = new FizzBuzz(1);
      fizzbuzz.generateList();
      const list = fizzbuzz.fizzBuzzList;
      const stats = list.getStatistics();
      
      expect(stats.fizz).toBe(27); // 3の倍数（15の倍数を除く）
      expect(stats.buzz).toBe(14); // 5の倍数（15の倍数を除く）
      expect(stats.fizzBuzz).toBe(6); // 15の倍数
      expect(stats.numbers).toBe(53); // その他の数値
    });
  });

  describe('フィルタリング', () => {
    it('onlyFizzBuzz() - FizzBuzzのみを取得する', () => {
      const fizzbuzz = new FizzBuzz(1);
      fizzbuzz.generateList();
      const list = fizzbuzz.fizzBuzzList;
      const fizzBuzzOnly = list.onlyFizzBuzz();
      
      expect(fizzBuzzOnly.length).toBe(6);
      for (let i = 0; i < fizzBuzzOnly.length; i++) {
        expect(fizzBuzzOnly.get(i).isFizzBuzz()).toBe(true);
      }
    });
  });
});
