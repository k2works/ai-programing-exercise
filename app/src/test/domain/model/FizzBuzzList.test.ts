import { describe, it, expect, beforeEach } from 'vitest';
import { FizzBuzz } from '../../../domain/model/FizzBuzz';
import { FizzBuzzList } from '../../../domain/model/FizzBuzzList';

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

  describe('基本操作', () => {
    let list: FizzBuzzList;

    beforeEach(() => {
      list = new FizzBuzzList();
    });

    it('空のリストを作成できる', () => {
      expect(list.length).toBe(0);
    });

    it('toStringArray() - 文字列配列を取得できる', () => {
      const fizzbuzz = new FizzBuzz(1);
      fizzbuzz.generateList(5);
      const list = fizzbuzz.fizzBuzzList;
      const stringArray = list.toStringArray();
      
      expect(stringArray).toEqual(['1', '2', 'Fizz', '4', 'Buzz']);
    });
  });
});
