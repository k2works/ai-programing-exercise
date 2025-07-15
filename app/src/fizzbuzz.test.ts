import { describe, it, expect, beforeEach, vi } from 'vitest';
import { FizzBuzz } from './fizzbuzz';

describe('FizzBuzz', () => {
  let fizzbuzz: typeof FizzBuzz;

  beforeEach(() => {
    fizzbuzz = FizzBuzz;
  });

  describe('数を文字列にして返す', () => {
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

  describe('タイプごとに出力を切り替えることができる', () => {
    describe('タイプ1の場合', () => {
      it('1を渡したら文字列"1"を返す', () => {
        expect(fizzbuzz.generate(1, 1)).toBe('1');
      });
    });
  });

  describe('1から100までの数', () => {
    it('1から100までのFizzBuzz配列を返す', () => {
      const result = fizzbuzz.generateRange(1, 100);

      expect(result[0]).toBe('1'); // 1
      expect(result[1]).toBe('2'); // 2
      expect(result[2]).toBe('Fizz'); // 3
      expect(result[4]).toBe('Buzz'); // 5
      expect(result[14]).toBe('FizzBuzz'); // 15
      expect(result.length).toBe(100);
    });
  });

  describe('プリント機能', () => {
    it('1から100までの結果をプリントする', () => {
      // コンソール出力をキャプチャするためのモック
      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      fizzbuzz.printRange(1, 100);

      expect(consoleSpy).toHaveBeenCalledTimes(100);
      expect(consoleSpy).toHaveBeenNthCalledWith(1, '1');
      expect(consoleSpy).toHaveBeenNthCalledWith(3, 'Fizz');
      expect(consoleSpy).toHaveBeenNthCalledWith(5, 'Buzz');
      expect(consoleSpy).toHaveBeenNthCalledWith(15, 'FizzBuzz');

      consoleSpy.mockRestore();
    });
  });
});
