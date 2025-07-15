import { describe, it, expect } from 'vitest';
import { FizzBuzzValue } from '../../../domain/model/FizzBuzzValue';

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

    it('toString() - 値を文字列として返す', () => {
      const value = new FizzBuzzValue('Test');
      expect(value.toString()).toBe('Test');
    });

    it('value プロパティで値を取得できる', () => {
      const value = new FizzBuzzValue('TestValue');
      expect(value.value).toBe('TestValue');
    });
  });
});
