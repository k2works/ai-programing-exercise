import { FizzBuzz } from './fizzbuzz';

describe('FizzBuzzTest', () => {
  test('1を渡したら文字列1を返す', () => {
    expect(FizzBuzz.generate(1)).toBe('1');
  });

  test('2を渡したら文字列2を返す', () => {
    expect(FizzBuzz.generate(2)).toBe('2');
  });
});
