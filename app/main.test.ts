import { FizzBuzz } from './fizzbuzz';

describe('FizzBuzzTest', () => {
  let fizzbuzz: typeof FizzBuzz;

  beforeEach(() => {
    fizzbuzz = FizzBuzz;
  });

  test('1を渡したら文字列1を返す', () => {
    expect(fizzbuzz.generate(1)).toBe('1');
  });

  test('2を渡したら文字列2を返す', () => {
    expect(fizzbuzz.generate(2)).toBe('2');
  });
});
