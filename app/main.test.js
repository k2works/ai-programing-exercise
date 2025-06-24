const { greeting, FizzBuzz } = require('./main');

// セットアップテスト
test('greeting returns hello world', () => {
  expect(greeting()).toBe('hello world');
});

// FizzBuzzテスト
describe('FizzBuzz', () => {
  let fizzbuzz;

  beforeEach(() => {
    fizzbuzz = FizzBuzz;
  });

  test('1を渡したら文字列1を返す', () => {
    expect(fizzbuzz.generate(1)).toBe('1');
  });

  test('2を渡したら文字列2を返す', () => {
    expect(fizzbuzz.generate(2)).toBe('2');
  });

  test('3を渡したら文字列Fizzを返す', () => {
    expect(fizzbuzz.generate(3)).toBe('Fizz');
  });

  test('5を渡したら文字列Buzzを返す', () => {
    expect(fizzbuzz.generate(5)).toBe('Buzz');
  });

  test('15を渡したら文字列FizzBuzzを返す', () => {
    expect(fizzbuzz.generate(15)).toBe('FizzBuzz');
  });

  test('1から100まで数をプリントする', () => {
    const spy = jest.spyOn(console, 'log').mockImplementation();
    fizzbuzz.printNumbers();
    
    expect(spy).toHaveBeenCalledTimes(100);
    expect(spy).toHaveBeenNthCalledWith(1, '1');
    expect(spy).toHaveBeenNthCalledWith(2, '2');
    expect(spy).toHaveBeenNthCalledWith(3, 'Fizz');
    expect(spy).toHaveBeenNthCalledWith(4, '4');
    expect(spy).toHaveBeenNthCalledWith(5, 'Buzz');
    expect(spy).toHaveBeenNthCalledWith(15, 'FizzBuzz');
    
    spy.mockRestore();
  });
});
