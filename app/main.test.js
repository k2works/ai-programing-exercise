const { greeting } = require('./main');

// テスト
test('greeting returns hello world', () => {
  expect(greeting()).toBe('hello world');
});
