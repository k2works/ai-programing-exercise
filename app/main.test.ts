function greeting(): string {
  return 'hello world';
}

describe('HelloTest', () => {
  test('test_greeting', () => {
    expect(greeting()).toBe('hello world');
  });
});
