class FizzBuzzTest {
  test_greeting() {
    const expected = 'hello world';
    const actual = greeting();
    console.assert(actual === expected, `Expected ${expected}, but got ${actual}`);
  }
}

function greeting() {
  return 'hello world';
}

// テスト実行
const test = new FizzBuzzTest();
test.test_greeting();
console.log('テスト完了');
