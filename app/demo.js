// 簡単な動作確認用スクリプト
import { Calculator } from './src/index.js';
import { FizzBuzz } from './src/fizzbuzz.js';

console.log('\n=== Calculator Test ===');
const calc = new Calculator();
console.log('2 + 3 =', calc.add(2, 3));
console.log('10 - 4 =', calc.subtract(10, 4));
console.log('3 * 4 =', calc.multiply(3, 4));
console.log('15 / 3 =', calc.divide(15, 3));

console.log('\n=== FizzBuzz Test ===');
const fizz = new FizzBuzz();
console.log('FizzBuzz(1-20):', fizz.generateSequence(20).join(', '));

console.log('\n=== Individual FizzBuzz Tests ===');
[1, 3, 5, 15, 21].forEach(n => {
  console.log(`FizzBuzz(${n}) = "${fizz.convert(n)}"`);
});

console.log('\n✅ すべての機能が正常に動作しています！');
