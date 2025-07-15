// テスト用簡易スクリプト
import { FizzBuzzCommand } from './src/application/FizzBuzzCommand.js';

console.log('=== FizzBuzzCommand テスト ===');

const command = new FizzBuzzCommand(1);

console.log('単一数値テスト:');
console.log(`15 -> ${command.execute(15)}`);
console.log(`3 -> ${command.execute(3)}`);
console.log(`5 -> ${command.execute(5)}`);

console.log('\nリスト生成テスト (1-10):');
const list = command.executeList(10);
console.log(list);

console.log('\n統計情報テスト:');
command.executeList(100);
const stats = command.getStatistics();
console.log(stats);

console.log('\nFizzBuzzのみテスト:');
const fizzBuzzOnly = command.getFizzBuzzOnly();
console.log(`最初の5個: ${fizzBuzzOnly.slice(0, 5)}`);

console.log('\n=== テスト完了 ===');
