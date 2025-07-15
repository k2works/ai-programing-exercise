import { FizzBuzz } from './src/domain/model/FizzBuzz';
import { FizzBuzzCommand } from './src/application/FizzBuzzCommand';

// メインエントリーポイント
function main() {
  // FizzBuzzCommandを使用したアプリケーション層での実行
  console.log('=== FizzBuzz Demo with Command Pattern ===');
  
  // タイプ1での実行
  const command = new FizzBuzzCommand(1);
  const result = command.executeList(15);
  console.log('Type 1 - First 15 elements:');
  console.log(result);
  
  // 統計情報の表示
  command.executeList(); // 全体のリスト生成
  const stats = command.getStatistics();
  console.log('Statistics:', stats);
  
  // 個別の値生成
  console.log('\nIndividual values:');
  for (let i = 1; i <= 15; i++) {
    console.log(`${i}: ${command.execute(i)}`);
  }

  // 直接FizzBuzzクラスを使用する例（従来の方法）
  console.log('\n=== Direct FizzBuzz Usage ===');
  const fizzbuzz1 = new FizzBuzz(1);
  fizzbuzz1.generateList(10);
  console.log('Type 1 - First 10 elements:');
  console.log(fizzbuzz1.list);
}

// Node.js環境での実行チェック
if (typeof require !== 'undefined' && require.main === module) {
  main();
}

export { main };
