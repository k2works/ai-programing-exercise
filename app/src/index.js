/**
 * テスト駆動開発から始めるJavaScript入門
 * メインエントリーポイント
 */

console.log('テスト駆動開発から始めるJavaScript入門へようこそ！');
console.log('ソフトウェア開発の三種の神器を準備しました：');
console.log('1. バージョン管理システム（Git）');
console.log('2. テスティングフレームワーク（Jest）');
console.log('3. タスクランナー（npm scripts）');

// 基本的なクラス例
export class Calculator {
  /**
   * 二つの数値を足し算する
   * @param {number} a 
   * @param {number} b 
   * @returns {number} 計算結果
   */
  add(a, b) {
    return a + b;
  }

  /**
   * 二つの数値を引き算する
   * @param {number} a 
   * @param {number} b 
   * @returns {number} 計算結果
   */
  subtract(a, b) {
    return a - b;
  }

  /**
   * 二つの数値を掛け算する
   * @param {number} a 
   * @param {number} b 
   * @returns {number} 計算結果
   */
  multiply(a, b) {
    return a * b;
  }

  /**
   * 二つの数値を割り算する
   * @param {number} a 
   * @param {number} b 
   * @returns {number} 計算結果
   * @throws {Error} ゼロ除算エラー
   */
  divide(a, b) {
    if (b === 0) {
      throw new Error('ゼロで割ることはできません');
    }
    return a / b;
  }
}
