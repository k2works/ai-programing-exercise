import { FizzBuzz } from '../domain/model/FizzBuzz';

/**
 * FizzBuzzアプリケーションの実行コマンド
 * アプリケーション層の責務として、ドメインロジックの呼び出しを担当
 */
export class FizzBuzzCommand {
  private fizzbuzz: FizzBuzz;

  constructor(type: number = 1) {
    this.fizzbuzz = FizzBuzz.create(type);
  }

  /**
   * 単一の数値に対してFizzBuzz処理を実行
   * @param number 処理対象の数値
   * @returns FizzBuzz処理結果の文字列
   */
  execute(number: number): string {
    return this.fizzbuzz.generate(number);
  }

  /**
   * 1から指定された数値までのFizzBuzzリストを生成
   * @param max 最大値（デフォルト: 100）
   * @returns FizzBuzz処理結果の文字列配列
   */
  executeList(max: number = 100): string[] {
    this.fizzbuzz.generateList(max);
    return this.fizzbuzz.list;
  }

  /**
   * FizzBuzzリストの統計情報を取得
   * @returns 統計情報オブジェクト
   */
  getStatistics() {
    return this.fizzbuzz.fizzBuzzList.getStatistics();
  }

  /**
   * FizzBuzzのみの結果を取得
   * @returns FizzBuzzのみの配列
   */
  getFizzBuzzOnly(): string[] {
    const fizzBuzzList = this.fizzbuzz.fizzBuzzList.onlyFizzBuzz();
    return fizzBuzzList.toStringArray();
  }

  /**
   * タイプを変更して新しいコマンドを作成
   * @param type 新しいタイプ
   * @returns 新しいFizzBuzzCommandインスタンス
   */
  static withType(type: number): FizzBuzzCommand {
    return new FizzBuzzCommand(type);
  }
}
