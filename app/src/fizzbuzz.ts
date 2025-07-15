// 各モジュールをインポート
import { FizzBuzzValue } from './value-objects/fizzbuzz-value';
import { FizzBuzzList } from './collections/fizzbuzz-list';
import { FizzBuzzType } from './types/fizzbuzz-type';
import { FizzBuzzValueCommand, FizzBuzzListCommand } from './commands/fizzbuzz-commands';

// 再エクスポート（後方互換性のため）
export { FizzBuzzValue } from './value-objects/fizzbuzz-value';
export { FizzBuzzList } from './collections/fizzbuzz-list';
export { FizzBuzzType } from './types/fizzbuzz-type';
export { InvalidTypeError, IndexOutOfRangeError } from './exceptions/custom-errors';

export class FizzBuzz {
  private _list: FizzBuzzList = new FizzBuzzList();
  private readonly _type: FizzBuzzType;

  constructor(type: number = 1) {
    this._type = FizzBuzzType.create(type);
  }

  static create(type: number): FizzBuzzType {
    return FizzBuzzType.create(type);
  }

  get list(): string[] {
    return this._list.toStringArray(); // 文字列配列として返す（後方互換性）
  }

  // FizzBuzzListとして取得する新しいアクセサ
  get fizzBuzzList(): FizzBuzzList {
    return this._list;
  }

  get type(): FizzBuzzType {
    return this._type;
  }

  generate(n: number, type?: number): string {
    if (type !== undefined) {
      // 後方互換性のために type パラメータが指定された場合の処理
      // コマンドパターンを使って実装
      const fizzBuzzType = FizzBuzzType.create(type);
      const command = new FizzBuzzValueCommand(fizzBuzzType, n);
      return command.execute().value;
    }
    
    // コマンドパターンを活用した処理
    const command = new FizzBuzzValueCommand(this._type, n);
    return command.execute().value;
  }

  generateList(): void {
    // コマンドパターンを使ってリスト生成
    const command = new FizzBuzzListCommand(this._type, 1, 100);
    this._list = command.execute();
  }

  // 後方互換性のための静的メソッド
  static generate(n: number, type: number = 1): string {
    const fizzbuzz = new FizzBuzz(type);
    return fizzbuzz.generate(n);
  }

  static generateRange(start: number, end: number): string[] {
    const result: string[] = [];
    for (let i = start; i <= end; i++) {
      result.push(this.generate(i));
    }
    return result;
  }

  static printRange(start: number, end: number): void {
    for (let i = start; i <= end; i++) {
      console.log(this.generate(i));
    }
  }
}
