import { FizzBuzzValue } from './FizzBuzzValue';
import { FizzBuzzList } from './FizzBuzzList';
import { FizzBuzzType } from '../type/FizzBuzzType';

/**
 * FizzBuzzドメインサービス
 * FizzBuzzゲームのメインロジックを担当
 */
export class FizzBuzz {
  private _list: FizzBuzzList = new FizzBuzzList();
  private readonly _type: FizzBuzzType;

  constructor(type: number = 1) {
    this._type = FizzBuzzType.create(type);
  }

  static create(type: number): FizzBuzz {
    return new FizzBuzz(type);
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

  generate(n: number): string {
    return this._type.generate(n).value;
  }

  generateList(max: number = 100): void {
    let list = new FizzBuzzList();
    for (let i = 1; i <= max; i++) {
      const value = new FizzBuzzValue(this.generate(i));
      list = list.add(value);
    }
    this._list = list;
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
