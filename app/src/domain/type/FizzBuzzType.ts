import { FizzBuzzValue } from '../model/FizzBuzzValue';
import { FizzBuzzList } from '../model/FizzBuzzList';

// カスタム例外クラス
export class InvalidTypeError extends Error {
  constructor(type: number) {
    super(`無効なタイプです: ${type}`);
    this.name = 'InvalidTypeError';
  }
}

// 基底クラス
export abstract class FizzBuzzType {
  abstract generate(n: number): FizzBuzzValue;

  // 共通メソッドをスーパークラスに移動（メソッド名を変更）
  protected isFizz(n: number): boolean {
    return n % 3 === 0;
  }

  protected isBuzz(n: number): boolean {
    return n % 5 === 0;
  }

  protected isFizzBuzz(n: number): boolean {
    return this.isFizz(n) && this.isBuzz(n);
  }

  // ファクトリメソッド
  static create(type: number): FizzBuzzType {
    switch (type) {
      case 1:
        return new FizzBuzzType01();
      case 2:
        return new FizzBuzzType02();
      case 3:
        return new FizzBuzzType03();
      default:
        throw new InvalidTypeError(type);
    }
  }

  // 範囲で作成（ファクトリメソッド）
  static createRange(type: FizzBuzzType, start: number = 1, end: number = 100): FizzBuzzList {
    const values: FizzBuzzValue[] = [];
    for (let i = start; i <= end; i++) {
      values.push(type.generate(i));
    }
    return new FizzBuzzList(values);
  }
}

// タイプクラス
export class FizzBuzzType01 extends FizzBuzzType {
  generate(n: number): FizzBuzzValue {
    if (this.isFizzBuzz(n)) {
      return new FizzBuzzValue('FizzBuzz');
    } else if (this.isFizz(n)) {
      return new FizzBuzzValue('Fizz');
    } else if (this.isBuzz(n)) {
      return new FizzBuzzValue('Buzz');
    }
    return new FizzBuzzValue(n.toString());
  }
}

export class FizzBuzzType02 extends FizzBuzzType {
  generate(n: number): FizzBuzzValue {
    return new FizzBuzzValue(n.toString());
  }
}

export class FizzBuzzType03 extends FizzBuzzType {
  generate(n: number): FizzBuzzValue {
    if (this.isFizzBuzz(n)) {
      return new FizzBuzzValue('FizzBuzz');
    }
    return new FizzBuzzValue(n.toString());
  }
}

// メインFizzBuzzクラス
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
      const fizzBuzzType = FizzBuzzType.create(type);
      return fizzBuzzType.generate(n).value;
    }
    
    // ポリモーフィズムを活用した処理
    return this._type.generate(n).value;
  }

  generateList(): void {
    // FizzBuzzListを使って新しいリストを作成
    this._list = FizzBuzzType.createRange(this._type, 1, 100);
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
