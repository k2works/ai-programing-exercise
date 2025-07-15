// ファーストクラスコレクション
export class FizzBuzzList {
  private readonly values: readonly FizzBuzzValue[];

  constructor(values: FizzBuzzValue[] = []) {
    // 防御的コピーによる不変性確保
    this.values = Object.freeze([...values]);
  }

  // 要素の追加（新しいインスタンスを返す）
  add(value: FizzBuzzValue): FizzBuzzList {
    return new FizzBuzzList([...this.values, value]);
  }

  // 複数要素の追加
  addAll(values: FizzBuzzValue[]): FizzBuzzList {
    return new FizzBuzzList([...this.values, ...values]);
  }

  // 要素の取得
  get(index: number): FizzBuzzValue {
    if (index < 0 || index >= this.values.length) {
      throw new Error('Index out of bounds');
    }
    return this.values[index];
  }

  // 安全な要素取得（Optional的な動作）
  tryGet(index: number): FizzBuzzValue | undefined {
    if (index < 0 || index >= this.values.length) {
      return undefined;
    }
    return this.values[index];
  }

  // 長さ
  get length(): number {
    return this.values.length;
  }

  // 文字列配列として取得（後方互換性のため）
  toStringArray(): string[] {
    return this.values.map(v => v.value);
  }

  // iterator
  *[Symbol.iterator](): Generator<FizzBuzzValue, void, unknown> {
    for (const value of this.values) {
      yield value;
    }
  }

  // 範囲で作成（ファクトリメソッド）
  static createRange(fizzBuzzType: FizzBuzzType, start: number = 1, end: number = 100): FizzBuzzList {
    const values: FizzBuzzValue[] = [];
    for (let i = start; i <= end; i++) {
      values.push(fizzBuzzType.generate(i));
    }
    return new FizzBuzzList(values);
  }
}

// 値オブジェクト
export class FizzBuzzValue {
  private readonly _value: string;

  constructor(value: string) {
    this._value = value;
  }

  get value(): string {
    return this._value;
  }

  equals(other: FizzBuzzValue): boolean {
    return this._value === other._value;
  }

  toString(): string {
    return this._value;
  }
}

// 基底クラス
abstract class FizzBuzzType {
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
}

// タイプクラス
class FizzBuzzType01 extends FizzBuzzType {
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

class FizzBuzzType02 extends FizzBuzzType {
  generate(n: number): FizzBuzzValue {
    return new FizzBuzzValue(n.toString());
  }
}

class FizzBuzzType03 extends FizzBuzzType {
  generate(n: number): FizzBuzzValue {
    if (this.isFizzBuzz(n)) {
      return new FizzBuzzValue('FizzBuzz');
    }
    return new FizzBuzzValue(n.toString());
  }
}

export class FizzBuzz {
  private _list: FizzBuzzList = new FizzBuzzList();
  private readonly _type: FizzBuzzType;

  constructor(type: number = 1) {
    this._type = FizzBuzz.create(type);
  }

  static create(type: number): FizzBuzzType {
    switch (type) {
      case 1:
        return new FizzBuzzType01();
      case 2:
        return new FizzBuzzType02();
      case 3:
        return new FizzBuzzType03();
      default:
        throw new Error('該当するタイプは存在しません');
    }
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
      // 新しいポリモーフィズムの仕組みを使って簡素化
      const fizzBuzzType = FizzBuzz.create(type);
      return fizzBuzzType.generate(n).value;
    }
    
    // ポリモーフィズムを活用した処理
    return this._type.generate(n).value;
  }

  generateList(): void {
    // FizzBuzzListを使って新しいリストを作成
    this._list = FizzBuzzList.createRange(this._type, 1, 100);
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
