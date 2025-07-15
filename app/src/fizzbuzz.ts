// カスタム例外クラス
export class InvalidTypeError extends Error {
  constructor(type: number) {
    super(`無効なタイプです: ${type}`);
    this.name = 'InvalidTypeError';
  }
}

export class IndexOutOfRangeError extends Error {
  constructor(index: number, length: number) {
    super(`インデックスが範囲外です: ${index} (配列サイズ: ${length})`);
    this.name = 'IndexOutOfRangeError';
  }
}

// コマンドパターンの実装
export interface Command<T> {
  execute(): T;
}

// FizzBuzz値生成コマンド
export class FizzBuzzValueCommand implements Command<FizzBuzzValue> {
  constructor(
    private readonly fizzBuzzType: FizzBuzzType,
    private readonly number: number
  ) {}

  execute(): FizzBuzzValue {
    return this.fizzBuzzType.generate(this.number);
  }
}

// FizzBuzzリスト生成コマンド
export class FizzBuzzListCommand implements Command<FizzBuzzList> {
  constructor(
    private readonly fizzBuzzType: FizzBuzzType,
    private readonly start: number,
    private readonly end: number
  ) {}

  execute(): FizzBuzzList {
    return FizzBuzzList.createRange(this.fizzBuzzType, this.start, this.end);
  }
}

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
      throw new IndexOutOfRangeError(index, this.values.length);
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

  // 統計情報を取得
  getStatistics(): { fizz: number; buzz: number; fizzBuzz: number; numbers: number } {
    const stats = { fizz: 0, buzz: 0, fizzBuzz: 0, numbers: 0 };
    for (const value of this.values) {
      if (value.isFizzBuzz()) stats.fizzBuzz++;
      else if (value.isFizz()) stats.fizz++;
      else if (value.isBuzz()) stats.buzz++;
      else stats.numbers++;
    }
    return stats;
  }

  // フィルタリング
  filter(predicate: (value: FizzBuzzValue) => boolean): FizzBuzzList {
    return new FizzBuzzList(this.values.filter(predicate));
  }

  // FizzBuzzのみを取得
  onlyFizzBuzz(): FizzBuzzList {
    return this.filter(value => value.isFizzBuzz());
  }

  // 文字列表現
  toString(): string {
    return this.values.map(v => v.toString()).join(', ');
  }

  // JSON表現
  toJSON(): string[] {
    return this.values.map(v => v.toJSON());
  }

  // 等価性
  equals(other: FizzBuzzList): boolean {
    if (!(other instanceof FizzBuzzList)) {
      return false;
    }
    if (this.length !== other.length) {
      return false;
    }
    for (let i = 0; i < this.length; i++) {
      if (!this.get(i).equals(other.get(i))) {
        return false;
      }
    }
    return true;
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

  // 追加のユーティリティメソッド
  isFizz(): boolean {
    return this._value === 'Fizz';
  }

  isBuzz(): boolean {
    return this._value === 'Buzz';
  }

  isFizzBuzz(): boolean {
    return this._value === 'FizzBuzz';
  }

  isNumber(): boolean {
    return !this.isFizz() && !this.isBuzz() && !this.isFizzBuzz();
  }

  // JSON表現
  toJSON(): string {
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
        throw new InvalidTypeError(type);
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
      // コマンドパターンを使って実装
      const fizzBuzzType = FizzBuzz.create(type);
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
