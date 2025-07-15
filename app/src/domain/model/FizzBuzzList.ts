import { FizzBuzzValue } from './FizzBuzzValue';

// カスタム例外クラス
export class IndexOutOfRangeError extends Error {
  constructor(index: number, length: number) {
    super(`インデックスが範囲外です: ${index} (配列サイズ: ${length})`);
    this.name = 'IndexOutOfRangeError';
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
