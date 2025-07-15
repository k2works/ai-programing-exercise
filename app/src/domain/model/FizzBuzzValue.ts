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
