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
  private _list: string[] = [];
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
    return [...this._list]; // 防御的コピーを返す
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
    // 新しい配列を作成してから割り当て
    const newList: string[] = [];
    for (let i = 1; i <= 100; i++) {
      newList.push(this._type.generate(i).value); // ポリモーフィズムを活用
    }
    this._list.length = 0; // 既存の配列をクリア
    this._list.push(...newList); // 新しい要素を追加
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
