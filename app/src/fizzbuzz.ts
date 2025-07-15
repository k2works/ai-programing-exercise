// タイプクラス
class FizzBuzzType01 {
  generate(n: number): string {
    const isFizz = n % 3 === 0;
    const isBuzz = n % 5 === 0;

    if (isFizz && isBuzz) {
      return 'FizzBuzz';
    } else if (isFizz) {
      return 'Fizz';
    } else if (isBuzz) {
      return 'Buzz';
    }
    return n.toString();
  }
}

class FizzBuzzType02 {
  generate(n: number): string {
    return n.toString();
  }
}

class FizzBuzzType03 {
  generate(n: number): string {
    const isFizz = n % 3 === 0;
    const isBuzz = n % 5 === 0;

    if (isFizz && isBuzz) {
      return 'FizzBuzz';
    }
    return n.toString();
  }
}

export class FizzBuzz {
  private _list: string[] = [];
  private readonly _type: FizzBuzzType01 | FizzBuzzType02 | FizzBuzzType03;

  constructor(type: number = 1) {
    this._type = FizzBuzz.create(type);
  }

  static create(type: number): FizzBuzzType01 | FizzBuzzType02 | FizzBuzzType03 {
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

  get type(): FizzBuzzType01 | FizzBuzzType02 | FizzBuzzType03 {
    return this._type;
  }

  generate(n: number, type?: number): string {
    if (type !== undefined) {
      // 後方互換性のために type パラメータが指定された場合の処理
      const isFizz = n % 3 === 0;
      const isBuzz = n % 5 === 0;
      const targetType = type;

      switch (targetType) {
        case 1:
          if (isFizz && isBuzz) {
            return 'FizzBuzz';
          } else if (isFizz) {
            return 'Fizz';
          } else if (isBuzz) {
            return 'Buzz';
          }
          return n.toString();
        case 2:
          return n.toString();
        case 3:
          if (isFizz && isBuzz) {
            return 'FizzBuzz';
          }
          return n.toString();
        default:
          throw new Error('タイプが未指定です');
      }
    }
    
    // ポリモーフィズムを活用した処理
    return this._type.generate(n);
  }

  generateList(): void {
    // 新しい配列を作成してから割り当て
    const newList: string[] = [];
    for (let i = 1; i <= 100; i++) {
      newList.push(this._type.generate(i)); // ポリモーフィズムを活用
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
