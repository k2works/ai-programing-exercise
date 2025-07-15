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
  private readonly _type: number;

  constructor(type: number = 1) {
    this._type = type;
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

  get type(): number {
    return this._type;
  }

  generate(n: number, type?: number): string {
    const isFizz = n % 3 === 0;
    const isBuzz = n % 5 === 0;
    const targetType = type ?? this._type;

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

  generateList(): void {
    // 新しい配列を作成してから割り当て
    const newList: string[] = [];
    for (let i = 1; i <= 100; i++) {
      newList.push(this.generate(i));
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
