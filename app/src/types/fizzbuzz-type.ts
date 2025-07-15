import { FizzBuzzValue } from '../value-objects/fizzbuzz-value';
import { InvalidTypeError } from '../exceptions/custom-errors';

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
