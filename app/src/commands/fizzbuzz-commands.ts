import { FizzBuzzValue } from '../value-objects/fizzbuzz-value';
import { FizzBuzzList } from '../collections/fizzbuzz-list';
import { FizzBuzzType } from '../types/fizzbuzz-type';

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
