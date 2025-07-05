import { FizzBuzzType, FizzBuzzType01, FizzBuzzType02, FizzBuzzType03 } from './FizzBuzzType'

export class FizzBuzz {
  private list: string[] = []
  private fizzBuzzType: FizzBuzzType

  constructor(type: number = 1) {
    this.fizzBuzzType = FizzBuzz.createFizzBuzzType(type)
  }

  static createFizzBuzzType(type: number): FizzBuzzType {
    switch (type) {
      case 1:
        return new FizzBuzzType01()
      case 2:
        return new FizzBuzzType02()
      case 3:
        return new FizzBuzzType03()
      default:
        throw new Error('該当するタイプは存在しません')
    }
  }

  static create(type: number): FizzBuzz {
    return new FizzBuzz(type)
  }

  generate(number: number): string {
    return this.fizzBuzzType.generate(number)
  }

  generateList(start: number = 1, end: number = 100): string[] {
    this.list = []
    for (let i = start; i <= end; i++) {
      this.list.push(this.generate(i))
    }
    return this.list
  }

  getList(): string[] {
    return this.list
  }

  getType(): number {
    return this.fizzBuzzType instanceof FizzBuzzType01 ? 1 :
           this.fizzBuzzType instanceof FizzBuzzType02 ? 2 : 3
  }
}
