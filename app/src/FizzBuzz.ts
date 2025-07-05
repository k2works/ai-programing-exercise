import { FizzBuzzType, FizzBuzzType01, FizzBuzzType02 } from './FizzBuzzType'

export class FizzBuzz {
  private list: string[] = []
  private fizzBuzzType: FizzBuzzType

  constructor(fizzBuzzType: FizzBuzzType) {
    this.fizzBuzzType = fizzBuzzType
  }

  static create(type: number): FizzBuzz {
    return new FizzBuzz(FizzBuzzType.create(type))
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
    return this.fizzBuzzType instanceof FizzBuzzType01 ? FizzBuzzType.TYPE_01 :
           this.fizzBuzzType instanceof FizzBuzzType02 ? FizzBuzzType.TYPE_02 : 
           FizzBuzzType.TYPE_03
  }
}
