import { FizzBuzzType, FizzBuzzType01, FizzBuzzType02, FizzBuzzType03 } from './FizzBuzzType'

export class FizzBuzz {
  private list: string[] = []
  private type: number
  private fizzBuzzType: FizzBuzzType

  constructor(type: number = 1) {
    if (type < 1 || type > 3) {
      throw new Error('該当するタイプは存在しません')
    }
    this.type = type
    this.fizzBuzzType = this.createFizzBuzzType(type)
  }

  private createFizzBuzzType(type: number): FizzBuzzType {
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

  generate(number: number, type?: number): string {
    if (type !== undefined) {
      const tempFizzBuzzType = this.createFizzBuzzType(type)
      return tempFizzBuzzType.generate(number)
    }
    
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
    return this.type
  }
}
