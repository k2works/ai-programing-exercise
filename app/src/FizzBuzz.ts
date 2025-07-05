import { FizzBuzzType, FizzBuzzType01, FizzBuzzType02 } from './FizzBuzzType'
import { FizzBuzzValue } from './FizzBuzzValue'
import { FizzBuzzList } from './FizzBuzzList'

export class FizzBuzz {
  private list: FizzBuzzList = new FizzBuzzList()
  private fizzBuzzType: FizzBuzzType

  constructor(fizzBuzzType: FizzBuzzType) {
    this.fizzBuzzType = fizzBuzzType
  }

  static create(type: number): FizzBuzz {
    return new FizzBuzz(FizzBuzzType.create(type))
  }

  generate(number: number): FizzBuzzValue {
    return this.fizzBuzzType.generate(number)
  }

  generateList(start: number = 1, end: number = 100): FizzBuzzList {
    let newList = new FizzBuzzList()
    for (let i = start; i <= end; i++) {
      newList = newList.add(this.generate(i))
    }
    this.list = newList
    return this.list
  }

  getList(): FizzBuzzList {
    return this.list
  }

  getType(): number {
    return this.fizzBuzzType instanceof FizzBuzzType01 ? FizzBuzzType.TYPE_01 :
           this.fizzBuzzType instanceof FizzBuzzType02 ? FizzBuzzType.TYPE_02 : 
           FizzBuzzType.TYPE_03
  }
}
