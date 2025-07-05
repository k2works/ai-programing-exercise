import { FizzBuzzValue } from './FizzBuzzValue'
import { FizzBuzzList } from './FizzBuzzList'
import { FizzBuzzType } from './FizzBuzzType'

export interface FizzBuzzCommand {
  execute(): FizzBuzzValue | FizzBuzzList
}

export class FizzBuzzValueCommand implements FizzBuzzCommand {
  private fizzBuzzType: FizzBuzzType
  private number: number

  constructor(fizzBuzzType: FizzBuzzType, number: number) {
    this.fizzBuzzType = fizzBuzzType
    this.number = number
  }

  execute(): FizzBuzzValue {
    return this.fizzBuzzType.generate(this.number)
  }
}

export class FizzBuzzListCommand implements FizzBuzzCommand {
  private fizzBuzzType: FizzBuzzType
  private start: number
  private end: number

  constructor(fizzBuzzType: FizzBuzzType, start: number = 1, end: number = 100) {
    this.fizzBuzzType = fizzBuzzType
    this.start = start
    this.end = end
  }

  execute(): FizzBuzzList {
    let list = new FizzBuzzList()
    for (let i = this.start; i <= this.end; i++) {
      const value = this.fizzBuzzType.generate(i)
      list = list.add(value)
    }
    return list
  }
}
