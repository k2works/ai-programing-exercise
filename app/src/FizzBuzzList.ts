import { FizzBuzzValue } from './FizzBuzzValue'

export class FizzBuzzList {
  private readonly _values: FizzBuzzValue[]

  constructor(values: FizzBuzzValue[] = []) {
    this._values = [...values] // immutable copy
  }

  get values(): FizzBuzzValue[] {
    return [...this._values] // immutable copy
  }

  add(value: FizzBuzzValue): FizzBuzzList {
    return new FizzBuzzList([...this._values, value])
  }

  get(index: number): FizzBuzzValue {
    return this._values[index]
  }

  get length(): number {
    return this._values.length
  }

  toString(): string {
    return this._values.map(v => v.toString()).join(', ')
  }

  // Array-like methods for compatibility
  first(): FizzBuzzValue {
    return this._values[0]
  }

  last(): FizzBuzzValue {
    return this._values[this._values.length - 1]
  }

  // Convert to string array for backward compatibility
  toStringArray(): string[] {
    return this._values.map(v => v.value)
  }
}
