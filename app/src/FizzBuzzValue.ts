export class FizzBuzzValue {
  private readonly _value: string

  constructor(value: string) {
    this._value = value
  }

  get value(): string {
    return this._value
  }

  toString(): string {
    return this._value
  }

  equals(other: FizzBuzzValue): boolean {
    return this._value === other._value
  }
}
