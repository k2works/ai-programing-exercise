// src/domain/model/account/AccountType.ts

/**
 * 科目区分 値オブジェクト
 */
export class AccountType {
  private readonly _value: string
  private static readonly VALID_TYPES = ['資産', '負債', '純資産', '収益', '費用']

  constructor(value: string) {
    if (!AccountType.VALID_TYPES.includes(value)) {
      throw new Error(
        `科目区分は ${AccountType.VALID_TYPES.join(', ')} のいずれかである必要があります`
      )
    }
    this._value = value
  }

  get value(): string {
    return this._value
  }

  equals(other: AccountType): boolean {
    return this._value === other._value
  }

  toString(): string {
    return this._value
  }
}
