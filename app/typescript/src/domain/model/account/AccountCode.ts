// src/domain/model/account/AccountCode.ts

/**
 * 科目コード 値オブジェクト
 */
export class AccountCode {
  private readonly _value: string

  constructor(value: string) {
    if (!value || value.length === 0 || value.length > 10) {
      throw new Error('科目コードは1〜10文字である必要があります')
    }
    this._value = value
  }

  get value(): string {
    return this._value
  }

  equals(other: AccountCode): boolean {
    return this._value === other._value
  }

  toString(): string {
    return this._value
  }
}
