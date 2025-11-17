// src/domain/model/account/Account.ts
import { AccountCode } from './AccountCode'
import { AccountType } from './AccountType'

/**
 * 勘定科目エンティティ
 * 財務会計における勘定科目の振る舞いを表現
 */
export class Account {
  private readonly _accountCode: AccountCode
  private _accountName: string
  private _accountKana?: string
  private readonly _accountType: AccountType
  private _sumAccount: boolean
  private _bsplDistinction?: string
  private _transactionDistinction?: string
  private _costDistinction?: string
  private _displayOrder?: number
  private _aggregationTarget: boolean

  // eslint-disable-next-line complexity
  constructor(
    accountCode: AccountCode,
    accountName: string,
    accountType: AccountType,
    options?: {
      accountKana?: string
      sumAccount?: boolean
      bsplDistinction?: string
      transactionDistinction?: string
      costDistinction?: string
      displayOrder?: number
      aggregationTarget?: boolean
    }
  ) {
    this._accountCode = accountCode
    this._accountName = accountName
    this._accountType = accountType
    this._accountKana = options?.accountKana
    this._sumAccount = options?.sumAccount ?? false
    this._bsplDistinction = options?.bsplDistinction
    this._transactionDistinction = options?.transactionDistinction
    this._costDistinction = options?.costDistinction
    this._displayOrder = options?.displayOrder
    this._aggregationTarget = options?.aggregationTarget ?? true

    // 生成時のバリデーション
    this.validate()
  }

  // Getters
  get accountCode(): AccountCode {
    return this._accountCode
  }
  get accountName(): string {
    return this._accountName
  }
  get accountKana(): string | undefined {
    return this._accountKana
  }
  get accountType(): AccountType {
    return this._accountType
  }
  get sumAccount(): boolean {
    return this._sumAccount
  }
  get bsplDistinction(): string | undefined {
    return this._bsplDistinction
  }
  get transactionDistinction(): string | undefined {
    return this._transactionDistinction
  }
  get costDistinction(): string | undefined {
    return this._costDistinction
  }
  get displayOrder(): number | undefined {
    return this._displayOrder
  }
  get aggregationTarget(): boolean {
    return this._aggregationTarget
  }

  /**
   * ビジネスルール: BSPL区分と科目区分の整合性チェック
   * - 資産・負債・純資産 → B（貸借対照表）
   * - 収益・費用 → P（損益計算書）
   */
  private validate(): void {
    this.validateBsplDistinction()
    this.validateTransactionDistinction()
  }

  // eslint-disable-next-line sonarjs/cognitive-complexity
  private validateBsplDistinction(): void {
    if (!this._bsplDistinction) {
      return
    }

    const isBalanceSheetType = ['資産', '負債', '純資産'].includes(this._accountType.value)
    const isProfitLossType = ['収益', '費用'].includes(this._accountType.value)

    if (isBalanceSheetType && this._bsplDistinction !== 'B') {
      throw new Error(`${this._accountType.value}科目のBSPL区分は "B" である必要があります`)
    }

    if (isProfitLossType && this._bsplDistinction !== 'P') {
      throw new Error(`${this._accountType.value}科目のBSPL区分は "P" である必要があります`)
    }
  }

  private validateTransactionDistinction(): void {
    if (!this._transactionDistinction) {
      return
    }

    const validDistinctions: Record<string, string[]> = {
      資産: ['1'],
      負債: ['2'],
      純資産: ['3'],
      収益: ['4'],
      費用: ['5']
    }

    const valid = validDistinctions[this._accountType.value]
    if (valid && !valid.includes(this._transactionDistinction)) {
      throw new Error(
        `${this._accountType.value}科目の取引要素区分は "${valid.join('または')}" である必要があります`
      )
    }
  }

  /**
   * ビジネスルール: 貸借対照表科目かどうか
   */
  isBalanceSheetAccount(): boolean {
    return ['資産', '負債', '純資産'].includes(this._accountType.value)
  }

  /**
   * ビジネスルール: 損益計算書科目かどうか
   */
  isProfitLossAccount(): boolean {
    return ['収益', '費用'].includes(this._accountType.value)
  }

  /**
   * ビジネスルール: 集計勘定かどうか
   */
  isSummaryAccount(): boolean {
    return this._sumAccount
  }

  /**
   * 勘定科目名を変更する
   */
  changeName(newName: string, newKana?: string): void {
    if (!newName || newName.length === 0 || newName.length > 40) {
      throw new Error('勘定科目名は1〜40文字である必要があります')
    }
    this._accountName = newName
    if (newKana !== undefined) {
      this._accountKana = newKana
    }
  }

  /**
   * 表示順を設定する
   */
  setDisplayOrder(order: number): void {
    if (order < 0) {
      throw new Error('表示順は0以上である必要があります')
    }
    this._displayOrder = order
  }

  // ファクトリメソッド
  static create(
    code: string,
    name: string,
    type: string,
    options?: {
      accountKana?: string
      sumAccount?: boolean
      bsplDistinction?: string
      transactionDistinction?: string
      costDistinction?: string
      displayOrder?: number
      aggregationTarget?: boolean
    }
  ): Account {
    return new Account(new AccountCode(code), name, new AccountType(type), options)
  }

  toString(): string {
    return `勘定科目[${this._accountCode.value}]: ${this._accountName} (${this._accountType.value})`
  }
}
