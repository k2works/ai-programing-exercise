// src/application/port/in/AccountUseCase.ts
import { Account } from '../../../domain/model/account/Account'

export interface CreateAccountCommand {
  accountCode: string
  accountName: string
  accountKana?: string
  accountType: string
  sumAccount?: boolean
  bsplDistinction?: string
  transactionDistinction?: string
  costDistinction?: string
  displayOrder?: number
  aggregationTarget?: boolean
}

export interface UpdateAccountCommand {
  accountCode: string
  accountName?: string
  accountKana?: string
  sumAccount?: boolean
  bsplDistinction?: string
  transactionDistinction?: string
  costDistinction?: string
  displayOrder?: number
  aggregationTarget?: boolean
}

export interface AccountQuery {
  accountCode?: string
  bsplDistinction?: string
  transactionDistinction?: string
}

/**
 * 勘定科目ユースケース（Input Port）
 * ビジネスユースケースのインターフェース定義
 */
export interface AccountUseCase {
  /**
   * 新しい勘定科目を作成する
   * @param command 勘定科目作成コマンド
   * @returns 作成された勘定科目
   * @throws Error 科目コードが重複している場合
   */
  createAccount(command: CreateAccountCommand): Promise<Account>

  /**
   * すべての勘定科目を取得する
   * @returns 勘定科目一覧（表示順）
   */
  getAllAccounts(): Promise<Account[]>

  /**
   * 科目コードで勘定科目を検索する
   * @param accountCode 科目コード
   * @returns 勘定科目
   * @throws Error 見つからない場合
   */
  getAccountByCode(accountCode: string): Promise<Account>

  /**
   * BSPL区分で勘定科目を取得する
   * @param bsplDistinction BSPL区分（'B' または 'P'）
   * @returns 勘定科目一覧
   */
  getAccountsByBspl(bsplDistinction: string): Promise<Account[]>

  /**
   * 勘定科目を更新する
   * @param command 更新コマンド
   * @returns 更新された勘定科目
   */
  updateAccount(command: UpdateAccountCommand): Promise<Account>

  /**
   * 勘定科目を削除する
   * @param accountCode 削除する科目コード
   */
  deleteAccount(accountCode: string): Promise<void>
}
