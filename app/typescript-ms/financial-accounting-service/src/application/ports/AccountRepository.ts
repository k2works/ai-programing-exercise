// src/application/ports/AccountRepository.ts

import { Account } from '../../domain/models/account/Account'
import { AccountCode } from '../../domain/models/account/AccountCode'

/**
 * 勘定科目リポジトリ（Output Port）
 * ドメイン層がデータアクセスに依存しないためのインターフェース
 */
export interface AccountRepository {
  /**
   * 勘定科目を保存する
   * @param account 保存する勘定科目
   * @returns 保存された勘定科目
   */
  save(account: Account): Promise<Account>

  /**
   * すべての勘定科目を取得する
   * @returns 勘定科目一覧（表示順でソート）
   */
  findAll(): Promise<Account[]>

  /**
   * 科目コードで勘定科目を検索する
   * @param accountCode 科目コード
   * @returns 勘定科目、見つからない場合はnull
   */
  findByCode(accountCode: AccountCode): Promise<Account | null>

  /**
   * BSPL区分で勘定科目を取得する
   * @param bsplDistinction BSPL区分（'B' または 'P'）
   * @returns 勘定科目一覧（表示順でソート）
   */
  findByBsplDistinction(bsplDistinction: string): Promise<Account[]>

  /**
   * 取引要素区分で勘定科目を取得する
   * @param transactionDistinction 取引要素区分
   * @returns 勘定科目一覧（表示順でソート）
   */
  findByTransactionDistinction(transactionDistinction: string): Promise<Account[]>

  /**
   * 勘定科目を削除する
   * @param accountCode 削除する勘定科目の科目コード
   */
  deleteByCode(accountCode: AccountCode): Promise<void>

  /**
   * すべての勘定科目を削除する（テスト用）
   */
  deleteAll(): Promise<void>
}
