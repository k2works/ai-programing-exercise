// src/application/ports/JournalRepository.ts

/**
 * 仕訳データ型（簡易版）
 */
export interface Journal {
  id?: number
  journalDate: Date
  fiscalYear: number
  description: string
  userId?: string
  userName?: string
  detailItems: JournalDetailItem[]
}

export interface JournalDetailItem {
  id?: number
  journalId?: number
  accountCode: string
  debitAmount: number
  creditAmount: number
  description?: string
}

/**
 * 仕訳リポジトリ（Output Port）
 */
export interface JournalRepository {
  /**
   * 仕訳を保存する
   * @param journal 保存する仕訳
   * @returns 保存された仕訳
   */
  save(journal: Journal): Promise<Journal>

  /**
   * すべての仕訳を取得する
   * @returns 仕訳一覧（日付降順）
   */
  findAll(): Promise<Journal[]>

  /**
   * IDで仕訳を検索する
   * @param id 仕訳ID
   * @returns 仕訳、見つからない場合はnull
   */
  findById(id: number): Promise<Journal | null>

  /**
   * 会計期間で仕訳を取得する
   * @param fiscalYear 会計年度
   * @returns 仕訳一覧
   */
  findByFiscalYear(fiscalYear: number): Promise<Journal[]>

  /**
   * 仕訳を削除する
   * @param id 削除する仕訳のID
   */
  deleteById(id: number): Promise<void>

  /**
   * すべての仕訳を削除する（テスト用）
   */
  deleteAll(): Promise<void>
}
