// src/domain/models/journal-cache.ts

/**
 * 財務会計サービスから受信した仕訳情報のキャッシュ
 *
 * 管理会計サービスは財務会計サービスのデータベースに直接アクセスせず、
 * イベント経由で必要な情報を受け取り、自サービスのデータベースにキャッシュする
 */
export interface JournalCache {
  id?: number
  journalId: string // 財務会計サービスの仕訳ID
  fiscalYear: number
  journalDate: Date
  totalDebitAmount: number
  totalCreditAmount: number
  receivedAt: Date
}

/**
 * JournalCache のリポジトリインターフェース
 */
export interface JournalCacheRepository {
  save(journalCache: JournalCache): Promise<JournalCache>
  findByFiscalYear(fiscalYear: number): Promise<JournalCache[]>
  findAll(): Promise<JournalCache[]>
}
