// src/application/services/FinancialAccountingService.ts

import { FinancialAccountingAdapter } from '../../infrastructure/adapters/FinancialAccountingAdapter'
import { FinancialAccountingEventTranslator } from '../translators/FinancialAccountingEventTranslator'
import { JournalCache } from '../../domain/models/journal-cache'

/**
 * 財務会計サービス統合ファサード
 *
 * 腐敗防止層（ACL）の外部インターフェースとして、
 * 財務会計サービスとの統合を一元管理する
 *
 * - イベント駆動通信（非同期）
 * - HTTP API通信（同期）
 *
 * の両方をサポートし、管理会計サービスのビジネスロジックを
 * 財務会計サービスの実装詳細から隔離する
 */
export class FinancialAccountingService {
  private readonly adapter: FinancialAccountingAdapter
  private readonly translator: FinancialAccountingEventTranslator

  constructor(financialAccountingBaseUrl: string) {
    this.adapter = new FinancialAccountingAdapter(financialAccountingBaseUrl)
    this.translator = new FinancialAccountingEventTranslator()
  }

  /**
   * 会計年度の仕訳データを同期的に取得
   *
   * 初期データロードやキャッシュ再構築時に使用
   *
   * @param fiscalYear 会計年度
   * @returns 仕訳キャッシュの配列
   */
  async fetchJournalCachesByFiscalYear(fiscalYear: number): Promise<JournalCache[]> {
    try {
      console.log(`📥 Fetching journals for fiscal year ${fiscalYear} from Financial Accounting Service`)

      const journals = await this.adapter.fetchJournalsByFiscalYear(fiscalYear)

      // 外部DTOを内部ドメインモデルに変換
      const journalCaches: JournalCache[] = journals.map((journal) => {
        const totalDebitAmount = journal.detailItems.reduce(
          (sum, item) => sum + item.debitAmount,
          0
        )
        const totalCreditAmount = journal.detailItems.reduce(
          (sum, item) => sum + item.creditAmount,
          0
        )

        return {
          journalId: journal.id.toString(),
          fiscalYear: journal.fiscalYear,
          journalDate: new Date(journal.journalDate),
          totalDebitAmount,
          totalCreditAmount,
          receivedAt: new Date()
        }
      })

      console.log(`✅ Fetched ${journalCaches.length} journals for fiscal year ${fiscalYear}`)

      return journalCaches
    } catch (error) {
      console.error(`❌ Failed to fetch journals for fiscal year ${fiscalYear}:`, error)
      throw error
    }
  }

  /**
   * 財務会計サービスのヘルスチェック
   *
   * @returns サービスが利用可能か
   */
  async isAvailable(): Promise<boolean> {
    return await this.adapter.healthCheck()
  }

  /**
   * トランスレーターのアクセサ
   *
   * イベントハンドラーで直接使用する場合に備えて公開
   */
  getTranslator(): FinancialAccountingEventTranslator {
    return this.translator
  }
}
