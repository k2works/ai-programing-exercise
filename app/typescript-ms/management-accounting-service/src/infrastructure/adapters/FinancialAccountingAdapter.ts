// src/infrastructure/adapters/FinancialAccountingAdapter.ts

import axios, { AxiosInstance } from 'axios'

/**
 * 財務会計サービスアダプター
 *
 * 腐敗防止層の一部として、財務会計サービスとの直接通信を抽象化する
 * イベント駆動アーキテクチャが主なコミュニケーション手段だが、
 * 必要に応じて同期的なHTTP APIアクセスも提供する
 *
 * DDD戦略的設計における Anti-Corruption Layer (ACL) パターンの実装
 */
export class FinancialAccountingAdapter {
  private readonly httpClient: AxiosInstance

  constructor(private readonly baseUrl: string) {
    this.httpClient = axios.create({
      baseURL: baseUrl,
      timeout: 5000,
      headers: {
        'Content-Type': 'application/json'
      }
    })
  }

  /**
   * 会計年度の仕訳データを取得
   *
   * イベントで受信したキャッシュデータが不完全な場合や、
   * 初期データ同期が必要な場合に使用
   *
   * @param fiscalYear 会計年度
   * @returns 仕訳データの配列
   */
  async fetchJournalsByFiscalYear(fiscalYear: number): Promise<JournalDto[]> {
    try {
      const response = await this.httpClient.get<{ journals: JournalDto[] }>(
        `/journals/fiscal-year/${fiscalYear}`
      )
      return response.data.journals
    } catch (error) {
      console.error(`Failed to fetch journals for fiscal year ${fiscalYear}:`, error)
      throw new FinancialAccountingAdapterError(
        `Failed to fetch journals: ${error instanceof Error ? error.message : 'Unknown error'}`
      )
    }
  }

  /**
   * 特定の仕訳を取得
   *
   * @param journalId 仕訳ID
   * @returns 仕訳データ
   */
  async fetchJournalById(journalId: number): Promise<JournalDto> {
    try {
      const response = await this.httpClient.get<JournalDto>(`/journals/${journalId}`)
      return response.data
    } catch (error) {
      console.error(`Failed to fetch journal ${journalId}:`, error)
      throw new FinancialAccountingAdapterError(
        `Failed to fetch journal: ${error instanceof Error ? error.message : 'Unknown error'}`
      )
    }
  }

  /**
   * 勘定科目マスタを取得
   *
   * @returns 勘定科目データの配列
   */
  async fetchAccounts(): Promise<AccountDto[]> {
    try {
      const response = await this.httpClient.get<{ accounts: AccountDto[] }>('/accounts')
      return response.data.accounts
    } catch (error) {
      console.error('Failed to fetch accounts:', error)
      throw new FinancialAccountingAdapterError(
        `Failed to fetch accounts: ${error instanceof Error ? error.message : 'Unknown error'}`
      )
    }
  }

  /**
   * ヘルスチェック
   *
   * @returns サービスが正常に動作しているか
   */
  async healthCheck(): Promise<boolean> {
    try {
      const response = await this.httpClient.get('/health')
      return response.status === 200
    } catch (error) {
      console.warn('Financial Accounting Service health check failed:', error)
      return false
    }
  }
}

/**
 * アダプター固有のエラークラス
 */
export class FinancialAccountingAdapterError extends Error {
  constructor(message: string) {
    super(message)
    this.name = 'FinancialAccountingAdapterError'
  }
}

/**
 * 財務会計サービスのDTO型定義
 * （外部システムの形式を表現）
 */

export interface JournalDto {
  id: number
  journalDate: string
  fiscalYear: number
  description: string
  userId?: string
  userName?: string
  detailItems: JournalDetailItemDto[]
}

export interface JournalDetailItemDto {
  id: number
  accountCode: string
  debitAmount: number
  creditAmount: number
  description?: string
}

export interface AccountDto {
  accountCode: string
  accountName: string
  accountType: string
  sumAccount: boolean
  bsplDistinction?: string
  displayOrder?: number
  aggregationTarget: boolean
}
