// src/application/port/in/JournalUseCase.ts
import { Journal, JournalDetail, JournalDetailItem } from '@prisma/client'

/**
 * 仕訳貸借明細作成コマンド
 */
export interface CreateJournalDetailItemCommand {
  debitCredit: string
  currencyCode: string
  exchangeRate: number
  departmentCode?: string
  projectCode?: string
  accountCode: string
  subAccountCode?: string
  amount: number
  baseAmount: number
  taxType?: string
  taxRate?: number
  taxCalcType?: string
  dueDate?: Date
  cashFlowFlag?: number
  segmentCode?: string
  offsetAccountCode?: string
  offsetSubAccount?: string
  noteCode?: string
  noteContent?: string
}

/**
 * 仕訳明細作成コマンド
 */
export interface CreateJournalDetailCommand {
  lineNo: number
  lineSummary: string
  items: CreateJournalDetailItemCommand[]
}

/**
 * 仕訳作成コマンド
 */
export interface CreateJournalCommand {
  voucherNo: string
  journalDate: Date
  inputDate: Date
  settlementFlag?: number
  singleFlag?: number
  voucherType: number
  recurringFlag?: number
  employeeCode?: string
  departmentCode?: string
  redSlipFlag?: number
  redBlackVoucherNo?: number
  details: CreateJournalDetailCommand[]
}

/**
 * 仕訳更新コマンド
 */
export interface UpdateJournalCommand {
  voucherNo: string
  journalDate?: Date
  inputDate?: Date
  settlementFlag?: number
  singleFlag?: number
  voucherType?: number
  recurringFlag?: number
  employeeCode?: string
  departmentCode?: string
  redSlipFlag?: number
  redBlackVoucherNo?: number
  details?: CreateJournalDetailCommand[]
}

/**
 * 仕訳（明細・貸借明細を含む完全な型）
 */
export type JournalWithDetails = Journal & {
  details: (JournalDetail & {
    items: JournalDetailItem[]
  })[]
}

/**
 * 仕訳ユースケース（Input Port）
 * ビジネスユースケースのインターフェース定義
 */
export interface JournalUseCase {
  /**
   * 新しい仕訳を作成する
   * @param command 仕訳作成コマンド
   * @returns 作成された仕訳（明細含む）
   * @throws Error 伝票番号が重複している場合
   */
  createJournal(command: CreateJournalCommand): Promise<JournalWithDetails>

  /**
   * すべての仕訳を取得する
   * @returns 仕訳一覧（起票日順）
   */
  getAllJournals(): Promise<JournalWithDetails[]>

  /**
   * 伝票番号で仕訳を検索する
   * @param voucherNo 伝票番号
   * @returns 仕訳（明細含む）
   * @throws Error 見つからない場合
   */
  getJournalByVoucherNo(voucherNo: string): Promise<JournalWithDetails>

  /**
   * 日付範囲で仕訳を取得する
   * @param startDate 開始日
   * @param endDate 終了日
   * @returns 仕訳一覧
   */
  getJournalsByDateRange(startDate: Date, endDate: Date): Promise<JournalWithDetails[]>

  /**
   * 仕訳を更新する
   * @param command 更新コマンド
   * @returns 更新された仕訳
   */
  updateJournal(command: UpdateJournalCommand): Promise<JournalWithDetails>

  /**
   * 仕訳を削除する
   * @param voucherNo 削除する伝票番号
   */
  deleteJournal(voucherNo: string): Promise<void>
}
