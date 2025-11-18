// src/application/service/JournalService.ts
import {
  JournalUseCase,
  CreateJournalCommand,
  UpdateJournalCommand,
  JournalWithDetails
} from '../port/in/JournalUseCase'
import { JournalRepository } from '../port/out/JournalRepository'
import { Prisma } from '@prisma/client'

/**
 * 仕訳サービス（Application Service）
 * Input Port の実装としてビジネスロジックを提供
 */
export class JournalService implements JournalUseCase {
  constructor(private journalRepository: JournalRepository) {}

  async createJournal(command: CreateJournalCommand): Promise<JournalWithDetails> {
    await this.validateNoDuplicate(command.voucherNo)
    this.validateJournal(command)
    const journal = this.buildJournalFromCommand(command)
    return await this.journalRepository.save(journal)
  }

  /**
   * 重複チェック
   */
  private async validateNoDuplicate(voucherNo: string): Promise<void> {
    const existing = await this.journalRepository.findByVoucherNo(voucherNo)
    if (existing) {
      throw new Error(`伝票番号 ${voucherNo} は既に使用されています`)
    }
  }

  /**
   * コマンドから仕訳データを構築
   */
  private buildJournalFromCommand(command: CreateJournalCommand): JournalWithDetails {
    return {
      ...this.buildJournalHeader(command),
      details: command.details.map((detail) => this.buildJournalDetail(command.voucherNo, detail))
    }
  }

  /**
   * 仕訳ヘッダーを構築
   */
  private buildJournalHeader(command: CreateJournalCommand) {
    return {
      voucherNo: command.voucherNo,
      journalDate: command.journalDate,
      inputDate: command.inputDate,
      voucherType: command.voucherType,
      ...this.buildOptionalHeaderFields(command),
      createdAt: new Date(),
      updatedAt: new Date()
    }
  }

  /**
   * オプションのヘッダーフィールドを構築
   */
  private buildOptionalHeaderFields(command: CreateJournalCommand) {
    return {
      ...this.buildHeaderFlags(command),
      ...this.buildHeaderCodes(command),
      redBlackVoucherNo: command.redBlackVoucherNo ?? null
    }
  }

  /**
   * ヘッダーフラグを構築
   */
  private buildHeaderFlags(command: CreateJournalCommand) {
    return {
      settlementFlag: command.settlementFlag ?? 0,
      singleFlag: command.singleFlag ?? 1,
      recurringFlag: command.recurringFlag ?? 0,
      redSlipFlag: command.redSlipFlag ?? 0
    }
  }

  /**
   * ヘッダーコードを構築
   */
  private buildHeaderCodes(command: CreateJournalCommand) {
    return {
      employeeCode: command.employeeCode ?? null,
      departmentCode: command.departmentCode ?? null
    }
  }

  /**
   * 仕訳明細を構築
   */
  private buildJournalDetail(voucherNo: string, detail: CreateJournalCommand['details'][0]) {
    return {
      voucherNo,
      lineNo: detail.lineNo,
      lineSummary: detail.lineSummary,
      createdAt: new Date(),
      updatedAt: new Date(),
      items: detail.items.map((item) => this.buildJournalDetailItem(voucherNo, detail.lineNo, item))
    }
  }

  /**
   * 仕訳貸借明細を構築
   */
  private buildJournalDetailItem(
    voucherNo: string,
    lineNo: number,
    item: CreateJournalCommand['details'][0]['items'][0]
  ) {
    return {
      voucherNo,
      lineNo,
      debitCredit: item.debitCredit,
      currencyCode: item.currencyCode,
      accountCode: item.accountCode,
      ...this.buildRequiredAmounts(item),
      ...this.buildOptionalDetailItemFields(item),
      createdAt: new Date(),
      updatedAt: new Date()
    }
  }

  /**
   * 必須金額フィールドを構築
   */
  private buildRequiredAmounts(item: CreateJournalCommand['details'][0]['items'][0]) {
    return {
      exchangeRate: new Prisma.Decimal(item.exchangeRate),
      amount: new Prisma.Decimal(item.amount),
      baseAmount: new Prisma.Decimal(item.baseAmount)
    }
  }

  /**
   * オプションの明細項目フィールドを構築
   */
  private buildOptionalDetailItemFields(item: CreateJournalCommand['details'][0]['items'][0]) {
    return {
      ...this.buildDepartmentProjectFields(item),
      ...this.buildTaxFields(item),
      ...this.buildOffsetFields(item),
      ...this.buildNoteFields(item)
    }
  }

  /**
   * 部門・プロジェクトフィールドを構築
   */
  private buildDepartmentProjectFields(item: CreateJournalCommand['details'][0]['items'][0]) {
    return {
      departmentCode: item.departmentCode ?? null,
      projectCode: item.projectCode ?? null,
      subAccountCode: item.subAccountCode ?? null
    }
  }

  /**
   * 税金関連フィールドを構築
   */
  private buildTaxFields(item: CreateJournalCommand['details'][0]['items'][0]) {
    return {
      taxType: item.taxType ?? null,
      taxRate: item.taxRate ?? null,
      taxCalcType: item.taxCalcType ?? null
    }
  }

  /**
   * 相殺関連フィールドを構築
   */
  private buildOffsetFields(item: CreateJournalCommand['details'][0]['items'][0]) {
    return {
      dueDate: item.dueDate ?? null,
      cashFlowFlag: item.cashFlowFlag ?? 0,
      segmentCode: item.segmentCode ?? null,
      offsetAccountCode: item.offsetAccountCode ?? null,
      offsetSubAccount: item.offsetSubAccount ?? null
    }
  }

  /**
   * 摘要関連フィールドを構築
   */
  private buildNoteFields(item: CreateJournalCommand['details'][0]['items'][0]) {
    return {
      noteCode: item.noteCode ?? null,
      noteContent: item.noteContent ?? null
    }
  }

  async getAllJournals(): Promise<JournalWithDetails[]> {
    return await this.journalRepository.findAll()
  }

  async getJournalByVoucherNo(voucherNo: string): Promise<JournalWithDetails> {
    const journal = await this.journalRepository.findByVoucherNo(voucherNo)
    if (!journal) {
      throw new Error(`伝票番号 ${voucherNo} が見つかりません`)
    }
    return journal
  }

  async getJournalsByDateRange(startDate: Date, endDate: Date): Promise<JournalWithDetails[]> {
    if (startDate > endDate) {
      throw new Error('開始日は終了日より前である必要があります')
    }
    return await this.journalRepository.findByDateRange(startDate, endDate)
  }

  async updateJournal(command: UpdateJournalCommand): Promise<JournalWithDetails> {
    const existing = await this.getExistingJournal(command.voucherNo)
    const updated = this.buildUpdatedJournal(command, existing)

    if (command.details) {
      this.validateJournal(updated)
    }

    return await this.journalRepository.save(updated)
  }

  /**
   * 既存の仕訳を取得
   */
  private async getExistingJournal(voucherNo: string): Promise<JournalWithDetails> {
    const existing = await this.journalRepository.findByVoucherNo(voucherNo)
    if (!existing) {
      throw new Error(`伝票番号 ${voucherNo} が見つかりません`)
    }
    return existing
  }

  /**
   * 更新された仕訳を構築
   */
  private buildUpdatedJournal(
    command: UpdateJournalCommand,
    existing: JournalWithDetails
  ): JournalWithDetails {
    return {
      ...existing,
      ...this.buildUpdatedHeader(command, existing),
      details: this.buildUpdatedDetails(command, existing)
    }
  }

  /**
   * 更新されたヘッダーを構築
   */
  private buildUpdatedHeader(command: UpdateJournalCommand, existing: JournalWithDetails) {
    return {
      ...this.mergeRequiredHeaderFields(command, existing),
      ...this.mergeOptionalHeaderFields(command, existing),
      updatedAt: new Date()
    }
  }

  /**
   * 必須ヘッダーフィールドをマージ
   */
  private mergeRequiredHeaderFields(command: UpdateJournalCommand, existing: JournalWithDetails) {
    return {
      journalDate: command.journalDate ?? existing.journalDate,
      inputDate: command.inputDate ?? existing.inputDate,
      voucherType: command.voucherType ?? existing.voucherType
    }
  }

  /**
   * オプションヘッダーフィールドをマージ
   */
  private mergeOptionalHeaderFields(command: UpdateJournalCommand, existing: JournalWithDetails) {
    return {
      ...this.mergeHeaderFlags(command, existing),
      ...this.mergeHeaderCodes(command, existing),
      redBlackVoucherNo: command.redBlackVoucherNo ?? existing.redBlackVoucherNo
    }
  }

  /**
   * ヘッダーフラグをマージ
   */
  private mergeHeaderFlags(command: UpdateJournalCommand, existing: JournalWithDetails) {
    return {
      settlementFlag: command.settlementFlag ?? existing.settlementFlag,
      singleFlag: command.singleFlag ?? existing.singleFlag,
      recurringFlag: command.recurringFlag ?? existing.recurringFlag,
      redSlipFlag: command.redSlipFlag ?? existing.redSlipFlag
    }
  }

  /**
   * ヘッダーコードをマージ
   */
  private mergeHeaderCodes(command: UpdateJournalCommand, existing: JournalWithDetails) {
    return {
      employeeCode: command.employeeCode ?? existing.employeeCode,
      departmentCode: command.departmentCode ?? existing.departmentCode
    }
  }

  /**
   * 更新された明細を構築
   */
  private buildUpdatedDetails(command: UpdateJournalCommand, existing: JournalWithDetails) {
    if (!command.details) {
      return existing.details
    }

    return command.details.map((detail) =>
      this.buildUpdatedDetail(command.voucherNo, detail, existing)
    )
  }

  /**
   * 更新された明細行を構築
   */
  private buildUpdatedDetail(
    voucherNo: string,
    detail: UpdateJournalCommand['details'][0],
    existing: JournalWithDetails
  ) {
    return {
      voucherNo,
      lineNo: detail.lineNo,
      lineSummary: detail.lineSummary,
      createdAt: existing.details[0]?.createdAt ?? new Date(),
      updatedAt: new Date(),
      items: detail.items.map((item) =>
        this.buildUpdatedDetailItem(voucherNo, detail.lineNo, item, existing)
      )
    }
  }

  /**
   * 更新された明細項目を構築
   */
  private buildUpdatedDetailItem(
    voucherNo: string,
    lineNo: number,
    item: UpdateJournalCommand['details'][0]['items'][0],
    existing: JournalWithDetails
  ) {
    return {
      ...this.buildJournalDetailItem(voucherNo, lineNo, item),
      createdAt: existing.details[0]?.items[0]?.createdAt ?? new Date()
    }
  }

  async deleteJournal(voucherNo: string): Promise<void> {
    const existing = await this.journalRepository.findByVoucherNo(voucherNo)
    if (!existing) {
      throw new Error(`伝票番号 ${voucherNo} が見つかりません`)
    }

    await this.journalRepository.deleteByVoucherNo(voucherNo)
  }

  /**
   * 仕訳のビジネスルール検証
   */
  private validateJournal(journal: {
    details: {
      items: {
        debitCredit: string
        amount: number | { toNumber?: () => number }
      }[]
    }[]
  }): void {
    this.validateDetailsExist(journal.details)
    const { debitTotal, creditTotal } = this.calculateDebitCreditTotals(journal.details)
    this.validateBalanceMatch(debitTotal, creditTotal)
  }

  /**
   * 明細の存在を検証
   */
  private validateDetailsExist(
    details: { items: { debitCredit: string; amount: number | { toNumber?: () => number } }[] }[]
  ): void {
    if (!details || details.length === 0) {
      throw new Error('仕訳明細が存在しません')
    }
  }

  /**
   * 借方・貸方の合計を計算
   */
  private calculateDebitCreditTotals(
    details: {
      items: { debitCredit: string; amount: number | { toNumber?: () => number } }[]
    }[]
  ): { debitTotal: number; creditTotal: number } {
    let debitTotal = 0
    let creditTotal = 0

    for (const detail of details) {
      this.validateItemsExist(detail.items)
      const totals = this.processDetailItems(detail.items)
      debitTotal += totals.debit
      creditTotal += totals.credit
    }

    return { debitTotal, creditTotal }
  }

  /**
   * 明細項目の存在を検証
   */
  private validateItemsExist(
    items: { debitCredit: string; amount: number | { toNumber?: () => number } }[]
  ): void {
    if (!items || items.length === 0) {
      throw new Error('仕訳明細項目が存在しません')
    }
  }

  /**
   * 明細項目を処理して借方・貸方を集計
   */
  private processDetailItems(
    items: { debitCredit: string; amount: number | { toNumber?: () => number } }[]
  ): { debit: number; credit: number } {
    let debit = 0
    let credit = 0

    for (const item of items) {
      const amount = this.getAmountValue(item.amount)
      const result = this.categorizeAmount(item.debitCredit, amount)
      debit += result.debit
      credit += result.credit
    }

    return { debit, credit }
  }

  /**
   * 金額の値を取得
   */
  private getAmountValue(amount: number | { toNumber?: () => number }): number {
    return typeof amount === 'number' ? amount : (amount.toNumber?.() ?? 0)
  }

  /**
   * 借方・貸方区分に応じて金額を分類
   */
  private categorizeAmount(debitCredit: string, amount: number): { debit: number; credit: number } {
    if (debitCredit === 'D') {
      return { debit: amount, credit: 0 }
    }
    if (debitCredit === 'C') {
      return { debit: 0, credit: amount }
    }
    throw new Error(`無効な借方・貸方区分です: ${debitCredit}`)
  }

  /**
   * 借方・貸方の一致を検証
   */
  private validateBalanceMatch(debitTotal: number, creditTotal: number): void {
    if (Math.abs(debitTotal - creditTotal) > 0.01) {
      throw new Error(`借方合計（${debitTotal}）と貸方合計（${creditTotal}）が一致しません`)
    }
  }
}
