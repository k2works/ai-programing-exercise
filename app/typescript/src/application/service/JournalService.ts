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
    // 1. 重複チェック
    const existing = await this.journalRepository.findByVoucherNo(command.voucherNo)
    if (existing) {
      throw new Error(`伝票番号 ${command.voucherNo} は既に使用されています`)
    }

    // 2. ビジネスルール検証
    this.validateJournal(command)

    // 3. 永続化用データの構築
    const journal: JournalWithDetails = {
      voucherNo: command.voucherNo,
      journalDate: command.journalDate,
      inputDate: command.inputDate,
      settlementFlag: command.settlementFlag ?? 0,
      singleFlag: command.singleFlag ?? 1,
      voucherType: command.voucherType,
      recurringFlag: command.recurringFlag ?? 0,
      employeeCode: command.employeeCode ?? null,
      departmentCode: command.departmentCode ?? null,
      redSlipFlag: command.redSlipFlag ?? 0,
      redBlackVoucherNo: command.redBlackVoucherNo ?? null,
      createdAt: new Date(),
      updatedAt: new Date(),
      details: command.details.map((detail) => ({
        voucherNo: command.voucherNo,
        lineNo: detail.lineNo,
        lineSummary: detail.lineSummary,
        createdAt: new Date(),
        updatedAt: new Date(),
        items: detail.items.map((item) => ({
          voucherNo: command.voucherNo,
          lineNo: detail.lineNo,
          debitCredit: item.debitCredit,
          currencyCode: item.currencyCode,
          exchangeRate: new Prisma.Decimal(item.exchangeRate),
          departmentCode: item.departmentCode ?? null,
          projectCode: item.projectCode ?? null,
          accountCode: item.accountCode,
          subAccountCode: item.subAccountCode ?? null,
          amount: new Prisma.Decimal(item.amount),
          baseAmount: new Prisma.Decimal(item.baseAmount),
          taxType: item.taxType ?? null,
          taxRate: item.taxRate ?? null,
          taxCalcType: item.taxCalcType ?? null,
          dueDate: item.dueDate ?? null,
          cashFlowFlag: item.cashFlowFlag ?? 0,
          segmentCode: item.segmentCode ?? null,
          offsetAccountCode: item.offsetAccountCode ?? null,
          offsetSubAccount: item.offsetSubAccount ?? null,
          noteCode: item.noteCode ?? null,
          noteContent: item.noteContent ?? null,
          createdAt: new Date(),
          updatedAt: new Date()
        }))
      }))
    }

    // 4. 永続化
    return await this.journalRepository.save(journal)
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
    // 1. 既存の仕訳を取得
    const existing = await this.journalRepository.findByVoucherNo(command.voucherNo)
    if (!existing) {
      throw new Error(`伝票番号 ${command.voucherNo} が見つかりません`)
    }

    // 2. 更新データの構築
    const updated: JournalWithDetails = {
      ...existing,
      journalDate: command.journalDate ?? existing.journalDate,
      inputDate: command.inputDate ?? existing.inputDate,
      settlementFlag: command.settlementFlag ?? existing.settlementFlag,
      singleFlag: command.singleFlag ?? existing.singleFlag,
      voucherType: command.voucherType ?? existing.voucherType,
      recurringFlag: command.recurringFlag ?? existing.recurringFlag,
      employeeCode: command.employeeCode ?? existing.employeeCode,
      departmentCode: command.departmentCode ?? existing.departmentCode,
      redSlipFlag: command.redSlipFlag ?? existing.redSlipFlag,
      redBlackVoucherNo: command.redBlackVoucherNo ?? existing.redBlackVoucherNo,
      updatedAt: new Date(),
      details:
        command.details?.map((detail) => ({
          voucherNo: command.voucherNo,
          lineNo: detail.lineNo,
          lineSummary: detail.lineSummary,
          createdAt: existing.details[0]?.createdAt ?? new Date(),
          updatedAt: new Date(),
          items: detail.items.map((item) => ({
            voucherNo: command.voucherNo,
            lineNo: detail.lineNo,
            debitCredit: item.debitCredit,
            currencyCode: item.currencyCode,
            exchangeRate: new Prisma.Decimal(item.exchangeRate),
            departmentCode: item.departmentCode ?? null,
            projectCode: item.projectCode ?? null,
            accountCode: item.accountCode,
            subAccountCode: item.subAccountCode ?? null,
            amount: new Prisma.Decimal(item.amount),
            baseAmount: new Prisma.Decimal(item.baseAmount),
            taxType: item.taxType ?? null,
            taxRate: item.taxRate ?? null,
            taxCalcType: item.taxCalcType ?? null,
            dueDate: item.dueDate ?? null,
            cashFlowFlag: item.cashFlowFlag ?? 0,
            segmentCode: item.segmentCode ?? null,
            offsetAccountCode: item.offsetAccountCode ?? null,
            offsetSubAccount: item.offsetSubAccount ?? null,
            noteCode: item.noteCode ?? null,
            noteContent: item.noteContent ?? null,
            createdAt: existing.details[0]?.items[0]?.createdAt ?? new Date(),
            updatedAt: new Date()
          }))
        })) ?? existing.details
    }

    // 3. ビジネスルール検証（details が更新される場合のみ）
    if (command.details) {
      this.validateJournal(updated)
    }

    // 4. 永続化
    return await this.journalRepository.save(updated)
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
   * - 借方・貸方の金額が一致すること
   * - 明細が存在すること
   */
  private validateJournal(journal: {
    details: {
      items: {
        debitCredit: string
        amount: number | { toNumber?: () => number }
      }[]
    }[]
  }): void {
    if (!journal.details || journal.details.length === 0) {
      throw new Error('仕訳明細が存在しません')
    }

    // 全明細の借方・貸方合計を計算
    let debitTotal = 0
    let creditTotal = 0

    for (const detail of journal.details) {
      if (!detail.items || detail.items.length === 0) {
        throw new Error('仕訳明細項目が存在しません')
      }

      for (const item of detail.items) {
        const amount =
          typeof item.amount === 'number' ? item.amount : item.amount.toNumber?.() ?? 0

        if (item.debitCredit === 'D') {
          debitTotal += amount
        } else if (item.debitCredit === 'C') {
          creditTotal += amount
        } else {
          throw new Error(`無効な借方・貸方区分です: ${item.debitCredit}`)
        }
      }
    }

    // 借方・貸方の一致確認（浮動小数点の誤差を考慮）
    if (Math.abs(debitTotal - creditTotal) > 0.01) {
      throw new Error(
        `借方合計（${debitTotal}）と貸方合計（${creditTotal}）が一致しません`
      )
    }
  }
}
