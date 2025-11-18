// src/infrastructure/web/dto/JournalResponseDto.ts
import { JournalWithDetails } from '../../../application/port/in/JournalUseCase'

/**
 * 仕訳貸借明細 DTO
 */
export interface JournalDetailItemDto {
  debitCredit: string // D=借方, C=貸方
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
  dueDate?: string
  cashFlowFlag: number
  segmentCode?: string
  offsetAccountCode?: string
  offsetSubAccount?: string
  noteCode?: string
  noteContent?: string
}

/**
 * 仕訳明細 DTO
 */
export interface JournalDetailDto {
  lineNo: number
  lineSummary: string
  items: JournalDetailItemDto[]
}

/**
 * 仕訳レスポンス DTO
 */
export interface JournalResponseDto {
  voucherNo: string
  journalDate: string
  inputDate: string
  settlementFlag: number
  singleFlag: number
  voucherType: number
  recurringFlag: number
  employeeCode?: string
  departmentCode?: string
  redSlipFlag: number
  redBlackVoucherNo?: number
  details: JournalDetailDto[]
}

/**
 * ドメインモデルから DTO へ変換
 */
export function toJournalResponseDto(journal: JournalWithDetails): JournalResponseDto {
  return {
    voucherNo: journal.voucherNo,
    journalDate: journal.journalDate.toISOString().split('T')[0],
    inputDate: journal.inputDate.toISOString().split('T')[0],
    settlementFlag: journal.settlementFlag,
    singleFlag: journal.singleFlag,
    voucherType: journal.voucherType,
    recurringFlag: journal.recurringFlag,
    employeeCode: journal.employeeCode ?? undefined,
    departmentCode: journal.departmentCode ?? undefined,
    redSlipFlag: journal.redSlipFlag,
    redBlackVoucherNo: journal.redBlackVoucherNo ?? undefined,
    details: journal.details.map((detail) => ({
      lineNo: detail.lineNo,
      lineSummary: detail.lineSummary,
      items: detail.items.map((item) => ({
        debitCredit: item.debitCredit,
        currencyCode: item.currencyCode,
        exchangeRate: Number(item.exchangeRate),
        departmentCode: item.departmentCode ?? undefined,
        projectCode: item.projectCode ?? undefined,
        accountCode: item.accountCode,
        subAccountCode: item.subAccountCode ?? undefined,
        amount: Number(item.amount),
        baseAmount: Number(item.baseAmount),
        taxType: item.taxType ?? undefined,
        taxRate: item.taxRate ?? undefined,
        taxCalcType: item.taxCalcType ?? undefined,
        dueDate: item.dueDate?.toISOString().split('T')[0],
        cashFlowFlag: item.cashFlowFlag,
        segmentCode: item.segmentCode ?? undefined,
        offsetAccountCode: item.offsetAccountCode ?? undefined,
        offsetSubAccount: item.offsetSubAccount ?? undefined,
        noteCode: item.noteCode ?? undefined,
        noteContent: item.noteContent ?? undefined
      }))
    }))
  }
}
