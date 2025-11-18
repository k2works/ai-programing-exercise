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
    details: journal.details.map((detail) => convertDetail(detail))
  }
}

/**
 * 仕訳明細を DTO に変換
 */
function convertDetail(detail: JournalWithDetails['details'][0]): JournalDetailDto {
  return {
    lineNo: detail.lineNo,
    lineSummary: detail.lineSummary,
    items: detail.items.map((item) => convertDetailItem(item))
  }
}

/**
 * 仕訳貸借明細を DTO に変換
 */
function convertDetailItem(
  item: JournalWithDetails['details'][0]['items'][0]
): JournalDetailItemDto {
  return {
    ...convertRequiredItemFields(item),
    ...convertOptionalItemFields(item)
  }
}

/**
 * 必須項目フィールドを変換
 */
function convertRequiredItemFields(item: JournalWithDetails['details'][0]['items'][0]) {
  return {
    debitCredit: item.debitCredit,
    currencyCode: item.currencyCode,
    exchangeRate: Number(item.exchangeRate),
    accountCode: item.accountCode,
    amount: Number(item.amount),
    baseAmount: Number(item.baseAmount),
    cashFlowFlag: item.cashFlowFlag
  }
}

/**
 * オプション項目フィールドを変換
 */
function convertOptionalItemFields(item: JournalWithDetails['details'][0]['items'][0]) {
  return {
    ...convertDepartmentProjectFields(item),
    ...convertTaxFields(item),
    ...convertOffsetFields(item),
    ...convertNoteFields(item)
  }
}

/**
 * 部門・プロジェクトフィールドを変換
 */
function convertDepartmentProjectFields(item: JournalWithDetails['details'][0]['items'][0]) {
  return {
    departmentCode: item.departmentCode ?? undefined,
    projectCode: item.projectCode ?? undefined,
    subAccountCode: item.subAccountCode ?? undefined
  }
}

/**
 * 税金関連フィールドを変換
 */
function convertTaxFields(item: JournalWithDetails['details'][0]['items'][0]) {
  return {
    taxType: item.taxType ?? undefined,
    taxRate: item.taxRate ?? undefined,
    taxCalcType: item.taxCalcType ?? undefined
  }
}

/**
 * 相殺関連フィールドを変換
 */
function convertOffsetFields(item: JournalWithDetails['details'][0]['items'][0]) {
  return {
    dueDate: item.dueDate?.toISOString().split('T')[0],
    segmentCode: item.segmentCode ?? undefined,
    offsetAccountCode: item.offsetAccountCode ?? undefined,
    offsetSubAccount: item.offsetSubAccount ?? undefined
  }
}

/**
 * 摘要関連フィールドを変換
 */
function convertNoteFields(item: JournalWithDetails['details'][0]['items'][0]) {
  return {
    noteCode: item.noteCode ?? undefined,
    noteContent: item.noteContent ?? undefined
  }
}
