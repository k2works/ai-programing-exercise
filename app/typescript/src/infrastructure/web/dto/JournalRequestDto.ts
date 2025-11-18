// src/infrastructure/web/dto/JournalRequestDto.ts

/**
 * 仕訳貸借明細作成リクエスト DTO
 */
export interface CreateJournalDetailItemDto {
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
  cashFlowFlag?: number
  segmentCode?: string
  offsetAccountCode?: string
  offsetSubAccount?: string
  noteCode?: string
  noteContent?: string
}

/**
 * 仕訳明細作成リクエスト DTO
 */
export interface CreateJournalDetailDto {
  lineNo: number
  lineSummary: string
  items: CreateJournalDetailItemDto[]
}

/**
 * 仕訳作成リクエスト DTO
 */
export interface CreateJournalRequestDto {
  voucherNo: string
  journalDate: string
  inputDate: string
  settlementFlag?: number
  singleFlag?: number
  voucherType: number
  recurringFlag?: number
  employeeCode?: string
  departmentCode?: string
  redSlipFlag?: number
  redBlackVoucherNo?: number
  details: CreateJournalDetailDto[]
}

/**
 * 仕訳更新リクエスト DTO
 */
export interface UpdateJournalRequestDto {
  journalDate?: string
  inputDate?: string
  settlementFlag?: number
  singleFlag?: number
  voucherType?: number
  recurringFlag?: number
  employeeCode?: string
  departmentCode?: string
  redSlipFlag?: number
  redBlackVoucherNo?: number
  details?: CreateJournalDetailDto[]
}
