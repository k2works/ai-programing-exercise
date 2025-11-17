// src/infrastructure/web/dto/AccountResponseDto.ts
import { Account } from '../../../domain/model/account/Account'

/**
 * 勘定科目レスポンスDTO
 */
export interface AccountResponseDto {
  accountCode: string
  accountName: string
  accountKana?: string
  accountType: string
  sumAccount: boolean
  bsplDistinction?: string
  transactionDistinction?: string
  costDistinction?: string
  displayOrder?: number
  aggregationTarget: boolean
}

/**
 * ドメインモデルから DTO へ変換
 */
export function toAccountResponseDto(account: Account): AccountResponseDto {
  return {
    accountCode: account.accountCode.value,
    accountName: account.accountName,
    accountKana: account.accountKana,
    accountType: account.accountType.value,
    sumAccount: account.sumAccount,
    bsplDistinction: account.bsplDistinction,
    transactionDistinction: account.transactionDistinction,
    costDistinction: account.costDistinction,
    displayOrder: account.displayOrder,
    aggregationTarget: account.aggregationTarget
  }
}
