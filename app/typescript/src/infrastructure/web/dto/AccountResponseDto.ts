// src/infrastructure/web/dto/AccountResponseDto.ts
import { Account } from '../../../domain/model/account/Account'

/**
 * 勘定科目レスポンスDTO
 */
export class AccountResponseDto {
  accountCode!: string
  accountName!: string
  accountKana?: string
  accountType!: string
  sumAccount!: boolean
  bsplDistinction?: string
  transactionDistinction?: string
  costDistinction?: string
  displayOrder?: number
  aggregationTarget!: boolean

  static fromDomain(account: Account): AccountResponseDto {
    const dto = new AccountResponseDto()
    dto.accountCode = account.accountCode.value
    dto.accountName = account.accountName
    dto.accountKana = account.accountKana
    dto.accountType = account.accountType.value
    dto.sumAccount = account.sumAccount
    dto.bsplDistinction = account.bsplDistinction
    dto.transactionDistinction = account.transactionDistinction
    dto.costDistinction = account.costDistinction
    dto.displayOrder = account.displayOrder
    dto.aggregationTarget = account.aggregationTarget
    return dto
  }
}
