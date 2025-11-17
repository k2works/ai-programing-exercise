// src/infrastructure/persistence/mapper/AccountMapper.ts
import { Account as PrismaAccount } from '@prisma/client'
import { Account } from '../../../domain/model/account/Account'
import { AccountCode } from '../../../domain/model/account/AccountCode'
import { AccountType } from '../../../domain/model/account/AccountType'

/**
 * 勘定科目マッパー
 * ドメインモデルと永続化モデルの変換を担当
 */
export class AccountMapper {
  /**
   * ドメインモデルから永続化モデルへ変換
   */
  toPersistence(account: Account) {
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

  /**
   * 永続化モデルからドメインモデルへ変換
   */
  toDomain(data: PrismaAccount): Account {
    return new Account(
      new AccountCode(data.accountCode),
      data.accountName,
      new AccountType(data.accountType),
      this.buildOptions(data)
    )
  }

  // eslint-disable-next-line complexity
  private buildOptions(data: PrismaAccount) {
    return {
      accountKana: data.accountKana ?? undefined,
      sumAccount: data.sumAccount ?? false,
      bsplDistinction: data.bsplDistinction ?? undefined,
      transactionDistinction: data.transactionDistinction ?? undefined,
      costDistinction: data.costDistinction ?? undefined,
      displayOrder: data.displayOrder ?? undefined,
      aggregationTarget: data.aggregationTarget ?? true
    }
  }
}
