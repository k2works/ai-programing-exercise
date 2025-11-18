// src/infrastructure/persistence/PrismaAccountRepository.ts

import { AccountRepository } from '../../application/ports/AccountRepository'
import { Account } from '../../domain/models/account/Account'
import { AccountCode } from '../../domain/models/account/AccountCode'
import { AccountType } from '../../domain/models/account/AccountType'
import { getPrismaClient } from './prisma-client'

/**
 * Prisma ベースの勘定科目リポジトリ実装（Adapter）
 */
export class PrismaAccountRepository implements AccountRepository {
  private prisma = getPrismaClient()

  async save(account: Account): Promise<Account> {
    const data = {
      accountCode: account.accountCode.value,
      accountName: account.accountName,
      accountType: account.accountType.value,
      sumAccount: account.sumAccount,
      bsplDistinction: account.bsplDistinction,
      displayOrder: account.displayOrder,
      aggregationTarget: account.aggregationTarget
    }

    const saved = await this.prisma.account.upsert({
      where: { accountCode: account.accountCode.value },
      update: data,
      create: data
    })

    return this.toDomain(saved)
  }

  async findAll(): Promise<Account[]> {
    const accounts = await this.prisma.account.findMany({
      orderBy: { displayOrder: 'asc' }
    })

    return accounts.map((account) => this.toDomain(account))
  }

  async findByCode(accountCode: AccountCode): Promise<Account | null> {
    const account = await this.prisma.account.findUnique({
      where: { accountCode: accountCode.value }
    })

    return account ? this.toDomain(account) : null
  }

  async findByBsplDistinction(bsplDistinction: string): Promise<Account[]> {
    const accounts = await this.prisma.account.findMany({
      where: { bsplDistinction: bsplDistinction },
      orderBy: { displayOrder: 'asc' }
    })

    return accounts.map((account) => this.toDomain(account))
  }

  async findByTransactionDistinction(_transactionDistinction: string): Promise<Account[]> {
    // transactionDistinction フィールドがスキーマに存在しないため、空配列を返す
    return []
  }

  async deleteByCode(accountCode: AccountCode): Promise<void> {
    await this.prisma.account.delete({
      where: { accountCode: accountCode.value }
    })
  }

  async deleteAll(): Promise<void> {
    await this.prisma.account.deleteMany({})
  }

  /**
   * Prisma データモデルをドメインモデルに変換
   */
  private toDomain(data: {
    accountCode: string
    accountName: string
    accountType: string
    sumAccount: boolean
    bsplDistinction: string | null
    displayOrder: number | null
    aggregationTarget: boolean
  }): Account {
    return new Account(
      new AccountCode(data.accountCode),
      data.accountName,
      new AccountType(data.accountType),
      {
        sumAccount: data.sumAccount,
        bsplDistinction: data.bsplDistinction ?? undefined,
        displayOrder: data.displayOrder ?? undefined,
        aggregationTarget: data.aggregationTarget
      }
    )
  }
}
