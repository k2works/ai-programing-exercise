// src/infrastructure/persistence/AccountPersistenceAdapter.ts
import { PrismaClient } from '@prisma/client'
import { AccountRepository } from '../../application/port/out/AccountRepository'
import { Account } from '../../domain/model/account/Account'
import { AccountCode } from '../../domain/model/account/AccountCode'
import { AccountMapper } from './mapper/AccountMapper'

/**
 * 勘定科目永続化アダプター（Output Adapter）
 * Prisma を使用してデータベースアクセスを実装
 */
export class AccountPersistenceAdapter implements AccountRepository {
  constructor(
    private prisma: PrismaClient,
    private mapper: AccountMapper
  ) {}

  async save(account: Account): Promise<Account> {
    const data = this.mapper.toPersistence(account)

    const saved = await this.prisma.account.upsert({
      where: { accountCode: account.accountCode.value },
      update: data,
      create: data
    })

    return this.mapper.toDomain(saved)
  }

  async findAll(): Promise<Account[]> {
    const accounts = await this.prisma.account.findMany({
      orderBy: { displayOrder: 'asc' }
    })

    return accounts.map((a) => this.mapper.toDomain(a))
  }

  async findByCode(accountCode: AccountCode): Promise<Account | null> {
    const account = await this.prisma.account.findUnique({
      where: { accountCode: accountCode.value }
    })

    return account ? this.mapper.toDomain(account) : null
  }

  async findByBsplDistinction(bsplDistinction: string): Promise<Account[]> {
    const accounts = await this.prisma.account.findMany({
      where: { bsplDistinction },
      orderBy: { displayOrder: 'asc' }
    })

    return accounts.map((a) => this.mapper.toDomain(a))
  }

  async findByTransactionDistinction(transactionDistinction: string): Promise<Account[]> {
    const accounts = await this.prisma.account.findMany({
      where: { transactionDistinction },
      orderBy: { displayOrder: 'asc' }
    })

    return accounts.map((a) => this.mapper.toDomain(a))
  }

  async deleteByCode(accountCode: AccountCode): Promise<void> {
    await this.prisma.account.delete({
      where: { accountCode: accountCode.value }
    })
  }

  async deleteAll(): Promise<void> {
    await this.prisma.account.deleteMany()
  }
}
