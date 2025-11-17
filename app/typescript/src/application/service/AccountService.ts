// src/application/service/AccountService.ts
import {
  AccountUseCase,
  CreateAccountCommand,
  UpdateAccountCommand
} from '../port/in/AccountUseCase'
import { AccountRepository } from '../port/out/AccountRepository'
import { Account } from '../../domain/model/account/Account'
import { AccountCode } from '../../domain/model/account/AccountCode'

/**
 * 勘定科目サービス（Application Service）
 * Input Port の実装としてビジネスロジックを提供
 */
export class AccountService implements AccountUseCase {
  constructor(private accountRepository: AccountRepository) {}

  async createAccount(command: CreateAccountCommand): Promise<Account> {
    // 1. 重複チェック
    const accountCode = new AccountCode(command.accountCode)
    const existing = await this.accountRepository.findByCode(accountCode)
    if (existing) {
      throw new Error(`科目コード ${command.accountCode} は既に使用されています`)
    }

    // 2. ドメインモデル生成（ビジネスルール検証を含む）
    const account = Account.create(command.accountCode, command.accountName, command.accountType, {
      accountKana: command.accountKana,
      sumAccount: command.sumAccount,
      bsplDistinction: command.bsplDistinction,
      transactionDistinction: command.transactionDistinction,
      costDistinction: command.costDistinction,
      displayOrder: command.displayOrder,
      aggregationTarget: command.aggregationTarget
    })

    // 3. 永続化
    return await this.accountRepository.save(account)
  }

  async getAllAccounts(): Promise<Account[]> {
    return await this.accountRepository.findAll()
  }

  async getAccountByCode(accountCode: string): Promise<Account> {
    const code = new AccountCode(accountCode)
    const account = await this.accountRepository.findByCode(code)
    if (!account) {
      throw new Error(`科目コード ${accountCode} が見つかりません`)
    }
    return account
  }

  async getAccountsByBspl(bsplDistinction: string): Promise<Account[]> {
    if (!['B', 'P'].includes(bsplDistinction)) {
      throw new Error('BSPL区分は "B" または "P" である必要があります')
    }
    return await this.accountRepository.findByBsplDistinction(bsplDistinction)
  }

  async updateAccount(command: UpdateAccountCommand): Promise<Account> {
    // 1. 既存の勘定科目を取得
    const accountCode = new AccountCode(command.accountCode)
    const existing = await this.accountRepository.findByCode(accountCode)
    if (!existing) {
      throw new Error(`科目コード ${command.accountCode} が見つかりません`)
    }

    // 2. 更新処理（ドメインモデルのメソッドを使用）
    if (command.accountName !== undefined) {
      existing.changeName(command.accountName, command.accountKana)
    }
    if (command.displayOrder !== undefined) {
      existing.setDisplayOrder(command.displayOrder)
    }

    // 3. 永続化
    return await this.accountRepository.save(existing)
  }

  async deleteAccount(accountCode: string): Promise<void> {
    const code = new AccountCode(accountCode)
    const existing = await this.accountRepository.findByCode(code)
    if (!existing) {
      throw new Error(`科目コード ${accountCode} が見つかりません`)
    }

    await this.accountRepository.deleteByCode(code)
  }
}
