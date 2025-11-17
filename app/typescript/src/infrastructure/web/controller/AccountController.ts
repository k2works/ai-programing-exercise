// src/infrastructure/web/controller/AccountController.ts
import { FastifyRequest, FastifyReply } from 'fastify'
import { AccountUseCase } from '../../../application/port/in/AccountUseCase'
import { CreateAccountRequestDto } from '../dto/CreateAccountRequestDto'
import { UpdateAccountRequestDto } from '../dto/UpdateAccountRequestDto'
import { AccountResponseDto } from '../dto/AccountResponseDto'

/**
 * 勘定科目コントローラー（Input Adapter）
 * Fastify を使用して HTTP リクエストを処理
 */
export class AccountController {
  constructor(private accountUseCase: AccountUseCase) {}

  /**
   * POST /accounts
   * 新しい勘定科目を作成
   */
  async createAccount(
    request: FastifyRequest<{ Body: CreateAccountRequestDto }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const account = await this.accountUseCase.createAccount(request.body)
      const response = AccountResponseDto.fromDomain(account)

      reply.status(201).send({
        success: true,
        data: { account: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * GET /accounts
   * すべての勘定科目を取得
   */
  async getAllAccounts(request: FastifyRequest, reply: FastifyReply): Promise<void> {
    const accounts = await this.accountUseCase.getAllAccounts()
    const response = accounts.map((a) => AccountResponseDto.fromDomain(a))

    reply.send({
      success: true,
      data: { accounts: response }
    })
  }

  /**
   * GET /accounts/:code
   * 科目コードで勘定科目を取得
   */
  async getAccountByCode(
    request: FastifyRequest<{ Params: { code: string } }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const account = await this.accountUseCase.getAccountByCode(request.params.code)
      const response = AccountResponseDto.fromDomain(account)

      reply.send({
        success: true,
        data: { account: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * GET /accounts/bspl/:distinction
   * BSPL区分で勘定科目を取得
   */
  async getAccountsByBspl(
    request: FastifyRequest<{ Params: { distinction: string } }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const accounts = await this.accountUseCase.getAccountsByBspl(request.params.distinction)
      const response = accounts.map((a) => AccountResponseDto.fromDomain(a))

      reply.send({
        success: true,
        data: { accounts: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * PUT /accounts/:code
   * 勘定科目を更新
   */
  async updateAccount(
    request: FastifyRequest<{
      Params: { code: string }
      Body: UpdateAccountRequestDto
    }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const account = await this.accountUseCase.updateAccount({
        accountCode: request.params.code,
        ...request.body
      })
      const response = AccountResponseDto.fromDomain(account)

      reply.send({
        success: true,
        data: { account: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * DELETE /accounts/:code
   * 勘定科目を削除
   */
  async deleteAccount(
    request: FastifyRequest<{ Params: { code: string } }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      await this.accountUseCase.deleteAccount(request.params.code)
      reply.status(204).send()
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  private handleError(error: unknown, reply: FastifyReply): void {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error'

    if (errorMessage.includes('が見つかりません')) {
      reply.status(404).send({
        success: false,
        error: errorMessage,
        code: 'NOT_FOUND'
      })
    } else if (errorMessage.includes('既に使用されています')) {
      reply.status(409).send({
        success: false,
        error: errorMessage,
        code: 'DUPLICATE'
      })
    } else {
      reply.status(400).send({
        success: false,
        error: errorMessage,
        code: 'VALIDATION_ERROR'
      })
    }
  }
}
