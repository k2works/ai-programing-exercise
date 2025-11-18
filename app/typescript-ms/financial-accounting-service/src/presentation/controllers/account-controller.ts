// src/presentation/controllers/account-controller.ts

import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify'
import { AccountRepository } from '../../application/ports/AccountRepository'
import { Account } from '../../domain/models/account/Account'
import { AccountCode } from '../../domain/models/account/AccountCode'
import { z } from 'zod'

// リクエストスキーマ
const createAccountSchema = z.object({
  accountCode: z.string().min(1).max(10),
  accountName: z.string().min(1).max(40),
  accountType: z.string(),
  accountKana: z.string().optional(),
  sumAccount: z.boolean().optional(),
  bsplDistinction: z.string().optional(),
  transactionDistinction: z.string().optional(),
  costDistinction: z.string().optional(),
  displayOrder: z.number().optional(),
  aggregationTarget: z.boolean().optional()
})

type CreateAccountRequest = z.infer<typeof createAccountSchema>

/**
 * 勘定科目コントローラー
 */
export async function accountController(
  fastify: FastifyInstance,
  accountRepository: AccountRepository
): Promise<void> {
  // GET /accounts - すべての勘定科目を取得
  fastify.get('/accounts', async (request: FastifyRequest, reply: FastifyReply) => {
    try {
      const accounts = await accountRepository.findAll()
      return reply.code(200).send({
        accounts: accounts.map((account) => toResponse(account))
      })
    } catch (error) {
      fastify.log.error(error)
      return reply.code(500).send({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Unknown error'
      })
    }
  })

  // GET /accounts/:code - 勘定科目を取得
  fastify.get<{ Params: { code: string } }>(
    '/accounts/:code',
    async (request: FastifyRequest<{ Params: { code: string } }>, reply: FastifyReply) => {
      try {
        const account = await accountRepository.findByCode(new AccountCode(request.params.code))

        if (!account) {
          return reply.code(404).send({
            error: 'Not Found',
            message: `Account with code ${request.params.code} not found`
          })
        }

        return reply.code(200).send(toResponse(account))
      } catch (error) {
        fastify.log.error(error)
        return reply.code(500).send({
          error: 'Internal Server Error',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  // POST /accounts - 勘定科目を作成
  fastify.post<{ Body: CreateAccountRequest }>(
    '/accounts',
    async (request: FastifyRequest<{ Body: CreateAccountRequest }>, reply: FastifyReply) => {
      try {
        // バリデーション
        const validatedData = createAccountSchema.parse(request.body)

        // ドメインモデルの作成
        const account = Account.create(
          validatedData.accountCode,
          validatedData.accountName,
          validatedData.accountType,
          {
            accountKana: validatedData.accountKana,
            sumAccount: validatedData.sumAccount,
            bsplDistinction: validatedData.bsplDistinction,
            transactionDistinction: validatedData.transactionDistinction,
            costDistinction: validatedData.costDistinction,
            displayOrder: validatedData.displayOrder,
            aggregationTarget: validatedData.aggregationTarget
          }
        )

        // 保存
        const saved = await accountRepository.save(account)

        return reply.code(201).send(toResponse(saved))
      } catch (error) {
        if (error instanceof z.ZodError) {
          return reply.code(400).send({
            error: 'Validation Error',
            details: error.errors
          })
        }

        fastify.log.error(error)
        return reply.code(500).send({
          error: 'Internal Server Error',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  // DELETE /accounts/:code - 勘定科目を削除
  fastify.delete<{ Params: { code: string } }>(
    '/accounts/:code',
    async (request: FastifyRequest<{ Params: { code: string } }>, reply: FastifyReply) => {
      try {
        await accountRepository.deleteByCode(new AccountCode(request.params.code))
        return reply.code(204).send()
      } catch (error) {
        fastify.log.error(error)
        return reply.code(500).send({
          error: 'Internal Server Error',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )
}

/**
 * ドメインモデルをレスポンス形式に変換
 */
function toResponse(account: Account) {
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
