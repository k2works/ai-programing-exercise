// src/presentation/controllers/journal-controller.ts

import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify'
import { JournalRepository, Journal } from '../../application/ports/JournalRepository'
import { EventPublisher } from '../../infrastructure/messaging/EventPublisher'
import { z } from 'zod'

// リクエストスキーマ
const journalDetailItemSchema = z.object({
  accountCode: z.string().min(1).max(10),
  debitAmount: z.number().min(0),
  creditAmount: z.number().min(0),
  description: z.string().optional()
})

const createJournalSchema = z.object({
  journalDate: z.string().transform((val) => new Date(val)),
  fiscalYear: z.number(),
  description: z.string().min(1).max(200),
  userId: z.string().optional(),
  userName: z.string().optional(),
  detailItems: z.array(journalDetailItemSchema).min(2) // 仕訳は最低2行必要
})

type CreateJournalRequest = z.infer<typeof createJournalSchema>

/**
 * 仕訳コントローラー
 */
export async function journalController(
  fastify: FastifyInstance,
  journalRepository: JournalRepository,
  eventPublisher: EventPublisher
): Promise<void> {
  // GET /journals - すべての仕訳を取得
  fastify.get('/journals', async (request: FastifyRequest, reply: FastifyReply) => {
    try {
      const journals = await journalRepository.findAll()
      return reply.code(200).send({
        journals: journals.map((journal) => toResponse(journal))
      })
    } catch (error) {
      fastify.log.error(error)
      return reply.code(500).send({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Unknown error'
      })
    }
  })

  // GET /journals/:id - 仕訳を取得
  fastify.get<{ Params: { id: string } }>(
    '/journals/:id',
    async (request: FastifyRequest<{ Params: { id: string } }>, reply: FastifyReply) => {
      try {
        const id = parseInt(request.params.id, 10)
        if (isNaN(id)) {
          return reply.code(400).send({
            error: 'Bad Request',
            message: 'Invalid journal ID'
          })
        }

        const journal = await journalRepository.findById(id)

        if (!journal) {
          return reply.code(404).send({
            error: 'Not Found',
            message: `Journal with id ${id} not found`
          })
        }

        return reply.code(200).send(toResponse(journal))
      } catch (error) {
        fastify.log.error(error)
        return reply.code(500).send({
          error: 'Internal Server Error',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  // GET /journals/fiscal-year/:year - 会計年度で仕訳を取得
  fastify.get<{ Params: { year: string } }>(
    '/journals/fiscal-year/:year',
    async (request: FastifyRequest<{ Params: { year: string } }>, reply: FastifyReply) => {
      try {
        const year = parseInt(request.params.year, 10)
        if (isNaN(year)) {
          return reply.code(400).send({
            error: 'Bad Request',
            message: 'Invalid fiscal year'
          })
        }

        const journals = await journalRepository.findByFiscalYear(year)
        return reply.code(200).send({
          journals: journals.map((journal) => toResponse(journal))
        })
      } catch (error) {
        fastify.log.error(error)
        return reply.code(500).send({
          error: 'Internal Server Error',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  // POST /journals - 仕訳を作成
  fastify.post<{ Body: CreateJournalRequest }>(
    '/journals',
    async (request: FastifyRequest<{ Body: CreateJournalRequest }>, reply: FastifyReply) => {
      try {
        // バリデーション
        const validatedData = createJournalSchema.parse(request.body)

        // 貸借一致チェック
        const totalDebit = validatedData.detailItems.reduce((sum, item) => sum + item.debitAmount, 0)
        const totalCredit = validatedData.detailItems.reduce(
          (sum, item) => sum + item.creditAmount,
          0
        )

        if (Math.abs(totalDebit - totalCredit) > 0.01) {
          // 浮動小数点誤差を考慮
          return reply.code(400).send({
            error: 'Validation Error',
            message: 'Debit and credit amounts must be equal'
          })
        }

        // 仕訳の作成
        const journal: Journal = {
          journalDate: validatedData.journalDate,
          fiscalYear: validatedData.fiscalYear,
          description: validatedData.description,
          userId: validatedData.userId,
          userName: validatedData.userName,
          detailItems: validatedData.detailItems
        }

        // 保存
        const saved = await journalRepository.save(journal)

        // イベント発行
        await eventPublisher.publish('journal.created', {
          eventType: 'JournalCreated',
          occurredAt: new Date(),
          payload: {
            journalId: saved.id?.toString() || '',
            fiscalYear: saved.fiscalYear,
            journalDate: saved.journalDate,
            totalDebitAmount: totalDebit,
            totalCreditAmount: totalCredit
          }
        })

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

  // DELETE /journals/:id - 仕訳を削除
  fastify.delete<{ Params: { id: string } }>(
    '/journals/:id',
    async (request: FastifyRequest<{ Params: { id: string } }>, reply: FastifyReply) => {
      try {
        const id = parseInt(request.params.id, 10)
        if (isNaN(id)) {
          return reply.code(400).send({
            error: 'Bad Request',
            message: 'Invalid journal ID'
          })
        }

        await journalRepository.deleteById(id)
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
function toResponse(journal: Journal) {
  return {
    id: journal.id,
    journalDate: journal.journalDate,
    fiscalYear: journal.fiscalYear,
    description: journal.description,
    userId: journal.userId,
    userName: journal.userName,
    detailItems: journal.detailItems.map((item) => ({
      id: item.id,
      accountCode: item.accountCode,
      debitAmount: item.debitAmount,
      creditAmount: item.creditAmount,
      description: item.description
    }))
  }
}
