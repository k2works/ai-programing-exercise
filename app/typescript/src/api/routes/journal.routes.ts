// src/api/routes/journal.routes.ts
import { FastifyInstance } from 'fastify'
import { PrismaClient } from '@prisma/client'
import { prisma as defaultPrisma } from '../lib/prisma'
import { JournalPersistenceAdapter } from '../../infrastructure/persistence/JournalPersistenceAdapter'
import { JournalService } from '../../application/port/in/JournalService'
import { JournalController } from '../../infrastructure/web/controller/JournalController'
import {
  CreateJournalRequestDto,
  UpdateJournalRequestDto
} from '../../infrastructure/web/dto/JournalRequestDto'

/**
 * 仕訳 API ルート
 * 依存関係の配線と Fastify へのルート登録
 *
 * @param app - Fastify インスタンス
 * @param options - オプション（テスト時に Prisma インスタンスをオーバーライド可能）
 */
export async function journalRoutes(
  app: FastifyInstance,
  options?: { prisma?: PrismaClient }
): Promise<void> {
  // 依存関係の構築（Dependency Injection）
  const prisma = options?.prisma || defaultPrisma
  const journalRepository = new JournalPersistenceAdapter(prisma)
  const journalService = new JournalService(journalRepository)
  const journalController = new JournalController(journalService)

  /**
   * POST /journals - 仕訳を作成
   */
  app.post<{ Body: CreateJournalRequestDto }>(
    '/journals',
    {
      schema: {
        tags: ['journals'],
        summary: '仕訳を作成',
        description: '新しい仕訳を作成します'
        // response schema は省略（プロパティストリップ問題を回避）
      }
    },
    async (request, reply) => {
      await journalController.createJournal(request, reply)
    }
  )

  /**
   * GET /journals - 全仕訳を取得
   */
  app.get(
    '/journals',
    {
      schema: {
        tags: ['journals'],
        summary: '全仕訳を取得',
        description: 'すべての仕訳を取得します（起票日降順）'
      }
    },
    async (request, reply) => {
      await journalController.getAllJournals(request, reply)
    }
  )

  /**
   * GET /journals/:voucherNo - 伝票番号で仕訳を取得
   */
  app.get<{ Params: { voucherNo: string } }>(
    '/journals/:voucherNo',
    {
      schema: {
        tags: ['journals'],
        summary: '伝票番号で仕訳を取得',
        description: '指定された伝票番号の仕訳を取得します',
        params: {
          type: 'object',
          properties: {
            voucherNo: { type: 'string', description: '伝票番号' }
          },
          required: ['voucherNo']
        }
      }
    },
    async (request, reply) => {
      await journalController.getJournalByVoucherNo(request, reply)
    }
  )

  /**
   * GET /journals/date-range - 日付範囲で仕訳を取得
   */
  app.get<{ Querystring: { startDate: string; endDate: string } }>(
    '/journals/date-range',
    {
      schema: {
        tags: ['journals'],
        summary: '日付範囲で仕訳を取得',
        description: '指定された日付範囲の仕訳を取得します',
        querystring: {
          type: 'object',
          properties: {
            startDate: { type: 'string', format: 'date', description: '開始日（YYYY-MM-DD）' },
            endDate: { type: 'string', format: 'date', description: '終了日（YYYY-MM-DD）' }
          },
          required: ['startDate', 'endDate']
        }
      }
    },
    async (request, reply) => {
      await journalController.getJournalsByDateRange(request, reply)
    }
  )

  /**
   * PUT /journals/:voucherNo - 仕訳を更新
   */
  app.put<{ Params: { voucherNo: string }; Body: UpdateJournalRequestDto }>(
    '/journals/:voucherNo',
    {
      schema: {
        tags: ['journals'],
        summary: '仕訳を更新',
        description: '指定された伝票番号の仕訳を更新します',
        params: {
          type: 'object',
          properties: {
            voucherNo: { type: 'string', description: '伝票番号' }
          },
          required: ['voucherNo']
        }
      }
    },
    async (request, reply) => {
      await journalController.updateJournal(request, reply)
    }
  )

  /**
   * DELETE /journals/:voucherNo - 仕訳を削除
   */
  app.delete<{ Params: { voucherNo: string } }>(
    '/journals/:voucherNo',
    {
      schema: {
        tags: ['journals'],
        summary: '仕訳を削除',
        description: '指定された伝票番号の仕訳を削除します',
        params: {
          type: 'object',
          properties: {
            voucherNo: { type: 'string', description: '伝票番号' }
          },
          required: ['voucherNo']
        }
      }
    },
    async (request, reply) => {
      await journalController.deleteJournal(request, reply)
    }
  )
}
