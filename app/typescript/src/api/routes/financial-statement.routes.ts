// src/api/routes/financial-statement.routes.ts
import { FastifyInstance } from 'fastify'
import { PrismaClient } from '@prisma/client'
import { prisma as defaultPrisma } from '../lib/prisma'
import { FinancialStatementService } from '../../application/port/in/FinancialStatementService'
import { FinancialStatementController } from '../../infrastructure/web/controller/FinancialStatementController'

/**
 * 財務諸表 API ルート
 * 依存関係の配線と Fastify へのルート登録
 *
 * @param app - Fastify インスタンス
 * @param options - オプション（テスト時に Prisma インスタンスをオーバーライド可能）
 */
export async function financialStatementRoutes(
  app: FastifyInstance,
  options?: { prisma?: PrismaClient }
): Promise<void> {
  // 依存関係の構築（Dependency Injection）
  const prisma = options?.prisma || defaultPrisma
  const financialStatementService = new FinancialStatementService(prisma)
  const financialStatementController = new FinancialStatementController(financialStatementService)

  /**
   * GET /statements/balance-sheet - 貸借対照表を取得
   */
  app.get<{ Querystring: { asOfDate: string } }>(
    '/statements/balance-sheet',
    {
      schema: {
        tags: ['statements'],
        summary: '貸借対照表を取得',
        description: '指定された基準日時点の貸借対照表を生成します',
        querystring: {
          type: 'object',
          properties: {
            asOfDate: {
              type: 'string',
              format: 'date',
              description: '基準日（YYYY-MM-DD）'
            }
          },
          required: ['asOfDate']
        }
      }
    },
    async (request, reply) => {
      await financialStatementController.getBalanceSheet(request, reply)
    }
  )

  /**
   * GET /statements/income-statement - 損益計算書を取得
   */
  app.get<{ Querystring: { fromDate: string; toDate: string } }>(
    '/statements/income-statement',
    {
      schema: {
        tags: ['statements'],
        summary: '損益計算書を取得',
        description: '指定された期間の損益計算書を生成します',
        querystring: {
          type: 'object',
          properties: {
            fromDate: {
              type: 'string',
              format: 'date',
              description: '開始日（YYYY-MM-DD）'
            },
            toDate: {
              type: 'string',
              format: 'date',
              description: '終了日（YYYY-MM-DD）'
            }
          },
          required: ['fromDate', 'toDate']
        }
      }
    },
    async (request, reply) => {
      await financialStatementController.getIncomeStatement(request, reply)
    }
  )
}
