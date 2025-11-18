// src/infrastructure/web/controller/FinancialAnalysisController.ts

import type { FastifyInstance } from 'fastify'
import type { FinancialAnalysisUseCase } from '../../../application/port/in/FinancialAnalysisUseCase'

/**
 * 財務分析 API のコントローラー
 */
export class FinancialAnalysisController {
  constructor(private readonly financialAnalysisUseCase: FinancialAnalysisUseCase) {}

  /**
   * ルートを登録
   */
  async registerRoutes(fastify: FastifyInstance): Promise<void> {
    // 単一年度の分析
    fastify.get<{ Params: { fiscalYear: string } }>(
      '/financial-analysis/:fiscalYear',
      {
        schema: {
          description: '指定された会計年度の財務分析を取得',
          tags: ['financial-analysis'],
          params: {
            type: 'object',
            required: ['fiscalYear'],
            properties: {
              fiscalYear: { type: 'string', description: '会計年度（例：2021）' }
            }
          }
          // response schema は省略（プロパティストリップ問題を回避）
        }
      },
      async (request, reply) => {
        try {
          const fiscalYear = parseInt(request.params.fiscalYear, 10)
          if (isNaN(fiscalYear)) {
            return reply.status(400).send({ error: '会計年度は数値で指定してください' })
          }
          const result = await this.financialAnalysisUseCase.analyzeByFiscalYear(fiscalYear)
          return reply.send(result)
        } catch (error) {
          if (error instanceof Error) {
            return reply.status(400).send({ error: error.message })
          }
          return reply.status(500).send({ error: '予期しないエラーが発生しました' })
        }
      }
    )

    // 複数年度の比較分析
    fastify.get<{ Querystring: { years: string } }>(
      '/financial-analysis/compare',
      {
        schema: {
          description: '複数期間の財務分析を比較',
          tags: ['financial-analysis'],
          querystring: {
            type: 'object',
            required: ['years'],
            properties: {
              years: {
                type: 'string',
                description: '会計年度のカンマ区切りリスト（例：2021,2022）'
              }
            }
          }
          // response schema は省略（プロパティストリップ問題を回避）
        }
      },
      async (request, reply) => {
        try {
          const years = request.query.years.split(',').map((y) => parseInt(y.trim(), 10))
          if (years.some((y) => isNaN(y))) {
            return reply.status(400).send({ error: '会計年度は数値で指定してください' })
          }
          if (years.length < 2) {
            return reply.status(400).send({ error: '比較には2つ以上の会計年度が必要です' })
          }
          const result = await this.financialAnalysisUseCase.compareMultiplePeriods(years)
          return reply.send(result)
        } catch (error) {
          if (error instanceof Error) {
            return reply.status(400).send({ error: error.message })
          }
          return reply.status(500).send({ error: '予期しないエラーが発生しました' })
        }
      }
    )
  }
}
