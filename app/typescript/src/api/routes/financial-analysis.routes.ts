// src/api/routes/financial-analysis.routes.ts
import { FastifyInstance } from 'fastify'
import { PrismaClient } from '@prisma/client'
import { prisma as defaultPrisma } from '../lib/prisma'
import { JournalPersistenceAdapter } from '../../infrastructure/persistence/JournalPersistenceAdapter'
import { FinancialAnalysisService } from '../../application/port/in/FinancialAnalysisService'
import { FinancialAnalysisController } from '../../infrastructure/web/controller/FinancialAnalysisController'

/**
 * 財務分析 API ルート
 * 依存関係の配線と Fastify へのルート登録
 *
 * @param app - Fastify インスタンス
 * @param options - オプション（テスト時に Prisma インスタンスをオーバーライド可能）
 */
export async function financialAnalysisRoutes(
  app: FastifyInstance,
  options?: { prisma?: PrismaClient }
): Promise<void> {
  // 依存関係の構築（Dependency Injection）
  const prisma = options?.prisma || defaultPrisma
  const journalRepository = new JournalPersistenceAdapter(prisma)
  const financialAnalysisService = new FinancialAnalysisService(journalRepository)
  const financialAnalysisController = new FinancialAnalysisController(financialAnalysisService)

  // ルートを登録
  await financialAnalysisController.registerRoutes(app)
}
