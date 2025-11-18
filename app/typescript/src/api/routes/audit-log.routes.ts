// src/api/routes/audit-log.routes.ts
import { FastifyInstance } from 'fastify'
import { PrismaClient } from '@prisma/client'
import { prisma as defaultPrisma } from '../lib/prisma'
import { AuditLogRepository } from '../../infrastructure/persistence/audit/AuditLogRepository'
import { AuditLogService } from '../../application/audit/service/AuditLogService'
import { AuditLogController } from '../../infrastructure/web/controller/audit/AuditLogController'

/**
 * 監査ログ API ルート
 * 依存関係の配線と Fastify へのルート登録
 *
 * @param app - Fastify インスタンス
 * @param options - オプション（テスト時に Prisma インスタンスをオーバーライド可能）
 */
export async function auditLogRoutes(
  app: FastifyInstance,
  options?: { prisma?: PrismaClient }
): Promise<void> {
  // 依存関係の構築（Dependency Injection）
  const prisma = options?.prisma || defaultPrisma
  const auditLogRepository = new AuditLogRepository(prisma)
  const auditLogService = new AuditLogService(auditLogRepository)
  const auditLogController = new AuditLogController(auditLogService)

  // ルートを登録
  auditLogController.registerRoutes(app)
}
