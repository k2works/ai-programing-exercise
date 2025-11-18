// src/audit/adapter/web/setup.ts
import { FastifyInstance } from 'fastify'
import { PrismaClient } from '@prisma/client'
import { AuditLogRepository } from '../../infrastructure/persistence/AuditLogRepository'
import { AuditLogService } from '../../application/service/AuditLogService'
import { AuditLogController } from './AuditLogController'

/**
 * 監査 API のセットアップ
 *
 * 依存性の注入とルートの登録を行う
 *
 * @param app - Fastify インスタンス
 * @param prisma - Prisma Client インスタンス
 */
export function setupAuditApi(app: FastifyInstance, prisma: PrismaClient): void {
  // 依存性の注入
  const auditLogRepository = new AuditLogRepository(prisma)
  const auditLogService = new AuditLogService(auditLogRepository)
  const auditLogController = new AuditLogController(auditLogService)

  // ルートを登録
  auditLogController.registerRoutes(app)
}
