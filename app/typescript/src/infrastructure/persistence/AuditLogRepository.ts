// src/infrastructure/persistence/AuditLogRepository.ts
import { PrismaClient } from '@prisma/client'
import { AuditLog } from '../../domain/model/audit/AuditLog'
import { AuditAction } from '../../domain/model/audit/AuditAction'

export class AuditLogRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async save(auditLog: AuditLog): Promise<AuditLog> {
    const saved = await this.prisma.auditLog.create({
      data: {
        entityType: auditLog.entityType,
        entityId: auditLog.entityId,
        action: auditLog.action,
        userId: auditLog.userId,
        userName: auditLog.userName,
        timestamp: auditLog.timestamp,
        oldValues: auditLog.oldValues,
        newValues: auditLog.newValues,
        changes: auditLog.changes,
        reason: auditLog.reason,
        ipAddress: auditLog.ipAddress,
        userAgent: auditLog.userAgent
      }
    })

    return AuditLog.reconstruct({
      id: saved.id,
      entityType: saved.entityType,
      entityId: saved.entityId,
      action: saved.action as AuditAction,
      userId: saved.userId,
      userName: saved.userName,
      timestamp: saved.timestamp,
      oldValues: saved.oldValues as Record<string, any> | undefined,
      newValues: saved.newValues as Record<string, any> | undefined,
      changes: saved.changes as Record<string, any> | undefined,
      reason: saved.reason || undefined,
      ipAddress: saved.ipAddress || undefined,
      userAgent: saved.userAgent || undefined
    })
  }

  async findByEntity(entityType: string, entityId: string): Promise<AuditLog[]> {
    const logs = await this.prisma.auditLog.findMany({
      where: { entityType, entityId },
      orderBy: { timestamp: 'desc' }
    })

    return logs.map((log) => this.mapToDomain(log))
  }

  async findByUser(userId: string, startDate?: Date, endDate?: Date): Promise<AuditLog[]> {
    const logs = await this.prisma.auditLog.findMany({
      where: {
        userId,
        timestamp: { gte: startDate, lte: endDate }
      },
      orderBy: { timestamp: 'desc' }
    })

    return logs.map((log) => this.mapToDomain(log))
  }

  async findByPeriod(startDate: Date, endDate: Date): Promise<AuditLog[]> {
    const logs = await this.prisma.auditLog.findMany({
      where: {
        timestamp: { gte: startDate, lte: endDate }
      },
      orderBy: { timestamp: 'desc' }
    })

    return logs.map((log) => this.mapToDomain(log))
  }

  async findByAction(action: AuditAction, limit: number = 100): Promise<AuditLog[]> {
    const logs = await this.prisma.auditLog.findMany({
      where: { action },
      orderBy: { timestamp: 'desc' },
      take: limit
    })

    return logs.map((log) => this.mapToDomain(log))
  }

  private mapToDomain(log: any): AuditLog {
    return AuditLog.reconstruct({
      id: log.id,
      entityType: log.entityType,
      entityId: log.entityId,
      action: log.action as AuditAction,
      userId: log.userId,
      userName: log.userName,
      timestamp: log.timestamp,
      oldValues: log.oldValues as Record<string, any> | undefined,
      newValues: log.newValues as Record<string, any> | undefined,
      changes: log.changes as Record<string, any> | undefined,
      reason: log.reason || undefined,
      ipAddress: log.ipAddress || undefined,
      userAgent: log.userAgent || undefined
    })
  }
}
