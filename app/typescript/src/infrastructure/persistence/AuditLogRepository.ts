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
      oldValues: saved.oldValues as Record<string, unknown> | undefined,
      newValues: saved.newValues as Record<string, unknown> | undefined,
      changes: saved.changes as Record<string, unknown> | undefined,
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

  private mapToDomain(log: unknown): AuditLog {
    const logData = log as {
      id: number
      entityType: string
      entityId: string
      action: string
      userId: string
      userName: string
      timestamp: Date
      oldValues: unknown
      newValues: unknown
      changes: unknown
      reason: string | null
      ipAddress: string | null
      userAgent: string | null
    }
    return AuditLog.reconstruct({
      id: logData.id,
      entityType: logData.entityType,
      entityId: logData.entityId,
      action: logData.action as AuditAction,
      userId: logData.userId,
      userName: logData.userName,
      timestamp: logData.timestamp,
      oldValues: logData.oldValues as Record<string, unknown> | undefined,
      newValues: logData.newValues as Record<string, unknown> | undefined,
      changes: logData.changes as Record<string, unknown> | undefined,
      reason: logData.reason || undefined,
      ipAddress: logData.ipAddress || undefined,
      userAgent: logData.userAgent || undefined
    })
  }
}
