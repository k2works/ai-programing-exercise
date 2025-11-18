// src/infrastructure/web/controller/audit/AuditLogController.ts
import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify'
import { AuditLogUseCase } from '../../../../application/audit/port/in/AuditLogUseCase'
import { AuditAction } from '../../../../domain/audit/AuditAction'

/**
 * 監査ログコントローラー（Input Adapter）
 *
 * Fastify を使用して監査ログ API のエンドポイントを提供
 * ヘキサゴナルアーキテクチャの Input Adapter として機能
 */
export class AuditLogController {
  constructor(private readonly auditLogUseCase: AuditLogUseCase) {}

  /**
   * ルートを登録
   */
  registerRoutes(app: FastifyInstance): void {
    // エンティティの監査ログを取得
    app.get(
      '/audit-logs/entity/:entityType/:entityId',
      {
        schema: {
          tags: ['audit'],
          summary: 'エンティティの監査ログ取得',
          description: '特定のエンティティに関する全ての監査ログを取得します',
          params: {
            type: 'object',
            properties: {
              entityType: {
                type: 'string',
                description: 'エンティティタイプ（Journal, Account等）'
              },
              entityId: {
                type: 'string',
                description: 'エンティティID'
              }
            },
            required: ['entityType', 'entityId']
          },
          response: {
            200: {
              description: '成功',
              type: 'array',
              items: {
                type: 'object',
                properties: {
                  id: { type: 'number' },
                  entityType: { type: 'string' },
                  entityId: { type: 'string' },
                  action: { type: 'string' },
                  userId: { type: 'string' },
                  userName: { type: 'string' },
                  timestamp: { type: 'string', format: 'date-time' },
                  oldValues: { type: 'object' },
                  newValues: { type: 'object' },
                  changes: { type: 'object' },
                  reason: { type: 'string' }
                }
              }
            }
          }
        }
      },
      this.getAuditLogsByEntity.bind(this)
    )

    // ユーザーの操作履歴を取得
    app.get(
      '/audit-logs/user/:userId',
      {
        schema: {
          tags: ['audit'],
          summary: 'ユーザーの操作履歴取得',
          params: {
            type: 'object',
            properties: {
              userId: { type: 'string', description: 'ユーザーID' }
            },
            required: ['userId']
          },
          querystring: {
            type: 'object',
            properties: {
              startDate: {
                type: 'string',
                format: 'date-time',
                description: '開始日時（省略可）'
              },
              endDate: {
                type: 'string',
                format: 'date-time',
                description: '終了日時（省略可）'
              }
            }
          },
          response: {
            200: {
              description: '成功',
              type: 'array',
              items: { type: 'object' }
            }
          }
        }
      },
      this.getAuditLogsByUser.bind(this)
    )

    // 期間指定で監査ログを取得
    app.get(
      '/audit-logs/period',
      {
        schema: {
          tags: ['audit'],
          summary: '期間指定での監査ログ取得',
          querystring: {
            type: 'object',
            properties: {
              startDate: {
                type: 'string',
                format: 'date-time',
                description: '開始日時'
              },
              endDate: {
                type: 'string',
                format: 'date-time',
                description: '終了日時'
              }
            },
            required: ['startDate', 'endDate']
          },
          response: {
            200: {
              description: '成功',
              type: 'array',
              items: { type: 'object' }
            }
          }
        }
      },
      this.getAuditLogsByPeriod.bind(this)
    )

    // アクション種別で監査ログを取得
    app.get(
      '/audit-logs/action/:action',
      {
        schema: {
          tags: ['audit'],
          summary: 'アクション種別での監査ログ取得',
          params: {
            type: 'object',
            properties: {
              action: {
                type: 'string',
                enum: ['CREATE', 'UPDATE', 'DELETE', 'READ'],
                description: 'アクション種別'
              }
            },
            required: ['action']
          },
          querystring: {
            type: 'object',
            properties: {
              limit: {
                type: 'number',
                description: '取得件数（デフォルト: 100、最大: 1000）',
                default: 100
              }
            }
          },
          response: {
            200: {
              description: '成功',
              type: 'array',
              items: { type: 'object' }
            }
          }
        }
      },
      this.getAuditLogsByAction.bind(this)
    )
  }

  /**
   * エンティティの監査ログを取得
   */
  private async getAuditLogsByEntity(
    request: FastifyRequest<{
      Params: { entityType: string; entityId: string }
    }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const { entityType, entityId } = request.params

      const logs = await this.auditLogUseCase.getAuditLogsByEntity(entityType, entityId)

      const response = logs.map((log) => ({
        id: log.id,
        entityType: log.entityType,
        entityId: log.entityId,
        action: log.action,
        userId: log.userId,
        userName: log.userName,
        timestamp: log.timestamp.toISOString(),
        oldValues: log.oldValues,
        newValues: log.newValues,
        changes: log.changes,
        reason: log.reason,
        summary: log.getChangeSummary()
      }))

      reply.send(response)
    } catch (error) {
      reply.status(500).send({
        statusCode: 500,
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : '監査ログの取得に失敗しました'
      })
    }
  }

  /**
   * ユーザーの操作履歴を取得
   */
  private async getAuditLogsByUser(
    request: FastifyRequest<{
      Params: { userId: string }
      Querystring: { startDate?: string; endDate?: string }
    }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const { userId } = request.params
      const { startDate, endDate } = request.query

      const logs = await this.auditLogUseCase.getAuditLogsByUser(
        userId,
        startDate ? new Date(startDate) : undefined,
        endDate ? new Date(endDate) : undefined
      )

      const response = logs.map((log) => ({
        id: log.id,
        entityType: log.entityType,
        entityId: log.entityId,
        action: log.action,
        timestamp: log.timestamp.toISOString(),
        oldValues: log.oldValues,
        newValues: log.newValues,
        changes: log.changes,
        reason: log.reason,
        summary: log.getChangeSummary()
      }))

      reply.send(response)
    } catch (error) {
      reply.status(400).send({
        statusCode: 400,
        error: 'Bad Request',
        message: error instanceof Error ? error.message : 'リクエストが不正です'
      })
    }
  }

  /**
   * 期間指定で監査ログを取得
   */
  private async getAuditLogsByPeriod(
    request: FastifyRequest<{
      Querystring: { startDate: string; endDate: string }
    }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const { startDate, endDate } = request.query

      const logs = await this.auditLogUseCase.getAuditLogsByPeriod(
        new Date(startDate),
        new Date(endDate)
      )

      const response = logs.map((log) => ({
        id: log.id,
        entityType: log.entityType,
        entityId: log.entityId,
        action: log.action,
        userId: log.userId,
        userName: log.userName,
        timestamp: log.timestamp.toISOString(),
        summary: log.getChangeSummary()
      }))

      reply.send(response)
    } catch (error) {
      reply.status(400).send({
        statusCode: 400,
        error: 'Bad Request',
        message: error instanceof Error ? error.message : 'リクエストが不正です'
      })
    }
  }

  /**
   * アクション種別で監査ログを取得
   */
  private async getAuditLogsByAction(
    request: FastifyRequest<{
      Params: { action: string }
      Querystring: { limit?: number }
    }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const { action } = request.params
      const { limit } = request.query

      const logs = await this.auditLogUseCase.getAuditLogsByAction(action as AuditAction, limit)

      const response = logs.map((log) => ({
        id: log.id,
        entityType: log.entityType,
        entityId: log.entityId,
        action: log.action,
        userId: log.userId,
        userName: log.userName,
        timestamp: log.timestamp.toISOString(),
        summary: log.getChangeSummary()
      }))

      reply.send(response)
    } catch (error) {
      reply.status(400).send({
        statusCode: 400,
        error: 'Bad Request',
        message: error instanceof Error ? error.message : 'リクエストが不正です'
      })
    }
  }
}
