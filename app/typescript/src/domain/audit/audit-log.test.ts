// src/domain/audit/audit-log.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { AuditLog } from './AuditLog'
import { AuditAction } from './AuditAction'

describe('監査ログ テーブル設計', () => {
  let testDb: TestDatabase

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
  }, 60000)

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  describe('レッド：監査ログの基本機能', () => {
    test('仕訳の作成操作を記録できる', async () => {
      const auditLog = AuditLog.create({
        entityType: 'Journal',
        entityId: '1',
        action: AuditAction.CREATE,
        userId: 'user001',
        userName: '山田太郎',
        changes: {
          journalDate: '2025-01-15',
          description: '売上計上',
          details: [
            { accountCode: '1001', debitOrCredit: '借方', amount: 100000 },
            { accountCode: '4001', debitOrCredit: '貸方', amount: 100000 }
          ]
        },
        ipAddress: '192.168.1.100',
        userAgent: 'Mozilla/5.0...'
      })

      expect(auditLog.entityType).toBe('Journal')
      expect(auditLog.action).toBe(AuditAction.CREATE)
      expect(auditLog.userId).toBe('user001')
    })

    test('勘定科目の更新操作を記録できる', async () => {
      const auditLog = AuditLog.create({
        entityType: 'Account',
        entityId: '1001',
        action: AuditAction.UPDATE,
        userId: 'user001',
        userName: '山田太郎',
        oldValues: {
          accountName: '現金'
        },
        newValues: {
          accountName: '現金及び預金'
        },
        ipAddress: '192.168.1.100'
      })

      expect(auditLog.action).toBe(AuditAction.UPDATE)
      expect(auditLog.oldValues).toEqual({ accountName: '現金' })
      expect(auditLog.newValues).toEqual({ accountName: '現金及び預金' })
    })

    test('仕訳の削除操作を記録できる', async () => {
      const auditLog = AuditLog.create({
        entityType: 'Journal',
        entityId: '1',
        action: AuditAction.DELETE,
        userId: 'user001',
        userName: '山田太郎',
        oldValues: {
          journalDate: '2025-01-15',
          description: '売上計上'
        },
        reason: '入力ミスによる削除',
        ipAddress: '192.168.1.100'
      })

      expect(auditLog.action).toBe(AuditAction.DELETE)
      expect(auditLog.reason).toBe('入力ミスによる削除')
    })

    test('タイムスタンプが自動的に設定される', async () => {
      const beforeCreate = new Date()

      const auditLog = AuditLog.create({
        entityType: 'Journal',
        entityId: '1',
        action: AuditAction.CREATE,
        userId: 'user001',
        userName: '山田太郎',
        changes: {}
      })

      const afterCreate = new Date()

      expect(auditLog.timestamp.getTime()).toBeGreaterThanOrEqual(beforeCreate.getTime())
      expect(auditLog.timestamp.getTime()).toBeLessThanOrEqual(afterCreate.getTime())
    })

    test('監査ログは作成後に変更できない（不変性）', () => {
      const auditLog = AuditLog.create({
        entityType: 'Journal',
        entityId: '1',
        action: AuditAction.CREATE,
        userId: 'user001',
        userName: '山田太郎',
        changes: {}
      })

      // 監査ログのプロパティは読み取り専用
      expect(() => {
        ;(auditLog as any).userId = 'user002'
      }).toThrow()
    })

    test('変更内容のサマリーを取得できる', () => {
      const createLog = AuditLog.create({
        entityType: 'Journal',
        entityId: '1',
        action: AuditAction.CREATE,
        userId: 'user001',
        userName: '山田太郎',
        changes: {}
      })

      expect(createLog.getChangeSummary()).toBe('Journal 1 を作成')

      const updateLog = AuditLog.create({
        entityType: 'Account',
        entityId: '1001',
        action: AuditAction.UPDATE,
        userId: 'user001',
        userName: '山田太郎',
        oldValues: {},
        newValues: {}
      })

      expect(updateLog.getChangeSummary()).toBe('Account 1001 を更新')

      const deleteLog = AuditLog.create({
        entityType: 'Journal',
        entityId: '2',
        action: AuditAction.DELETE,
        userId: 'user001',
        userName: '山田太郎',
        oldValues: {},
        reason: 'テスト'
      })

      expect(deleteLog.getChangeSummary()).toBe('Journal 2 を削除')
    })
  })

  describe('グリーン：監査ログの永続化', () => {
    test('監査ログをデータベースに保存できる', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      const { AuditLogRepository } = await import(
        '../../infrastructure/persistence/audit/AuditLogRepository'
      )
      const repository = new AuditLogRepository(testDb.prisma)

      const auditLog = AuditLog.create({
        entityType: 'Journal',
        entityId: 'J001',
        action: AuditAction.CREATE,
        userId: 'user001',
        userName: '山田太郎',
        changes: {
          journalDate: '2025-01-15',
          description: '売上計上'
        },
        ipAddress: '192.168.1.100'
      })

      const saved = await repository.save(auditLog)

      expect(saved.id).toBeDefined()
      expect(saved.entityType).toBe('Journal')
      expect(saved.entityId).toBe('J001')
      expect(saved.userId).toBe('user001')
    })

    test('エンティティの監査ログを取得できる', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      const { AuditLogRepository } = await import(
        '../../infrastructure/persistence/audit/AuditLogRepository'
      )
      const repository = new AuditLogRepository(testDb.prisma)

      // 複数の監査ログを作成
      await repository.save(
        AuditLog.create({
          entityType: 'Journal',
          entityId: 'J001',
          action: AuditAction.CREATE,
          userId: 'user001',
          userName: '山田太郎',
          changes: {}
        })
      )

      await repository.save(
        AuditLog.create({
          entityType: 'Journal',
          entityId: 'J001',
          action: AuditAction.UPDATE,
          userId: 'user002',
          userName: '佐藤花子',
          oldValues: { description: '売上計上' },
          newValues: { description: '売上計上（修正）' }
        })
      )

      // 別のエンティティ
      await repository.save(
        AuditLog.create({
          entityType: 'Journal',
          entityId: 'J002',
          action: AuditAction.CREATE,
          userId: 'user001',
          userName: '山田太郎',
          changes: {}
        })
      )

      const logs = await repository.findByEntity('Journal', 'J001')

      expect(logs).toHaveLength(2)
      expect(logs[0].action).toBe(AuditAction.UPDATE) // 最新が先
      expect(logs[1].action).toBe(AuditAction.CREATE)
    })

    test('ユーザーの監査ログを取得できる', async () => {
      if (!testDb.prisma) throw new Error('Prisma not initialized')

      const { AuditLogRepository } = await import(
        '../../infrastructure/persistence/audit/AuditLogRepository'
      )
      const repository = new AuditLogRepository(testDb.prisma)

      await repository.save(
        AuditLog.create({
          entityType: 'Journal',
          entityId: 'J001',
          action: AuditAction.CREATE,
          userId: 'user001',
          userName: '山田太郎',
          changes: {}
        })
      )

      await repository.save(
        AuditLog.create({
          entityType: 'Account',
          entityId: 'A001',
          action: AuditAction.UPDATE,
          userId: 'user001',
          userName: '山田太郎',
          oldValues: {},
          newValues: {}
        })
      )

      const logs = await repository.findByUser('user001')

      expect(logs).toHaveLength(2)
    })
  })
})
