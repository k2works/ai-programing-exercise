import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { ProcessMasterRepository, RoutingRepository } from './process.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('工程管理', () => {
  let testDb: TestDatabase
  let processMasterRepository: ProcessMasterRepository
  let routingRepository: RoutingRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    processMasterRepository = new ProcessMasterRepository(testDb.prisma!)
    routingRepository = new RoutingRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  describe('工程マスタ', () => {
    test('工程マスタを登録できる', async () => {
      // Act
      const process = await processMasterRepository.create({
        processCode: 'PROC-001',
        processName: 'プレス加工',
        standardCycleTime: 5.5,
        setupTime: 30.0,
        createdBy: 'user01',
      })

      // Assert
      expect(process.processCode).toBe('PROC-001')
      expect(process.processName).toBe('プレス加工')
      expect(Number(process.standardCycleTime)).toBe(5.5)
      expect(Number(process.setupTime)).toBe(30.0)
    })

    test('工程コードで工程マスタを検索できる', async () => {
      // Arrange
      await processMasterRepository.create({
        processCode: 'PROC-001',
        processName: 'プレス加工',
      })

      // Act
      const process = await processMasterRepository.findByCode('PROC-001')

      // Assert
      expect(process).toBeDefined()
      expect(process!.processCode).toBe('PROC-001')
      expect(process!.processName).toBe('プレス加工')
    })

    test('全工程マスタを取得できる', async () => {
      // Arrange
      await processMasterRepository.create({
        processCode: 'PROC-001',
        processName: 'プレス加工',
      })

      await processMasterRepository.create({
        processCode: 'PROC-002',
        processName: 'メッキ加工',
      })

      // Act
      const processes = await processMasterRepository.findAll()

      // Assert
      expect(processes).toHaveLength(2)
      expect(processes[0].processCode).toBe('PROC-001')
      expect(processes[1].processCode).toBe('PROC-002')
    })
  })

  describe('工程表', () => {
    beforeEach(async () => {
      // 共通マスタデータを準備
      await unitRepository.create({
        unitCode: 'PC',
        unitSymbol: '個',
        unitName: '個数',
      })

      await itemRepository.create({
        itemCode: 'PART-001',
        effectiveFrom: new Date('2025-01-01'),
        itemName: '加工部品A',
        itemCategory: 'PART',
        unitCode: 'PC',
      })

      await processMasterRepository.create({
        processCode: 'PROC-001',
        processName: 'プレス加工',
      })

      await processMasterRepository.create({
        processCode: 'PROC-002',
        processName: 'メッキ加工',
      })
    })

    test('工程表を登録できる', async () => {
      // Act
      const routing = await routingRepository.create({
        itemCode: 'PART-001',
        effectiveFrom: new Date('2025-01-01'),
        sequence: 1,
        processCode: 'PROC-001',
        workTime: 10.5,
        createdBy: 'user01',
      })

      // Assert
      expect(routing.itemCode).toBe('PART-001')
      expect(routing.sequence).toBe(1)
      expect(routing.processCode).toBe('PROC-001')
      expect(Number(routing.workTime)).toBe(10.5)
    })

    test('品目コードで工程表を検索できる', async () => {
      // Arrange
      await routingRepository.create({
        itemCode: 'PART-001',
        effectiveFrom: new Date('2025-01-01'),
        sequence: 1,
        processCode: 'PROC-001',
        workTime: 10.5,
      })

      await routingRepository.create({
        itemCode: 'PART-001',
        effectiveFrom: new Date('2025-01-01'),
        sequence: 2,
        processCode: 'PROC-002',
        workTime: 15.0,
      })

      // Act
      const routings = await routingRepository.findByItem('PART-001', new Date('2025-01-15'))

      // Assert
      expect(routings).toHaveLength(2)
      expect(routings[0].sequence).toBe(1)
      expect(routings[0].processCode).toBe('PROC-001')
      expect(routings[1].sequence).toBe(2)
      expect(routings[1].processCode).toBe('PROC-002')
    })

    test('適用日で工程表を絞り込める', async () => {
      // Arrange: 古い工程表
      await routingRepository.create({
        itemCode: 'PART-001',
        effectiveFrom: new Date('2025-01-01'),
        sequence: 1,
        processCode: 'PROC-001',
        effectiveTo: new Date('2025-01-31'), // 1月末まで有効
      })

      // 新しい工程表
      await routingRepository.create({
        itemCode: 'PART-001',
        effectiveFrom: new Date('2025-02-01'),
        sequence: 1,
        processCode: 'PROC-002',
      })

      // Act: 1月15日時点の工程表
      const routingsJan = await routingRepository.findByItem('PART-001', new Date('2025-01-15'))

      // Act: 2月15日時点の工程表
      const routingsFeb = await routingRepository.findByItem('PART-001', new Date('2025-02-15'))

      // Assert
      expect(routingsJan).toHaveLength(1)
      expect(routingsJan[0].processCode).toBe('PROC-001')

      expect(routingsFeb).toHaveLength(1)
      expect(routingsFeb[0].processCode).toBe('PROC-002')
    })

    test('品目コードと適用開始日で工程表を検索できる', async () => {
      // Arrange
      await routingRepository.create({
        itemCode: 'PART-001',
        effectiveFrom: new Date('2025-01-01'),
        sequence: 1,
        processCode: 'PROC-001',
      })

      await routingRepository.create({
        itemCode: 'PART-001',
        effectiveFrom: new Date('2025-01-01'),
        sequence: 2,
        processCode: 'PROC-002',
      })

      // Act
      const routings = await routingRepository.findByItemAndDate('PART-001', new Date('2025-01-01'))

      // Assert
      expect(routings).toHaveLength(2)
      expect(routings[0].sequence).toBe(1)
      expect(routings[1].sequence).toBe(2)
    })
  })
})
