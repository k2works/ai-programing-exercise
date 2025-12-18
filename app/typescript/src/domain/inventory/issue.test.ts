import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { IssueRepository } from './issue.repository'
import { InventoryRepository } from './inventory.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'
import { LocationRepository } from '../location/location.repository'
import { WorkOrderRepository, WorkOrderStatus } from '../process/work-order.repository'
import { ProcessMasterRepository } from '../process/process.repository'

describe('払出業務', () => {
  let testDb: TestDatabase
  let issueRepository: IssueRepository
  let inventoryRepository: InventoryRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository
  let locationRepository: LocationRepository
  let workOrderRepository: WorkOrderRepository
  let processMasterRepository: ProcessMasterRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    issueRepository = new IssueRepository(testDb.prisma!)
    inventoryRepository = new InventoryRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
    locationRepository = new LocationRepository(testDb.prisma!)
    workOrderRepository = new WorkOrderRepository(testDb.prisma!)
    processMasterRepository = new ProcessMasterRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  async function setupTestData() {
    // 単位
    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    // 品目
    await itemRepository.create({
      itemCode: 'PRODUCT-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品A',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'MATERIAL-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '材料A',
      itemCategory: 'MATERIAL',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'MATERIAL-002',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '材料B',
      itemCategory: 'MATERIAL',
      unitCode: 'PC',
    })

    // 場所マスタ
    await locationRepository.create({
      locationCode: 'LOC-001',
      locationName: '倉庫A',
      locationType: 'WAREHOUSE',
    })

    await locationRepository.create({
      locationCode: 'LOC-002',
      locationName: '工場B',
      locationType: 'FACTORY',
    })

    // 工程マスタ
    await processMasterRepository.create({
      processCode: 'PROC-001',
      processName: 'プレス加工',
    })

    // 作業指示
    await workOrderRepository.create({
      workOrderNumber: 'WO-2025-001',
      productionOrderNumber: 'PO-2025-001',
      workOrderDate: new Date('2025-01-20'),
      itemCode: 'PRODUCT-001',
      orderQuantity: 100,
      scheduledStartDate: new Date('2025-01-21'),
      scheduledEndDate: new Date('2025-01-25'),
      status: WorkOrderStatus.IN_PROGRESS,
      details: [
        {
          sequence: 1,
          processCode: 'PROC-001',
        },
      ],
    })

    // 初期在庫を設定
    await inventoryRepository.increase({
      locationCode: 'LOC-001',
      itemCode: 'MATERIAL-001',
      quantity: 1000,
      status: 'accepted',
    })

    await inventoryRepository.increase({
      locationCode: 'LOC-001',
      itemCode: 'MATERIAL-002',
      quantity: 500,
      status: 'accepted',
    })
  }

  describe('払出指示', () => {
    test('払出指示データを登録できる', async () => {
      // Arrange
      await setupTestData()

      // Act
      const result = await issueRepository.createInstruction({
        issueInstructionNumber: 'ISI-2025-001',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-21'),
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-001',
            sequence: 1,
            issueQuantity: 100,
          },
          {
            issueLineNumber: 2,
            itemCode: 'MATERIAL-002',
            sequence: 1,
            issueQuantity: 50,
          },
        ],
      })

      // Assert
      expect(result.id).toBeDefined()
      expect(result.issueInstructionNumber).toBe('ISI-2025-001')
      expect(result.workOrderNumber).toBe('WO-2025-001')
      expect(result.sequence).toBe(1)
      expect(result.locationCode).toBe('LOC-001')
      expect(result.details).toHaveLength(2)
      expect(result.details[0].itemCode).toBe('MATERIAL-001')
      expect(Number(result.details[0].issueQuantity)).toBe(100)
      expect(result.details[1].itemCode).toBe('MATERIAL-002')
      expect(Number(result.details[1].issueQuantity)).toBe(50)
    })

    test('払出指示番号で払出指示データを検索できる', async () => {
      // Arrange
      await setupTestData()

      await issueRepository.createInstruction({
        issueInstructionNumber: 'ISI-2025-001',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-21'),
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-001',
            sequence: 1,
            issueQuantity: 100,
          },
        ],
      })

      // Act
      const result = await issueRepository.findInstructionByNumber('ISI-2025-001')

      // Assert
      expect(result).toBeDefined()
      expect(result!.issueInstructionNumber).toBe('ISI-2025-001')
      expect(result!.details).toHaveLength(1)
      expect(Number(result!.details[0].issueQuantity)).toBe(100)
    })

    test('作業指示番号で払出指示データを検索できる', async () => {
      // Arrange
      await setupTestData()

      // 1回目の払出指示
      await issueRepository.createInstruction({
        issueInstructionNumber: 'ISI-2025-001',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-21'),
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-001',
            sequence: 1,
            issueQuantity: 100,
          },
        ],
      })

      // 2回目の払出指示
      await issueRepository.createInstruction({
        issueInstructionNumber: 'ISI-2025-002',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-22'),
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-002',
            sequence: 1,
            issueQuantity: 50,
          },
        ],
      })

      // Act
      const results = await issueRepository.findInstructionsByWorkOrder('WO-2025-001', 1)

      // Assert
      expect(results).toHaveLength(2)
      expect(results[0].issueInstructionNumber).toBe('ISI-2025-001')
      expect(results[1].issueInstructionNumber).toBe('ISI-2025-002')
    })
  })

  describe('払出実行', () => {
    test('払出データを登録できる', async () => {
      // Arrange
      await setupTestData()

      // Act
      const result = await issueRepository.createIssue({
        issueNumber: 'ISS-2025-001',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-21'),
        issuerCode: 'EMP-001',
        createdBy: 'user01',
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-001',
            issueQuantity: 100,
          },
          {
            issueLineNumber: 2,
            itemCode: 'MATERIAL-002',
            issueQuantity: 50,
          },
        ],
      })

      // Assert
      expect(result.id).toBeDefined()
      expect(result.issueNumber).toBe('ISS-2025-001')
      expect(result.workOrderNumber).toBe('WO-2025-001')
      expect(result.sequence).toBe(1)
      expect(result.locationCode).toBe('LOC-001')
      expect(result.issuerCode).toBe('EMP-001')
      expect(result.details).toHaveLength(2)
      expect(result.details[0].itemCode).toBe('MATERIAL-001')
      expect(Number(result.details[0].issueQuantity)).toBe(100)
      expect(result.details[1].itemCode).toBe('MATERIAL-002')
      expect(Number(result.details[1].issueQuantity)).toBe(50)
    })

    test('払出実行時に在庫が減る', async () => {
      // Arrange
      await setupTestData()

      // 払出前の在庫確認
      const beforeInventory1 = await inventoryRepository.findByLocation('LOC-001', 'MATERIAL-001')
      const beforeInventory2 = await inventoryRepository.findByLocation('LOC-001', 'MATERIAL-002')
      expect(Number(beforeInventory1!.stockQuantity)).toBe(1000)
      expect(Number(beforeInventory2!.stockQuantity)).toBe(500)

      // Act
      await issueRepository.createIssue({
        issueNumber: 'ISS-2025-001',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-21'),
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-001',
            issueQuantity: 100,
          },
          {
            issueLineNumber: 2,
            itemCode: 'MATERIAL-002',
            issueQuantity: 50,
          },
        ],
      })

      // Assert
      const afterInventory1 = await inventoryRepository.findByLocation('LOC-001', 'MATERIAL-001')
      const afterInventory2 = await inventoryRepository.findByLocation('LOC-001', 'MATERIAL-002')
      expect(Number(afterInventory1!.stockQuantity)).toBe(900) // 1000 - 100
      expect(Number(afterInventory2!.stockQuantity)).toBe(450) // 500 - 50
    })

    test('払出番号で払出データを検索できる', async () => {
      // Arrange
      await setupTestData()

      await issueRepository.createIssue({
        issueNumber: 'ISS-2025-001',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-21'),
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-001',
            issueQuantity: 100,
          },
        ],
      })

      // Act
      const result = await issueRepository.findByNumber('ISS-2025-001')

      // Assert
      expect(result).toBeDefined()
      expect(result!.issueNumber).toBe('ISS-2025-001')
      expect(result!.details).toHaveLength(1)
      expect(Number(result!.details[0].issueQuantity)).toBe(100)
    })

    test('作業指示番号で払出データを検索できる', async () => {
      // Arrange
      await setupTestData()

      // 1回目の払出
      await issueRepository.createIssue({
        issueNumber: 'ISS-2025-001',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-21'),
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-001',
            issueQuantity: 100,
          },
        ],
      })

      // 2回目の払出
      await issueRepository.createIssue({
        issueNumber: 'ISS-2025-002',
        workOrderNumber: 'WO-2025-001',
        sequence: 1,
        locationCode: 'LOC-001',
        issueDate: new Date('2025-01-22'),
        details: [
          {
            issueLineNumber: 1,
            itemCode: 'MATERIAL-002',
            issueQuantity: 50,
          },
        ],
      })

      // Act
      const results = await issueRepository.findByWorkOrder('WO-2025-001', 1)

      // Assert
      expect(results).toHaveLength(2)
      expect(results[0].issueNumber).toBe('ISS-2025-001')
      expect(results[1].issueNumber).toBe('ISS-2025-002')
    })

    test('在庫が不足している場合はエラーになる', async () => {
      // Arrange
      await setupTestData()

      // Act & Assert
      await expect(
        issueRepository.createIssue({
          issueNumber: 'ISS-2025-001',
          workOrderNumber: 'WO-2025-001',
          sequence: 1,
          locationCode: 'LOC-001',
          issueDate: new Date('2025-01-21'),
          details: [
            {
              issueLineNumber: 1,
              itemCode: 'MATERIAL-001',
              issueQuantity: 2000, // 在庫は1000しかない
            },
          ],
        })
      ).rejects.toThrow('在庫が不足しています')
    })

    test('払出失敗時は在庫が変更されない', async () => {
      // Arrange
      await setupTestData()

      const beforeInventory = await inventoryRepository.findByLocation('LOC-001', 'MATERIAL-001')
      const beforeQuantity = Number(beforeInventory!.stockQuantity)

      // Act & Assert
      await expect(
        issueRepository.createIssue({
          issueNumber: 'ISS-2025-001',
          workOrderNumber: 'WO-2025-001',
          sequence: 1,
          locationCode: 'LOC-001',
          issueDate: new Date('2025-01-21'),
          details: [
            {
              issueLineNumber: 1,
              itemCode: 'MATERIAL-001',
              issueQuantity: 2000,
            },
          ],
        })
      ).rejects.toThrow()

      // 在庫が変更されていないことを確認
      const afterInventory = await inventoryRepository.findByLocation('LOC-001', 'MATERIAL-001')
      expect(Number(afterInventory!.stockQuantity)).toBe(beforeQuantity)
    })
  })
})
