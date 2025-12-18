import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { CompletionResultRepository } from './completion-result.repository'
import { WorkOrderRepository, WorkOrderStatus } from './work-order.repository'
import { ProcessMasterRepository } from './process.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('完成実績業務', () => {
  let testDb: TestDatabase
  let completionResultRepository: CompletionResultRepository
  let workOrderRepository: WorkOrderRepository
  let processMasterRepository: ProcessMasterRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    completionResultRepository = new CompletionResultRepository(testDb.prisma!)
    workOrderRepository = new WorkOrderRepository(testDb.prisma!)
    processMasterRepository = new ProcessMasterRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
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

    // 製品
    await itemRepository.create({
      itemCode: 'PRODUCT-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品A',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    // 材料
    await itemRepository.create({
      itemCode: 'MATERIAL-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '材料A',
      itemCategory: 'MATERIAL',
      unitCode: 'PC',
    })

    // 工程マスタ
    await processMasterRepository.create({
      processCode: 'PROC-001',
      processName: 'プレス加工',
    })

    // 欠点マスタ
    await testDb.prisma!.$executeRaw`
      INSERT INTO defect_masters (defect_code, defect_content)
      VALUES ('DEF-001', 'キズ')
    `

    await testDb.prisma!.$executeRaw`
      INSERT INTO defect_masters (defect_code, defect_content)
      VALUES ('DEF-002', 'バリ')
    `

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
  }

  test('完成実績データを登録できる', async () => {
    // Arrange
    await setupTestData()

    // Act
    const result = await completionResultRepository.create({
      completionNumber: 'CR-2025-001',
      workOrderNumber: 'WO-2025-001',
      completionDate: new Date('2025-01-25'),
      itemCode: 'PRODUCT-001',
      completedQuantity: 95,
      defectQuantity: 5,
      reporterCode: 'EMP-001',
      createdBy: 'user01',
    })

    // Assert
    expect(result.id).toBeDefined()
    expect(result.completionNumber).toBe('CR-2025-001')
    expect(result.workOrderNumber).toBe('WO-2025-001')
    expect(Number(result.completedQuantity)).toBe(95)
    expect(Number(result.defectQuantity)).toBe(5)
  })

  test('完成実績番号で完成実績データを検索できる', async () => {
    // Arrange
    await setupTestData()

    await completionResultRepository.create({
      completionNumber: 'CR-2025-001',
      workOrderNumber: 'WO-2025-001',
      completionDate: new Date('2025-01-25'),
      itemCode: 'PRODUCT-001',
      completedQuantity: 95,
    })

    // Act
    const result = await completionResultRepository.findByNumber('CR-2025-001')

    // Assert
    expect(result).toBeDefined()
    expect(result!.completionNumber).toBe('CR-2025-001')
    expect(Number(result!.completedQuantity)).toBe(95)
  })

  test('作業指示番号で完成実績データを検索できる', async () => {
    // Arrange
    await setupTestData()

    // 1回目の完成実績
    await completionResultRepository.create({
      completionNumber: 'CR-2025-001',
      workOrderNumber: 'WO-2025-001',
      completionDate: new Date('2025-01-23'),
      itemCode: 'PRODUCT-001',
      completedQuantity: 50,
    })

    // 2回目の完成実績
    await completionResultRepository.create({
      completionNumber: 'CR-2025-002',
      workOrderNumber: 'WO-2025-001',
      completionDate: new Date('2025-01-25'),
      itemCode: 'PRODUCT-001',
      completedQuantity: 45,
    })

    // Act
    const results = await completionResultRepository.findByWorkOrder('WO-2025-001')

    // Assert
    expect(results).toHaveLength(2)
    expect(results[0].completionNumber).toBe('CR-2025-001')
    expect(results[1].completionNumber).toBe('CR-2025-002')
  })

  test('完成検査結果データを登録できる', async () => {
    // Arrange
    await setupTestData()

    // Act
    const result = await completionResultRepository.create({
      completionNumber: 'CR-2025-001',
      workOrderNumber: 'WO-2025-001',
      completionDate: new Date('2025-01-25'),
      itemCode: 'PRODUCT-001',
      completedQuantity: 95,
      defectQuantity: 5,
      inspectionResults: [
        {
          defectCode: 'DEF-001',
          defectQuantity: 3,
        },
        {
          defectCode: 'DEF-002',
          defectQuantity: 2,
        },
      ],
    })

    // Assert
    expect(result.inspectionResults).toHaveLength(2)
    expect(result.inspectionResults[0].defectCode).toBe('DEF-001')
    expect(Number(result.inspectionResults[0].defectQuantity)).toBe(3)
    expect(result.inspectionResults[1].defectCode).toBe('DEF-002')
    expect(Number(result.inspectionResults[1].defectQuantity)).toBe(2)
  })

  test('完成実績消費データを登録できる', async () => {
    // Arrange
    await setupTestData()

    // Act
    const result = await completionResultRepository.create({
      completionNumber: 'CR-2025-001',
      workOrderNumber: 'WO-2025-001',
      completionDate: new Date('2025-01-25'),
      itemCode: 'PRODUCT-001',
      completedQuantity: 95,
      consumption: {
        consumptionDate: new Date('2025-01-25'),
        details: [
          {
            lineNumber: 1,
            itemCode: 'MATERIAL-001',
            consumptionQuantity: 100, // 95個作るのに100個消費（歩留り95%）
          },
        ],
      },
    })

    // Assert
    expect(result.consumption).toBeDefined()
    expect(result.consumption!.details).toHaveLength(1)
    expect(result.consumption!.details[0].itemCode).toBe('MATERIAL-001')
    expect(Number(result.consumption!.details[0].consumptionQuantity)).toBe(100)
  })

  test('完成実績番号で検査結果と消費データを含めて検索できる', async () => {
    // Arrange
    await setupTestData()

    await completionResultRepository.create({
      completionNumber: 'CR-2025-001',
      workOrderNumber: 'WO-2025-001',
      completionDate: new Date('2025-01-25'),
      itemCode: 'PRODUCT-001',
      completedQuantity: 95,
      defectQuantity: 5,
      inspectionResults: [
        {
          defectCode: 'DEF-001',
          defectQuantity: 5,
        },
      ],
      consumption: {
        consumptionDate: new Date('2025-01-25'),
        details: [
          {
            lineNumber: 1,
            itemCode: 'MATERIAL-001',
            consumptionQuantity: 100,
          },
        ],
      },
    })

    // Act
    const result = await completionResultRepository.findByNumber('CR-2025-001')

    // Assert
    expect(result).toBeDefined()
    expect(result!.completionNumber).toBe('CR-2025-001')
    expect(result!.inspectionResults).toHaveLength(1)
    expect(result!.inspectionResults[0].defectCode).toBe('DEF-001')
    expect(result!.consumption).toBeDefined()
    expect(result!.consumption!.details).toHaveLength(1)
    expect(result!.consumption!.details[0].itemCode).toBe('MATERIAL-001')
  })
})
